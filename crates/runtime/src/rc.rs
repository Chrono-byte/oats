//! Reference-counting helpers
use libc::c_void;
use std::io::{self, Write};
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};

// Import header helpers from sibling module (parent has already imported header)
use crate::header::*;

/// Atomically increment the strong reference count of a heap-allocated object.
/// # Safety
/// `p` must be a valid pointer returned by the runtime allocator (or a
/// string-data pointer); passing arbitrary or freed pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_inc(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let p_addr = p as usize;
        if !crate::is_plausible_addr(p_addr) {
            return;
        }
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }

        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return;
        }

        loop {
            let old_header = (*header).load(Ordering::Relaxed);
            let rc = old_header & HEADER_RC_MASK;
            let new_rc = rc.wrapping_add(1) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_rc;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(_) => continue,
            }
        }
    }
}

pub fn rc_inc_pub(p: *mut c_void) {
    unsafe { rc_inc(p) }
}

/// Resolve the base object pointer from a possibly-offset pointer.
#[inline]
pub(crate) unsafe fn get_object_base(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let header = p as *const AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        let rc = header_val & HEADER_RC_MASK;
        let is_static = (header_val & HEADER_STATIC_BIT) != 0;

        if is_static || (rc > 0 && rc < 10000 && header_val > 256) {
            return p;
        }

        let obj_ptr = (p as *const u8).sub(16) as *mut c_void;
        let obj_header = obj_ptr as *const AtomicU64;
        let obj_header_val = (*obj_header).load(Ordering::Relaxed);
        let obj_rc = obj_header_val & HEADER_RC_MASK;
        let obj_is_static = (obj_header_val & HEADER_STATIC_BIT) != 0;

        if obj_is_static || (obj_rc > 0 && obj_rc < 10000 && obj_header_val > 256) {
            return obj_ptr;
        }

        p
    }
}

/// Atomically decrement the strong reference count and free the object if the count reaches zero.
/// # Safety
/// `p` must be a valid pointer returned by the runtime allocator (or a
/// string-data pointer). Double-free or invalid pointers are undefined behaviour.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        crate::init_runtime_log();
        if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
            let _ = io::stdout()
                .write_all(format!("[oats runtime] rc_dec called p={:p}\n", p).as_bytes());
            let _ = io::stdout().flush();
        }
        let p_addr = p as usize;
        if !crate::is_plausible_addr(p_addr) {
            if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stdout().write_all(
                    format!("[oats runtime] rc_dec: implausible p={:p}, ignoring\n", p).as_bytes(),
                );
                let _ = io::stdout().flush();
            }
            return;
        }
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }

        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return;
        }

        loop {
            let old_header = (*header).load(Ordering::Acquire);
            let rc = old_header & HEADER_RC_MASK;

            if rc == 0 {
                return;
            }

            let new_rc = rc.wrapping_sub(1) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_rc;

            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    if new_rc == 0 {
                        std::sync::atomic::fence(Ordering::Acquire);

                        let type_tag = (old_header >> HEADER_TYPE_TAG_SHIFT) as u32;
                        if type_tag == 1 {
                            let dtor_ptr_ptr = (obj_ptr as *mut u8).add(std::mem::size_of::<u64>())
                                as *mut *mut c_void;
                            let dtor_raw = *dtor_ptr_ptr;
                            if !dtor_raw.is_null() {
                                let dtor: extern "C" fn(*mut c_void) =
                                    std::mem::transmute(dtor_raw);
                                dtor(obj_ptr);
                            }
                        }

                        crate::rc_weak_dec(obj_ptr);
                    } else {
                        // Check if this object can form cycles - if not, skip GC
                        if (old_header & HEADER_CYCLE_BIT) == 0 {
                            crate::add_root_candidate(obj_ptr);
                        }
                    }
                    break;
                }
                Err(_) => continue,
            }
        }
    }
}

pub fn rc_dec_pub(p: *mut c_void) {
    unsafe { rc_dec(p) }
}

/// Atomically increment the weak reference count on an object.
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_weak_inc(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }
        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return;
        }

        loop {
            let old_header = (*header).load(Ordering::Relaxed);
            let weak = header_get_weak_bits(old_header);
            let new_weak = (weak.wrapping_add(1)) & 0xffffu64;
            let new_header = header_with_weak(old_header, new_weak);
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(_) => continue,
            }
        }
    }
}

/// Atomically decrement the weak reference count and free the control block
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_weak_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }
        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return;
        }

        loop {
            let old_header = (*header).load(Ordering::Acquire);
            let weak = header_get_weak_bits(old_header);
            if weak == 0 {
                return;
            }
            let new_weak = (weak - 1) & 0xffffu64;
            let new_header = header_with_weak(old_header, new_weak);

            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    let strong = new_header & HEADER_RC_MASK;
                    let weak_after = header_get_weak_bits(new_header);
                    if strong == 0 && weak_after == 0 {
                        std::sync::atomic::fence(Ordering::Acquire);
                        let header_atomic = obj_ptr as *mut AtomicU64;
                        let _ = (*header_atomic).fetch_and(!HEADER_CLAIM_BIT, Ordering::AcqRel);
                        crate::runtime_free(obj_ptr);
                    }
                    break;
                }
                Err(_) => continue,
            }
        }
    }
}

/// Attempt to upgrade a weak pointer into a strong one.
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_weak_upgrade(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return ptr::null_mut();
        }

        let header = obj_ptr as *mut AtomicU64;
        loop {
            let old_header = (*header).load(Ordering::Acquire);
            if (old_header & HEADER_STATIC_BIT) != 0 {
                return obj_ptr;
            }
            let strong = old_header & HEADER_RC_MASK;
            if strong == 0 {
                return ptr::null_mut();
            }
            let new_strong = (strong.wrapping_add(1)) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_strong;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => return obj_ptr,
                Err(_) => continue,
            }
        }
    }
}
