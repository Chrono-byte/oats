//! Reference-counting helpers
use libc::c_void;
use std::cell::RefCell;
use std::io::{self, Write};
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};

// Import header helpers from sibling module (parent has already imported header)
use crate::MAX_RECURSION_DEPTH;
use crate::header::*;

// Thread-local recursion depth counter for rc_dec destructor calls
thread_local! {
    static RC_DEC_DEPTH: RefCell<usize> = const { RefCell::new(0) };
}

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

        // Optimized CAS loop: reduce redundant loads by reusing the initial load
        let mut old_header = (*header).load(Ordering::Relaxed);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        // Optimized loop: reuse loaded value, only reload on CAS failure
        loop {
            let rc = old_header & HEADER_RC_MASK;
            // Overflow protection: don't increment beyond safe limit
            if rc >= 0x7FFFFFFF {
                break;
            }
            let new_rc = rc.wrapping_add(1) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_rc;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => {
                    // Reuse the actual value from CAS failure instead of reloading
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
            }
        }
    }
}

/// # Safety
///
/// `p` must be a valid pointer previously returned by the runtime, or null.
/// If non-null, the pointer must remain valid for the duration of this call.
pub unsafe fn rc_inc_pub(p: *mut c_void) {
    unsafe { rc_inc(p) }
}

/// Resolve the base object pointer from a possibly-offset pointer.
///
/// Optimized with better heuristics:
/// - Most pointers are already at the base (common case)
/// - String data pointers are at offset +16
/// - Fast path for aligned pointers that look like valid headers
#[inline]
pub(crate) unsafe fn get_object_base(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // Fast path: check if current pointer looks like a valid header
        // Most pointers are already at the base, so this avoids the subtraction
        let header = p as *const AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        let rc = header_val & HEADER_RC_MASK;
        let is_static = (header_val & HEADER_STATIC_BIT) != 0;

        // Heuristic: if it looks like a valid header (has reasonable RC or is static),
        // it's likely already the base pointer
        if is_static || (rc > 0 && rc < 10000 && (header_val & HEADER_FLAGS_MASK) != 0) {
            return p;
        }

        // Try offset -16 (string data pointer case)
        let obj_ptr = (p as *const u8).sub(16) as *mut c_void;
        let obj_header = obj_ptr as *const AtomicU64;
        let obj_header_val = (*obj_header).load(Ordering::Relaxed);
        let obj_rc = obj_header_val & HEADER_RC_MASK;
        let obj_is_static = (obj_header_val & HEADER_STATIC_BIT) != 0;

        // Heuristic: if the offset header looks valid, return it
        if obj_is_static
            || (obj_rc > 0 && obj_rc < 10000 && (obj_header_val & HEADER_FLAGS_MASK) != 0)
        {
            return obj_ptr;
        }

        // Fallback: return original pointer (assume it's already the base)
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
        // Lazy initialization of logging - only initialize once
        static LOG_INIT: std::sync::Once = std::sync::Once::new();
        LOG_INIT.call_once(|| {
            crate::init_runtime_log();
        });
        let should_log = crate::RUNTIME_LOG.load(Ordering::Relaxed);

        if should_log {
            let _ = io::stdout()
                .write_all(format!("[oats runtime] rc_dec called p={:p}\n", p).as_bytes());
            let _ = io::stdout().flush();
        }

        let p_addr = p as usize;
        if !crate::is_plausible_addr(p_addr) {
            if should_log {
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

        // Optimized CAS loop: reuse loaded values
        let mut old_header = (*header).load(Ordering::Acquire);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        loop {
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

                        // Check recursion depth to prevent stack overflow from destructor chains
                        let depth = RC_DEC_DEPTH.with(|d| {
                            let mut depth = d.borrow_mut();
                            *depth += 1;
                            *depth
                        });

                        // If we exceed the recursion limit, defer the destructor call
                        // to prevent stack overflow. The object will be cleaned up
                        // by the cycle collector. We still need to decrement weak refs
                        // and add to collector candidates for deferred cleanup.
                        if depth > MAX_RECURSION_DEPTH {
                            // Decrement depth counter before returning
                            RC_DEC_DEPTH.with(|d| {
                                *d.borrow_mut() -= 1;
                            });
                            // Still decrement weak references (this is safe even at depth limit)
                            crate::rc_weak_dec(obj_ptr);
                            // Add to cycle collector candidates for deferred cleanup
                            // This ensures the destructor will be called later when depth is lower
                            crate::add_root_candidate(obj_ptr);
                            break;
                        }

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

                        // Decrement depth counter after destructor call
                        RC_DEC_DEPTH.with(|d| {
                            *d.borrow_mut() -= 1;
                        });
                    } else {
                        // Check if this object can form cycles - if not, skip GC
                        if (old_header & HEADER_CYCLE_BIT) == 0 {
                            crate::add_root_candidate(obj_ptr);
                        }
                    }
                    break;
                }
                Err(actual) => {
                    // Reuse the actual value from CAS failure
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
            }
        }
    }
}

/// # Safety
///
/// `p` must be a valid pointer previously returned by the runtime, or null.
/// If non-null, the pointer must remain valid for the duration of this call.
pub unsafe fn rc_dec_pub(p: *mut c_void) {
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
        let mut old_header = (*header).load(Ordering::Relaxed);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        // Optimized loop: reuse loaded values
        loop {
            let weak = header_get_weak_bits(old_header);
            // Overflow protection: don't increment beyond 16-bit limit
            if weak >= 0xffff {
                break;
            }
            let new_weak = (weak.wrapping_add(1)) & 0xffffu64;
            let new_header = header_with_weak(old_header, new_weak);
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => {
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
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
        let mut old_header = (*header).load(Ordering::Acquire);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        // Optimized loop: reuse loaded values
        loop {
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
                Err(actual) => {
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
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
        let mut old_header = (*header).load(Ordering::Acquire);

        // Optimized loop: reuse loaded values
        loop {
            if (old_header & HEADER_STATIC_BIT) != 0 {
                return obj_ptr;
            }
            let strong = old_header & HEADER_RC_MASK;
            if strong == 0 {
                return ptr::null_mut();
            }
            // Overflow protection
            if strong >= 0x7FFFFFFF {
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
                Err(actual) => {
                    old_header = actual;
                }
            }
        }
    }
}
