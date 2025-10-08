// Runtime helpers for the OATS AOT runtime.
//
// This file provides a small set of C-callable helpers used by the
// code generator. It intentionally keeps implementations small and
// conservative. The object header layout used here is a u64 where the
// high 32 bits contain type/flags and the low 32 bits contain a
// refcount. All refcount updates are done via atomic CAS on the full
// u64 word.

use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::io::{self, Write};
use std::mem;
use std::process;
use std::ptr;
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};

// --- Memory Management ---

#[unsafe(no_mangle)]
pub extern "C" fn runtime_malloc(size: size_t) -> *mut c_void {
    unsafe { libc::malloc(size) }
}

#[unsafe(no_mangle)]
pub extern "C" fn runtime_free(p: *mut c_void) {
    unsafe { libc::free(p) }
}

// --- String Operations ---

#[unsafe(no_mangle)]
pub extern "C" fn runtime_strlen(s: *const c_char) -> size_t {
    if s.is_null() {
        return 0;
    }
    unsafe { libc::strlen(s) }
}

#[unsafe(no_mangle)]
pub extern "C" fn str_dup(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len = libc::strlen(s) + 1;
        let dst = libc::malloc(len) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        libc::memcpy(dst as *mut c_void, s as *const c_void, len);
        dst
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    if a.is_null() || b.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let la = libc::strlen(a);
        let lb = libc::strlen(b);
        let total = la + lb + 1;
        let dst = libc::malloc(total) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        libc::memcpy(dst as *mut c_void, a as *const c_void, la);
        libc::memcpy(dst.add(la as usize) as *mut c_void, b as *const c_void, lb);
        *dst.add((la + lb) as usize) = 0;
        dst
    }
}

// --- Printing ---

#[unsafe(no_mangle)]
pub extern "C" fn print_f64(v: f64) {
    unsafe {
        libc::printf(b"%f\n\0".as_ptr() as *const c_char, v);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn print_str(s: *const c_char) {
    if s.is_null() {
        return;
    }
    let _ = io::stdout().write_all(unsafe { CStr::from_ptr(s).to_bytes() });
    let _ = io::stdout().write_all(b"\n");
}

#[unsafe(no_mangle)]
pub extern "C" fn print_i32(v: i32) {
    unsafe {
        libc::printf(b"%d\n\0".as_ptr() as *const c_char, v);
    }
}

// --- Array Operations ---
//
// Array Layout: [header: u64][len: u64][data...]
// header: high 32 bits = flags/type, low 32 bits = refcount
//
// The header offset for data is consistently 16 bytes (2 * u64).
const ARRAY_HEADER_SIZE: usize = mem::size_of::<u64>() * 2;

/// Called when an array index is out-of-bounds. Prints a helpful message
/// to stderr and aborts the process to avoid undefined behavior.
fn runtime_index_oob_abort(_arr: *mut c_void, idx: usize, len: usize) -> ! {
    // Try to print a best-effort diagnostic. Avoid panicking inside the
    // runtime; just write to stderr and abort.
    let _ = io::stderr().write_all(b"OATS runtime: array index out of bounds\n");
    let _ = io::stderr().write_all(b"Index: ");
    let s = idx.to_string();
    let _ = io::stderr().write_all(s.as_bytes());
    let _ = io::stderr().write_all(b"\nLength: ");
    let s2 = len.to_string();
    let _ = io::stderr().write_all(s2.as_bytes());
    let _ = io::stderr().write_all(b"\n");
    process::abort();
}

#[unsafe(no_mangle)]
pub extern "C" fn array_alloc(len: usize, elem_size: usize, elem_is_number: i32) -> *mut c_void {
    let data_bytes = len * elem_size;
    let total_bytes = ARRAY_HEADER_SIZE + data_bytes;
    unsafe {
        let p = libc::malloc(total_bytes) as *mut u8;
        if p.is_null() {
            return ptr::null_mut();
        }

        // Initialize header: flags in high 32 bits, refcount (0) in low 32 bits.
        let header_ptr = p as *mut u64;
        let flags = ((elem_is_number as u64) << 32) & 0xffffffff00000000u64;
        *header_ptr = flags;

        // Initialize length.
        let len_ptr = p.add(mem::size_of::<u64>()) as *mut u64;
        *len_ptr = len as u64;

        // The data area is not zeroed by default.
        p as *mut c_void
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_get_f64(arr: *mut c_void, idx: usize) -> f64 {
    if arr.is_null() {
        return 0.0;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<f64>()) as *const f64;
        *elem_ptr
    }
}

/// Loads an element from a pointer-typed array and returns an owned pointer.
///
/// This helper increments the referenced object's refcount before returning
/// so the caller receives an owning reference. The caller is responsible for
/// eventually calling `rc_dec` on the returned pointer.
#[unsafe(no_mangle)]
pub extern "C" fn array_get_ptr(arr: *mut c_void, idx: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<*mut c_void>()) as *const *mut c_void;
        let p = *elem_ptr;
        if !p.is_null() {
            rc_inc(p);
        }
        p
    }
}

/// Returns a borrowed pointer from a pointer-typed array without changing refcounts.
///
/// The caller must not store the returned pointer into any long-lived location
/// without first manually calling `rc_inc`. This is useful for short-lived,
/// read-only access where the pointer's lifetime is guaranteed to be short.
#[unsafe(no_mangle)]
pub extern "C" fn array_get_ptr_borrow(arr: *mut c_void, idx: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<*mut c_void>()) as *const *mut c_void;
        *elem_ptr
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_set_f64(arr: *mut c_void, idx: usize, v: f64) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<f64>()) as *mut f64;
        *elem_ptr = v;
    }
}

/// Sets a pointer in a pointer-typed array, managing refcounts correctly.
///
/// It increments the new pointer's refcount (if not null) and decrements
/// the old pointer's refcount (if not null) that was replaced.
#[unsafe(no_mangle)]
pub extern "C" fn array_set_ptr(arr: *mut c_void, idx: usize, p: *mut c_void) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr =
            data_start.add(idx * mem::size_of::<*mut c_void>()) as *mut AtomicPtr<c_void>;

        // Increment the new pointer's refcount *before* swapping it in.
        if !p.is_null() {
            rc_inc(p);
        }

        // Atomically swap the new pointer into the array, getting the old one back.
        let old = (*elem_ptr).swap(p, Ordering::AcqRel);

        // Decrement the old pointer's refcount *after* it has been removed.
        if !old.is_null() {
            rc_dec(old);
        }
    }
}

// --- Atomic Reference Counting ---

/// Atomically increments the reference count of a heap-allocated object.
#[unsafe(no_mangle)]
pub extern "C" fn rc_inc(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let header = p as *mut AtomicU64;
        // Relaxed ordering is sufficient for an increment, as it only needs to be
        // atomic, not synchronize with other memory operations.
        loop {
            let old_header = (*header).load(Ordering::Relaxed);
            let rc = old_header & 0xffffffff;
            let new_rc = rc.wrapping_add(1) & 0xffffffff;
            let new_header = (old_header & 0xffffffff00000000) | new_rc;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(_) => continue, // Spin on contention
            }
        }
    }
}

/// Atomically decrements the reference count and frees the object if the count reaches zero.
#[unsafe(no_mangle)]
pub extern "C" fn rc_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let header = p as *mut AtomicU64;
        loop {
            // Acquire ordering ensures that we see any writes from other threads
            // that were holding a reference before we decrement.
            let old_header = (*header).load(Ordering::Acquire);
            let rc = old_header & 0xffffffff;

            if rc == 0 {
                // This indicates a double-free or memory corruption.
                // In a real-world scenario, you might abort or log an error.
                return;
            }

            let new_rc = rc.wrapping_sub(1) & 0xffffffff;
            let new_header = (old_header & 0xffffffff00000000) | new_rc;

            // AcqRel ordering:
            // - Acquire: See above.
            // - Release: Ensures that our write to the refcount is visible to any
            //   other thread that might try to acquire a reference.
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    if new_rc == 0 {
                        // An acquire fence ensures that all memory effects that happened
                        // before the last reference was dropped are visible before we free.
                        std::sync::atomic::fence(Ordering::Acquire);

                        // If the high 32 bits encode a type tag that indicates a
                        // per-object destructor pointer is present, call it before
                        // freeing. We use a simple convention for now: a type tag
                        // value of 1 (in the high 32 bits) means the second word
                        // at offset sizeof(u64) holds a function pointer with
                        // signature `extern "C" fn(*mut c_void)`.
                        let type_tag = (old_header >> 32) as u32;
                        if type_tag == 1 {
                            // Read the destructor pointer from the second word.
                            let dtor_ptr_ptr =
                                (p as *mut u8).add(std::mem::size_of::<u64>()) as *mut *mut c_void;
                            let dtor_raw = *dtor_ptr_ptr as *mut c_void;
                            if !dtor_raw.is_null() {
                                // SAFETY: we trust the stored pointer is a valid
                                // `extern "C" fn(*mut c_void)`. The pointer was
                                // stored by the emitter (codegen/tests) when the
                                // object was created.
                                let dtor: extern "C" fn(*mut c_void) =
                                    std::mem::transmute(dtor_raw);
                                dtor(p);
                            }
                        }

                        libc::free(p);
                    }
                    break;
                }
                Err(_) => continue, // Spin on contention
            }
        }
    }
}

// Unit tests for runtime. Placing these here (instead of `tests/`) ensures
// they can call the runtime functions directly even when the crate is built
// as a `staticlib` for linking into AOT outputs.
#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;
    use std::sync::atomic::{AtomicBool, Ordering};

    static CALLED: AtomicBool = AtomicBool::new(false);

    extern "C" fn my_dtor(p: *mut c_void) {
        let _ = p;
        CALLED.store(true, Ordering::SeqCst);
    }

    #[test]
    fn test_rc_dec_calls_destructor() {
        unsafe {
            // allocate two u64 words: header + dtor pointer via runtime_malloc
            let size = std::mem::size_of::<u64>() * 2;
            let mem = runtime_malloc(size) as *mut u8;
            assert!(!mem.is_null());

            // set header: type_tag=1 << 32, refcount=1
            let header_ptr = mem as *mut u64;
            let header_val: u64 = ((1u64) << 32) | 1u64;
            *header_ptr = header_val;

            // store destructor pointer at second word
            let dtor_ptr = mem.add(std::mem::size_of::<u64>()) as *mut *mut c_void;
            *dtor_ptr = my_dtor as *mut c_void;

            // call rc_dec which should call the destructor and free
            rc_dec(mem as *mut c_void);

            // destructor should have been called
            assert!(CALLED.load(Ordering::SeqCst));
        }
    }
}
