//! Array helpers for runtime

use libc::{c_char, c_void};
use std::ffi::CStr;
use std::io::{self, Write};
use std::mem;
use std::ptr;
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};

use crate::header::HEADER_TYPE_TAG_SHIFT;
use crate::rc::{rc_dec, rc_inc, rc_weak_dec, rc_weak_inc};
use crate::string::{heap_str_from_cstr, rc_dec_str};
use crate::{ARRAY_HEADER_SIZE, MIN_ARRAY_CAPACITY, RUNTIME_LOG};

/// Called when an array index is out-of-bounds. Prints a helpful message
/// to stderr and returns gracefully to avoid undefined behavior.
/// This function does not return (-> !) but is called from contexts that
/// can handle the early return.
fn runtime_index_oob_error(_arr: *mut c_void, idx: usize, len: usize) {
    // Print diagnostic message to stderr
    if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
        let _ = io::stderr().write_all(b"OATS runtime: array index out of bounds\n");
        let _ = io::stderr().write_all(b"Index: ");
        let s = idx.to_string();
        let _ = io::stderr().write_all(s.as_bytes());
        let _ = io::stderr().write_all(b"\nLength: ");
        let s2 = len.to_string();
        let _ = io::stderr().write_all(s2.as_bytes());
        let _ = io::stderr().write_all(b"\n");
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_alloc(len: usize, elem_size: usize, elem_is_number: i32) -> *mut c_void {
    // For empty arrays, allocate with minimum capacity to allow push/assignment
    let capacity = if len == 0 { MIN_ARRAY_CAPACITY } else { len };
    let data_bytes = match capacity.checked_mul(elem_size) {
        Some(bytes) => bytes,
        None => {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_alloc: integer overflow (capacity={}, elem_size={})\n",
                        capacity, elem_size
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        }
    };
    let total_bytes = match ARRAY_HEADER_SIZE.checked_add(data_bytes) {
        Some(total) => total,
        None => {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    b"[oats runtime] array_alloc: integer overflow in total size calculation\n",
                );
            }
            return ptr::null_mut();
        }
    };

    unsafe {
        let p = crate::runtime_malloc(total_bytes) as *mut u8;
        if p.is_null() {
            return ptr::null_mut();
        }

        let header_ptr = p as *mut u64;
        let flags = ((elem_is_number as u64) << 32) & 0xffffffff00000000u64;
        let initial_rc = 1u64;
        *header_ptr = flags | initial_rc;

        let len_ptr = p.add(mem::size_of::<u64>()) as *mut u64;
        *len_ptr = len as u64;

        let cap_ptr = p.add(mem::size_of::<u64>() * 2) as *mut u64;
        *cap_ptr = capacity as u64;

        p as *mut c_void
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_get_f64(arr: *mut c_void, idx: usize) -> f64 {
    if arr.is_null() {
        return 0.0;
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return 0.0;
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return 0.0;
        }
        // Validate element offset calculation to prevent overflow
        let elem_offset = match idx.checked_mul(mem::size_of::<f64>()) {
            Some(offset) => offset,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_get_f64: integer overflow in element offset (idx={})\n",
                            idx
                        )
                        .as_bytes(),
                    );
                }
                return 0.0;
            }
        };

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(elem_offset) as *const f64;
        *elem_ptr
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_get_ptr(arr: *mut c_void, idx: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return ptr::null_mut();
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return ptr::null_mut();
        }
        // Validate element offset calculation to prevent overflow
        let elem_offset = match idx.checked_mul(mem::size_of::<*mut c_void>()) {
            Some(offset) => offset,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_get_ptr: integer overflow in element offset (idx={})\n",
                            idx
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(elem_offset) as *const *mut c_void;
        let p = *elem_ptr;
        if !p.is_null() {
            rc_inc(p);
        }
        p
    }
}

/// Return a borrowed pointer from a pointer-typed array without changing refcounts.
///
/// The caller must not store the returned pointer into any long-lived location
/// without first manually calling `rc_inc`. This is useful for short-lived,
/// read-only access where the pointer's lifetime is guaranteed to be short.
///
/// # Safety
/// Caller must ensure `arr` is valid and the borrowed pointer is not stored into
/// long-lived locations without calling `rc_inc` first.
#[unsafe(no_mangle)]
pub extern "C" fn array_get_ptr_borrow(arr: *mut c_void, idx: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return ptr::null_mut();
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return ptr::null_mut();
        }
        // Validate element offset calculation to prevent overflow
        let elem_offset = match idx.checked_mul(mem::size_of::<*mut c_void>()) {
            Some(offset) => offset,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_get_ptr_borrow: integer overflow in element offset (idx={})\n",
                            idx
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(elem_offset) as *const *mut c_void;
        *elem_ptr
    }
}

/// Grow an array to accommodate at least min_capacity elements.
/// Returns a new array pointer with the same refcount and data copied over.
/// The old array is NOT freed - caller is responsible for managing refcounts.
///
/// Uses geometric growth (1.5x) to amortize reallocation costs.
///
/// # Safety
/// Caller must ensure arr is a valid array pointer and manage refcounts appropriately.
unsafe fn array_grow(arr: *mut c_void, min_capacity: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        // Read current metadata
        let header_ptr = arr as *const u64;
        let header = *header_ptr;
        let elem_is_number = ((header >> 32) & 1) as i32;

        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;

        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *const u64;
        let old_capacity = *cap_ptr as usize;

        // Calculate new capacity with geometric growth (1.5x)
        // Validate old_capacity to prevent overflow in growth calculation
        const MAX_SAFE_CAPACITY: usize = usize::MAX / 3; // Ensure 1.5x growth doesn't overflow
        if old_capacity > MAX_SAFE_CAPACITY {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_grow: capacity {} exceeds safe limit\n",
                        old_capacity
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        }

        // Use checked arithmetic to prevent overflow
        let growth = old_capacity / 2;
        let mut new_capacity = match old_capacity.checked_add(growth.max(1)) {
            Some(cap) => cap,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        b"[oats runtime] array_grow: integer overflow in capacity calculation\n",
                    );
                }
                return ptr::null_mut();
            }
        };
        if new_capacity < min_capacity {
            // Validate min_capacity doesn't exceed maximum safe value
            if min_capacity > MAX_SAFE_CAPACITY {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_grow: min_capacity {} exceeds safe limit\n",
                            min_capacity
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
            new_capacity = min_capacity;
        }

        // Determine element size
        let elem_size = if elem_is_number != 0 {
            mem::size_of::<f64>()
        } else {
            mem::size_of::<*mut c_void>()
        };

        let new_arr = array_alloc(new_capacity, elem_size, elem_is_number);
        if new_arr.is_null() {
            return ptr::null_mut();
        }

        let new_len_ptr = (new_arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        *new_len_ptr = len as u64;

        // Validate copy size to prevent overflow
        let copy_size = match len.checked_mul(elem_size) {
            Some(size) => size,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_grow: integer overflow in copy size (len={}, elem_size={})\n",
                            len, elem_size
                        )
                        .as_bytes(),
                    );
                }
                // Clean up allocated array before returning
                rc_dec(new_arr);
                return ptr::null_mut();
            }
        };

        // Ensure len doesn't exceed old_capacity (safety check)
        if len > old_capacity {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_grow: length {} exceeds capacity {}\n",
                        len, old_capacity
                    )
                    .as_bytes(),
                );
            }
            rc_dec(new_arr);
            return ptr::null_mut();
        }

        let old_data = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let new_data = (new_arr as *mut u8).add(ARRAY_HEADER_SIZE);
        ptr::copy_nonoverlapping(old_data, new_data, copy_size);

        if elem_is_number == 0 {
            let ptrs = new_data as *mut *mut c_void;
            for i in 0..len {
                let p = *ptrs.add(i);
                if !p.is_null() {
                    rc_inc(p);
                }
            }
        }

        new_arr
    }
}

/// # Safety
///
/// `arr_ptr` must be a valid pointer to a pointer to an array object, or null.
/// If non-null, the array must be a valid array object allocated by this runtime.
/// The function performs null checks and bounds checking internally.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_f64(arr_ptr: *mut *mut c_void, idx: usize, v: f64) {
    if arr_ptr.is_null() {
        return;
    }
    unsafe {
        let mut arr = *arr_ptr;
        if arr.is_null() {
            return;
        }
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return;
        }

        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *mut u64;
        let capacity = *cap_ptr as usize;

        // If index is beyond capacity, grow the array
        if idx >= capacity {
            let new_arr = array_grow(arr, idx + 1);
            if new_arr.is_null() {
                return; // allocation failed
            }
            // Decrement old array refcount and update pointer
            rc_dec(arr);
            arr = new_arr;
            *arr_ptr = new_arr;
        }

        // Re-get pointers in case arr changed
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;

        // If index is beyond length, zero-fill and extend length
        if idx >= len {
            // Validate index doesn't cause overflow when extending length
            let new_len = match idx.checked_add(1) {
                Some(nl) => nl,
                None => {
                    if RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            format!(
                                "[oats runtime] array_set_f64: index {} would cause length overflow\n",
                                idx
                            )
                            .as_bytes(),
                        );
                    }
                    return;
                }
            };

            // Validate zeros_count calculation doesn't overflow
            let zeros_count = idx - len;
            // Ensure zeros_count is reasonable (prevent excessive zero-filling)
            const MAX_ZERO_FILL: usize = 1_000_000; // 1M elements max
            if zeros_count > MAX_ZERO_FILL {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_f64: zero-fill count {} exceeds limit\n",
                            zeros_count
                        )
                        .as_bytes(),
                    );
                }
                return;
            }

            let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
            // Validate offset calculation for zeros_start
            let zeros_offset = match len.checked_mul(mem::size_of::<f64>()) {
                Some(offset) => offset,
                None => {
                    if RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            b"[oats runtime] array_set_f64: integer overflow in zeros offset\n",
                        );
                    }
                    return;
                }
            };
            let zeros_start = data_start.add(zeros_offset) as *mut f64;
            for i in 0..zeros_count {
                *zeros_start.add(i) = 0.0;
            }
            *len_ptr = new_len as u64;
        }

        // Set the value
        // Validate element offset calculation to prevent overflow
        let elem_offset = match idx.checked_mul(mem::size_of::<f64>()) {
            Some(offset) => offset,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_f64: integer overflow in element offset (idx={})\n",
                            idx
                        )
                        .as_bytes(),
                    );
                }
                return;
            }
        };

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(elem_offset) as *mut f64;
        *elem_ptr = v;
    }
}

/// # Safety
///
/// `arr_ptr` must be a valid pointer to a pointer to an array object, or null.
/// If non-null, the array must be a valid array object allocated by this runtime.
/// `p` must be a valid pointer to an object managed by this runtime's reference counting,
/// or null. The function performs null checks and bounds checking internally.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_ptr(arr_ptr: *mut *mut c_void, idx: usize, p: *mut c_void) {
    if arr_ptr.is_null() {
        return;
    }
    unsafe {
        let mut arr = *arr_ptr;
        if arr.is_null() {
            return;
        }
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return;
        }

        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *mut u64;
        let capacity = *cap_ptr as usize;

        // If index is beyond capacity, grow the array
        if idx >= capacity {
            let new_arr = array_grow(arr, idx + 1);
            if new_arr.is_null() {
                return; // allocation failed
            }
            // Decrement old array refcount and update pointer
            rc_dec(arr);
            arr = new_arr;
            *arr_ptr = new_arr;
        }

        // Re-get pointers in case arr changed
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;

        // If index is beyond length, null-fill and extend length
        if idx >= len {
            // Validate index doesn't cause overflow when extending length
            let new_len = match idx.checked_add(1) {
                Some(nl) => nl,
                None => {
                    if RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            format!(
                                "[oats runtime] array_set_ptr: index {} would cause length overflow\n",
                                idx
                            )
                            .as_bytes(),
                        );
                    }
                    return;
                }
            };

            // Validate nulls_count calculation doesn't overflow
            let nulls_count = idx - len;
            // Ensure nulls_count is reasonable (prevent excessive null-filling)
            const MAX_NULL_FILL: usize = 1_000_000; // 1M elements max
            if nulls_count > MAX_NULL_FILL {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_ptr: null-fill count {} exceeds limit\n",
                            nulls_count
                        )
                        .as_bytes(),
                    );
                }
                return;
            }

            let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
            // Validate offset calculation for nulls_start
            let nulls_offset = match len.checked_mul(mem::size_of::<*mut c_void>()) {
                Some(offset) => offset,
                None => {
                    if RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            b"[oats runtime] array_set_ptr: integer overflow in nulls offset\n",
                        );
                    }
                    return;
                }
            };
            let nulls_start = data_start.add(nulls_offset) as *mut *mut c_void;
            for i in 0..nulls_count {
                *nulls_start.add(i) = ptr::null_mut();
            }
            *len_ptr = new_len as u64;
        }

        // Validate element offset calculation to prevent overflow
        let elem_offset = match idx.checked_mul(mem::size_of::<*mut c_void>()) {
            Some(offset) => offset,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_ptr: integer overflow in element offset (idx={})\n",
                            idx
                        )
                        .as_bytes(),
                    );
                }
                return;
            }
        };

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(elem_offset) as *mut AtomicPtr<c_void>;

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

/// # Safety
///
/// `arr` must be a valid pointer to an array object allocated by this runtime, or null.
/// `p` must be a valid pointer to an object managed by this runtime's reference counting,
/// or null. The function performs null checks and bounds checking internally.
/// This function uses weak references, so it does not increment the strong reference count.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_ptr_weak(arr: *mut c_void, idx: usize, p: *mut c_void) {
    if arr.is_null() {
        return;
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return;
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return;
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr =
            data_start.add(idx * mem::size_of::<*mut c_void>()) as *mut AtomicPtr<c_void>;

        // Increment the new pointer's WEAK refcount *before* swapping it in.
        if !p.is_null() {
            rc_weak_inc(p);
        }

        // Atomically swap the new pointer into the array, getting the old one back.
        let old = (*elem_ptr).swap(p, Ordering::AcqRel);

        // Decrement the old pointer's WEAK refcount *after* it has been removed.
        if !old.is_null() {
            rc_weak_dec(old);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_push_f64(arr: *mut c_void, value: f64) {
    if arr.is_null() {
        return;
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return;
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;
        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *const u64;
        let capacity = *cap_ptr as usize;

        if len >= capacity {
            if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(b"OATS runtime: array push capacity exceeded\n");
                let _ = io::stderr().write_all(b"Length: ");
                let s = len.to_string();
                let _ = io::stderr().write_all(s.as_bytes());
                let _ = io::stderr().write_all(b"\nCapacity: ");
                let s2 = capacity.to_string();
                let _ = io::stderr().write_all(s2.as_bytes());
                let _ = io::stderr()
                    .write_all(b"\nConsider using array[i] = value for dynamic growth\n");
            }
            // Return early instead of panicking - caller should handle capacity growth
            return;
        }

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(len * mem::size_of::<f64>()) as *mut f64;
        *elem_ptr = value;
        *len_ptr = (len + 1) as u64;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_pop_f64(arr: *mut c_void) -> f64 {
    if arr.is_null() {
        return 0.0;
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return 0.0;
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as isize;
        if len <= 0 {
            return 0.0;
        }
        let new_len = (len - 1) as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(new_len * mem::size_of::<f64>()) as *mut f64;
        let val = *elem_ptr;
        // Optionally zero the slot
        *elem_ptr = 0.0;
        *len_ptr = new_len as u64;
        val
    }
}

/// # Safety
///
/// `arr` must be a valid pointer to an array object allocated by this runtime, or null.
/// `value` must be a valid pointer to an object managed by this runtime's reference counting,
/// or null. The function performs null checks and bounds checking internally.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_push_ptr(arr: *mut c_void, value: *mut c_void) {
    if arr.is_null() {
        return;
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return;
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
        *elem_ptr = value;
        if !value.is_null() {
            rc_inc(value);
        }
        *len_ptr = (len + 1) as u64;
    }
}

/// # Safety
///
/// `arr` must be a valid pointer to an array object allocated by this runtime, or null.
/// `value` must be a valid pointer to an object managed by this runtime's reference counting,
/// or null. The function performs null checks and bounds checking internally.
/// This function uses weak references, so it does not increment the strong reference count.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_push_ptr_weak(arr: *mut c_void, value: *mut c_void) {
    if arr.is_null() {
        return;
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return;
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
        *elem_ptr = value;
        if !value.is_null() {
            rc_weak_inc(value);
        }
        *len_ptr = (len + 1) as u64;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_pop_ptr(arr: *mut c_void) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return ptr::null_mut();
        }
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as isize;
        if len <= 0 {
            return ptr::null_mut();
        }
        let new_len = (len - 1) as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(new_len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
        let raw = *elem_ptr;
        // Clear the slot to avoid dangling references in the array buffer.
        *elem_ptr = ptr::null_mut();
        *len_ptr = new_len as u64;
        raw
    }
}

/// Convert an array to its string representation.
///
/// This function creates a heap-allocated string containing a JSON-like
/// representation of the array. For numeric arrays, it formats as `[1, 2, 3]`.
/// For pointer arrays, it recursively stringifies nested arrays and formats
/// pointers as strings or `<ptr 0x...>`.
///
/// # Safety
/// `arr` must be a valid array pointer (not null, properly allocated).
/// Passing arbitrary pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_to_string(arr: *mut c_void) -> *mut c_char {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return ptr::null_mut();
        }
        let header_ptr = arr as *const u64;
        let header = *header_ptr;
        let flags = (header >> 32) as u32;
        let elem_is_number = (flags & 1) != 0;

        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;

        if elem_is_number {
            let mut s = String::new();
            s.push('[');
            for i in 0..len {
                let v = array_get_f64(arr, i);
                if i != 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", v));
            }
            s.push(']');
            let c = std::ffi::CString::new(s).unwrap_or_default();
            heap_str_from_cstr(c.as_ptr())
        } else {
            let mut s = String::new();
            s.push('[');
            for i in 0..len {
                if i != 0 {
                    s.push_str(", ");
                }
                let p = array_get_ptr(arr, i);
                if p.is_null() {
                    s.push_str("null");
                    continue;
                }
                let mut printed = false;
                let header_ptr = p as *const AtomicU64;
                let header_val = (*header_ptr).load(Ordering::Relaxed);
                let type_tag = (header_val >> HEADER_TYPE_TAG_SHIFT) as u64;
                if type_tag == 0 {
                    let nested = array_to_string(p);
                    if !nested.is_null() {
                        let cstr = CStr::from_ptr(nested);
                        if let Ok(str_slice) = cstr.to_str() {
                            s.push_str(str_slice);
                            printed = true;
                        }
                        rc_dec_str(nested);
                    }
                }
                if !printed {
                    let maybe = CStr::from_ptr(p as *const c_char);
                    if let Ok(st) = maybe.to_str() {
                        s.push('"');
                        s.push_str(st);
                        s.push('"');
                        printed = true;
                    }
                }
                if !printed {
                    s.push_str(&format!("<ptr {:p}>", p));
                }
                rc_dec(p);
            }
            s.push(']');
            let c = std::ffi::CString::new(s).unwrap_or_default();
            heap_str_from_cstr(c.as_ptr())
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_get(arr: *mut c_void, idx: usize) -> *mut c_void {
    array_get_ptr(arr, idx)
}

/// # Safety
///
/// `arr` must be a valid pointer to an array object allocated by this runtime, or null.
/// `value` must be a valid pointer to an object managed by this runtime's reference counting,
/// or null. The function performs null checks and bounds checking internally.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set(arr: *mut c_void, idx: usize, value: *mut c_void) {
    let mut arr_ptr = arr;
    unsafe {
        array_set_ptr(&mut arr_ptr, idx, value);
    }
}

pub fn array_alloc_pub(element_size: usize, length: usize) -> *mut c_void {
    array_alloc(length, element_size, 0)
}

pub fn array_get_pub(arr: *mut c_void, idx: usize) -> *mut c_void {
    array_get(arr, idx)
}

/// # Safety
///
/// `arr` must be a valid pointer to an array object allocated by this runtime, or null.
/// `value` must be a valid pointer to an object managed by this runtime's reference counting,
/// or null. The function performs null checks and bounds checking internally.
pub unsafe fn array_set_pub(arr: *mut c_void, idx: usize, value: *mut c_void) {
    unsafe { array_set(arr, idx, value) }
}
