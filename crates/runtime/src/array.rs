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

/// Function to allocate an array on the heap.
///
/// # Returns
/// A pointer to the allocated array, or null if allocation failed.
///
/// # Panics
/// Panics if allocation fails.
#[unsafe(no_mangle)]
pub extern "C" fn array_alloc(len: usize, elem_size: usize, elem_is_number: i32) -> *mut c_void {
    // For empty arrays, allocate with minimum capacity to allow push/assignment
    let capacity = if len == 0 { MIN_ARRAY_CAPACITY } else { len };
    let Some(data_bytes) = capacity.checked_mul(elem_size) else {
        if RUNTIME_LOG.load(Ordering::Relaxed) {
            let _ = io::stderr().write_all(
                format!(
                    "[oats runtime] array_alloc: integer overflow (capacity={capacity}, elem_size={elem_size})\n"
                )
                .as_bytes(),
            );
        }
        return ptr::null_mut();
    };
    let Some(total_bytes) = ARRAY_HEADER_SIZE.checked_add(data_bytes) else {
        if RUNTIME_LOG.load(Ordering::Relaxed) {
            let _ = io::stderr().write_all(
                b"[oats runtime] array_alloc: integer overflow in total size calculation\n",
            );
        }
        return ptr::null_mut();
    };

    unsafe {
        let p = crate::runtime_malloc(total_bytes).cast::<u8>();
        if p.is_null() {
            return ptr::null_mut();
        }

        // Write header using unaligned writes since we control the memory layout
        // elem_is_number is used as a boolean (0 or 1), extract only the bit we need
        let elem_is_number_u32 = match u32::try_from(elem_is_number.max(0)) {
            Ok(val) => val,
            Err(e) => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!("[oats runtime] array_alloc: elem_is_number ({elem_is_number}) did not fit in u32: {e}\n").as_bytes()
                    );
                }
                return ptr::null_mut();
            }
        };
        let elem_is_number_flag = u64::from(elem_is_number_u32) & 1;
        let flags = (elem_is_number_flag << 32) & 0xffff_ffff_0000_0000_u64;
        let initial_rc = 0u64;
        ptr::write_unaligned(p.cast::<u64>(), flags | initial_rc);

        // Write length
        ptr::write_unaligned(p.add(mem::size_of::<u64>()).cast::<u64>(), len as u64);

        // Write capacity
        ptr::write_unaligned(
            p.add(mem::size_of::<u64>() * 2).cast::<u64>(),
            capacity as u64,
        );

        p.cast::<c_void>()
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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return 0.0;
        }
        // Validate element offset calculation to prevent overflow
        let Some(elem_offset) = idx.checked_mul(mem::size_of::<f64>()) else {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_get_f64: integer overflow in element offset (idx={idx})\n"
                    )
                    .as_bytes(),
                );
            }
            return 0.0;
        };

        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_addr = data_start as usize + elem_offset;
        let elem_ptr = ptr::with_exposed_provenance::<f64>(elem_addr).cast_mut();
        ptr::read_unaligned(elem_ptr)
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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return ptr::null_mut();
        }
        // Validate element offset calculation to prevent overflow
        let Some(elem_offset) = idx.checked_mul(mem::size_of::<*mut c_void>()) else {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_get_ptr: integer overflow in element offset (idx={idx})\n"
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        };

        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_addr = data_start as usize + elem_offset;
        let elem_ptr = ptr::with_exposed_provenance::<*mut c_void>(elem_addr).cast_mut();
        let p = ptr::read_unaligned(elem_ptr);
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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return ptr::null_mut();
        }
        // Validate element offset calculation to prevent overflow
        let Some(elem_offset) = idx.checked_mul(mem::size_of::<*mut c_void>()) else {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_get_ptr_borrow: integer overflow in element offset (idx={idx})\n"
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        };

        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_addr = data_start as usize + elem_offset;
        let elem_ptr = ptr::with_exposed_provenance::<*mut c_void>(elem_addr).cast_mut();
        ptr::read_unaligned(elem_ptr)
    }
}

/// Read metadata from an array pointer.
/// Returns `(elem_is_number, len, capacity)` tuple.
///
/// # Safety
/// `arr` must be a valid array pointer allocated by this runtime.
unsafe fn read_array_metadata(arr: *mut c_void) -> (i32, usize, usize) {
    let header_u64 = unsafe { ptr::read_unaligned(arr.cast::<u64>()) };
    let elem_is_number = ((header_u64 >> 32) & 1) as i32;

    let len_ptr_u8 = unsafe { arr.cast::<u8>().add(mem::size_of::<u64>()) };
    let len_u64 = unsafe { ptr::read_unaligned(len_ptr_u8 as *const u64) };
    let len = usize::try_from(len_u64).unwrap_or(0);

    let cap_addr = arr as usize + mem::size_of::<u64>() * 2;
    let cap_ptr = ptr::with_exposed_provenance::<u64>(cap_addr).cast_mut();
    let capacity_u64 = unsafe { ptr::read_unaligned(cap_ptr) };
    let capacity = usize::try_from(capacity_u64).unwrap_or(0);

    (elem_is_number, len, capacity)
}

/// Calculate new capacity using geometric growth (1.5x) with bounds checking.
/// Returns None if calculation would overflow or exceed safe limits.
fn calculate_new_capacity(old_capacity: usize, min_capacity: usize) -> Option<usize> {
    const MAX_SAFE_CAPACITY: usize = usize::MAX / 3; // Ensure 1.5x growth doesn't overflow

    if old_capacity > MAX_SAFE_CAPACITY {
        return None;
    }

    if min_capacity > MAX_SAFE_CAPACITY {
        return None;
    }

    // Use checked arithmetic to prevent overflow
    let growth = old_capacity / 2;
    let new_capacity = old_capacity.checked_add(growth.max(1))?;

    if new_capacity < min_capacity {
        Some(min_capacity)
    } else {
        Some(new_capacity)
    }
}

/// Copy array data from old array to new array, handling reference counting.
/// Returns true on success, false on overflow.
///
/// # Safety
/// `old_arr` and `new_arr` must be valid array pointers allocated by this runtime.
/// `len` must not exceed the capacity of either array.
unsafe fn copy_array_data(
    old_arr: *mut c_void,
    new_arr: *mut c_void,
    len: usize,
    elem_size: usize,
    elem_is_number: i32,
) -> bool {
    // Validate copy size to prevent overflow
    let Some(copy_size) = len.checked_mul(elem_size) else {
        return false;
    };

    let old_data = unsafe {
        old_arr
            .cast::<u64>()
            .add(ARRAY_HEADER_SIZE / 8)
            .cast::<u8>()
    };
    let new_data = unsafe {
        new_arr
            .cast::<u64>()
            .add(ARRAY_HEADER_SIZE / 8)
            .cast::<u8>()
    };
    unsafe { ptr::copy_nonoverlapping(old_data, new_data, copy_size) };

    // Handle reference counting for pointer elements
    if elem_is_number == 0 {
        for i in 0..len {
            let ptr_offset = i * mem::size_of::<*mut c_void>();
            let ptr_addr = new_data as usize + ptr_offset;
            let ptr_ptr = ptr::with_exposed_provenance::<*mut c_void>(ptr_addr);
            let p = unsafe { ptr::read_unaligned(ptr_ptr) };
            if !p.is_null() {
                unsafe { rc_inc(p) };
            }
        }
    }

    true
}

/// Grow an array to accommodate at least `min_capacity` elements.
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
        let (elem_is_number, len, old_capacity) = read_array_metadata(arr);

        let Some(new_capacity) = calculate_new_capacity(old_capacity, min_capacity) else {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_grow: capacity {old_capacity} exceeds safe limit or overflow\n"
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        };

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

        let new_len_addr = new_arr as usize + mem::size_of::<u64>();
        let new_len_ptr = ptr::with_exposed_provenance::<u64>(new_len_addr).cast_mut();
        ptr::write_unaligned(new_len_ptr, len as u64);

        // Ensure len doesn't exceed old_capacity (safety check)
        if len > old_capacity {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_grow: length {len} exceeds capacity {old_capacity}\n"
                    )
                    .as_bytes(),
                );
            }
            rc_dec(new_arr);
            return ptr::null_mut();
        }

        if !copy_array_data(arr, new_arr, len, elem_size, elem_is_number) {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_grow: integer overflow in copy size (len={len}, elem_size={elem_size})\n"
                    )
                    .as_bytes(),
                );
            }
            rc_dec(new_arr);
            return ptr::null_mut();
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
    const MAX_ZERO_FILL: usize = 1_000_000; // 1M elements max

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

        let cap_addr = arr as usize + mem::size_of::<u64>() * 2;
        let cap_ptr = ptr::with_exposed_provenance::<u64>(cap_addr).cast_mut();
        let capacity_u64 = ptr::read_unaligned(cap_ptr);
        let capacity = usize::try_from(capacity_u64).unwrap_or(0);

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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);

        // If index is beyond length, zero-fill and extend length
        if idx >= len {
            // Validate index doesn't cause overflow when extending length
            let Some(new_len) = idx.checked_add(1) else {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_f64: index {idx} would cause length overflow\n"
                        )
                        .as_bytes(),
                    );
                }
                return;
            };

            // Validate zeros_count calculation doesn't overflow
            let zeros_count = idx - len;
            // Ensure zeros_count is reasonable (prevent excessive zero-filling)
            if zeros_count > MAX_ZERO_FILL {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_f64: zero-fill count {zeros_count} exceeds limit\n"
                        )
                        .as_bytes(),
                    );
                }
                return;
            }

            let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
            // Validate offset calculation for zeros_start
            let Some(zeros_offset) = len.checked_mul(mem::size_of::<f64>()) else {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        b"[oats runtime] array_set_f64: integer overflow in zeros offset\n",
                    );
                }
                return;
            };
            let zeros_addr = data_start as usize + zeros_offset;
            let zeros_start = ptr::with_exposed_provenance::<f64>(zeros_addr).cast_mut();
            for i in 0..zeros_count {
                ptr::write_unaligned(zeros_start.add(i), 0.0);
            }
            ptr::write_unaligned(len_ptr_u8.cast::<u64>(), new_len as u64);
        }

        // Set the value
        // Validate element offset calculation to prevent overflow
        let Some(elem_offset) = idx.checked_mul(mem::size_of::<f64>()) else {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_set_f64: integer overflow in element offset (idx={idx})\n"
                    )
                    .as_bytes(),
                );
            }
            return;
        };

        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_addr = data_start as usize + elem_offset;
        // elem_offset is multiple of 8 (size_of::<f64>()), so should maintain alignment
        debug_assert!(elem_addr.is_multiple_of(std::mem::align_of::<f64>()));
        let elem_ptr = elem_addr as *mut f64;
        ptr::write_unaligned(elem_ptr, v);
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
    const MAX_NULL_FILL: usize = 1_000_000; // 1M elements max

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

        let cap_addr = arr as usize + mem::size_of::<u64>() * 2;
        let cap_ptr = ptr::with_exposed_provenance::<u64>(cap_addr).cast_mut();
        let capacity_u64 = ptr::read_unaligned(cap_ptr);
        let capacity = usize::try_from(capacity_u64).unwrap_or(0);

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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);

        // If index is beyond length, null-fill and extend length
        if idx >= len {
            // Validate index doesn't cause overflow when extending length
            let Some(new_len) = idx.checked_add(1) else {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_ptr: index {idx} would cause length overflow\n"
                        )
                        .as_bytes(),
                    );
                }
                return;
            };

            // Validate nulls_count calculation doesn't overflow
            let nulls_count = idx - len;
            // Ensure nulls_count is reasonable (prevent excessive null-filling)
            if nulls_count > MAX_NULL_FILL {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] array_set_ptr: null-fill count {nulls_count} exceeds limit\n"
                        )
                        .as_bytes(),
                    );
                }
                return;
            }

            let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
            // Validate offset calculation for nulls_start
            let Some(nulls_offset) = len.checked_mul(mem::size_of::<*mut c_void>()) else {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        b"[oats runtime] array_set_ptr: integer overflow in nulls offset\n",
                    );
                }
                return;
            };
            let nulls_addr = data_start as usize + nulls_offset;
            // nulls_offset is multiple of 8 (size_of::<*mut c_void>()), so should maintain alignment
            debug_assert!(nulls_addr.is_multiple_of(std::mem::align_of::<*mut c_void>()));
            let nulls_start = nulls_addr as *mut *mut c_void;
            for i in 0..nulls_count {
                ptr::write_unaligned(nulls_start.add(i), ptr::null_mut());
            }
            ptr::write_unaligned(len_ptr_u8.cast::<u64>(), new_len as u64);
        }

        // Validate element offset calculation to prevent overflow
        let Some(elem_offset) = idx.checked_mul(mem::size_of::<*mut c_void>()) else {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_set_ptr: integer overflow in element offset (idx={idx})\n"
                    )
                    .as_bytes(),
                );
            }
            return;
        };

        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_addr = data_start as usize + elem_offset;
        // elem_offset is multiple of 8 (size_of::<*mut c_void>()), so should maintain alignment
        debug_assert!(elem_addr.is_multiple_of(std::mem::align_of::<AtomicPtr<c_void>>()));
        let elem_ptr = ptr::with_exposed_provenance::<AtomicPtr<c_void>>(elem_addr).cast_mut();

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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);
        if idx >= len {
            runtime_index_oob_error(arr, idx, len);
            return;
        }
        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_offset = idx * mem::size_of::<*mut c_void>();
        let elem_addr = data_start as usize + elem_offset;
        let elem_ptr = ptr::with_exposed_provenance::<AtomicPtr<c_void>>(elem_addr).cast_mut();

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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);
        let cap_addr = arr as usize + mem::size_of::<u64>() * 2;
        let cap_ptr = ptr::with_exposed_provenance::<u64>(cap_addr).cast_mut();
        let capacity_u64 = ptr::read_unaligned(cap_ptr);
        let capacity = usize::try_from(capacity_u64).unwrap_or(0);

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

        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_addr = data_start as usize + len * mem::size_of::<f64>();
        // len * size_of::<f64>() maintains 8-byte alignment
        debug_assert!(elem_addr.is_multiple_of(std::mem::align_of::<f64>()));
        let elem_ptr = elem_addr as *mut f64;
        ptr::write_unaligned(elem_ptr, value);
        ptr::write_unaligned(len_ptr_u8.cast::<u64>(), (len + 1) as u64);
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
        let len_addr = arr as usize + mem::size_of::<u64>();
        // Adding 8 bytes to 8-byte aligned pointer maintains alignment
        debug_assert!(len_addr.is_multiple_of(std::mem::align_of::<u64>()));
        let len_ptr = ptr::with_exposed_provenance::<u64>(len_addr).cast_mut();
        let len = usize::try_from(*len_ptr).unwrap_or(0);
        if len == 0 {
            return 0.0;
        }
        let new_len = len - 1;
        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_addr = data_start as usize + new_len * mem::size_of::<f64>();
        // new_len * size_of::<f64>() maintains 8-byte alignment
        debug_assert!(elem_addr.is_multiple_of(std::mem::align_of::<f64>()));
        let elem_ptr = elem_addr as *mut f64;
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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);
        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_offset = len * mem::size_of::<*mut c_void>();
        let elem_addr = data_start as usize + elem_offset;
        let elem_ptr = ptr::with_exposed_provenance::<*mut c_void>(elem_addr).cast_mut();
        ptr::write_unaligned(elem_ptr, value);
        if !value.is_null() {
            rc_inc(value);
        }
        ptr::write_unaligned(len_ptr_u8.cast::<u64>(), (len + 1) as u64);
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
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);
        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_offset = len * mem::size_of::<*mut c_void>();
        let elem_addr = data_start as usize + elem_offset;
        let elem_ptr = ptr::with_exposed_provenance::<*mut c_void>(elem_addr).cast_mut();
        ptr::write_unaligned(elem_ptr, value);
        if !value.is_null() {
            rc_weak_inc(value);
        }
        ptr::write_unaligned(len_ptr_u8.cast::<u64>(), (len + 1) as u64);
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
        let len_addr = arr as usize + mem::size_of::<u64>();
        let len_ptr = ptr::with_exposed_provenance::<u64>(len_addr).cast_mut();
        let len = usize::try_from(*len_ptr).unwrap_or(0);
        if len == 0 {
            return ptr::null_mut();
        }
        let new_len = len - 1;
        let data_start = arr.cast::<u64>().add(ARRAY_HEADER_SIZE / 8).cast::<u8>();
        let elem_offset = new_len * mem::size_of::<*mut c_void>();
        let elem_addr = data_start as usize + elem_offset;
        let elem_ptr = ptr::with_exposed_provenance::<*mut c_void>(elem_addr).cast_mut();
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
    use std::fmt::Write;
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // SAFETY: arr is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(arr as usize) {
            return ptr::null_mut();
        }
        let header_u64 = ptr::read_unaligned(arr.cast::<u64>());
        let flags = (header_u64 >> 32) as u32;
        let elem_is_number = (flags & 1) != 0;

        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        let len = usize::try_from(len_u64).unwrap_or(0);

        let mut s = String::new();
        s.push('[');
        if elem_is_number {
            for i in 0..len {
                let v = array_get_f64(arr, i);
                if i != 0 {
                    s.push_str(", ");
                }
                let _ = write!(s, "{v}"); // Should never fail for String
            }
        } else {
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
                let header_ptr = p.cast::<AtomicU64>();
                let header_val = ptr::read_unaligned(header_ptr).load(Ordering::Relaxed);
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
                    use std::fmt::Write;
                    let _ = write!(s, "<ptr {p:p}>"); // Should never fail for String
                }
                rc_dec(p);
            }
        }
        s.push(']');
        let c = std::ffi::CString::new(s).unwrap_or_default();
        heap_str_from_cstr(c.as_ptr())
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
        array_set_ptr(&raw mut arr_ptr, idx, value);
    }
}

#[must_use]
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
