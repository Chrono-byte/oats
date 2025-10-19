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
/// to stderr and panics the process to avoid undefined behavior.
fn runtime_index_oob_panic(_arr: *mut c_void, idx: usize, len: usize) -> ! {
    // Try to print a best-effort diagnostic. Avoid panicking inside the
    // runtime; just write to stderr and panic.
    let _ = io::stderr().write_all(b"OATS runtime: array index out of bounds\n");
    let _ = io::stderr().write_all(b"Index: ");
    let s = idx.to_string();
    let _ = io::stderr().write_all(s.as_bytes());
    let _ = io::stderr().write_all(b"\nLength: ");
    let s2 = len.to_string();
    let _ = io::stderr().write_all(s2.as_bytes());
    let _ = io::stderr().write_all(b"\n");
    panic!("Array index out of bounds: index={}, length={}", idx, len);
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
            runtime_index_oob_panic(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<f64>()) as *const f64;
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
            runtime_index_oob_panic(arr, idx, len);
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
            runtime_index_oob_panic(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<*mut c_void>()) as *const *mut c_void;
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
        let mut new_capacity = old_capacity + (old_capacity / 2).max(1);
        if new_capacity < min_capacity {
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

        let old_data = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let new_data = (new_arr as *mut u8).add(ARRAY_HEADER_SIZE);
        ptr::copy_nonoverlapping(old_data, new_data, len * elem_size);

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
            let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
            let zeros_start = data_start.add(len * mem::size_of::<f64>()) as *mut f64;
            let zeros_count = idx - len;
            for i in 0..zeros_count {
                *zeros_start.add(i) = 0.0;
            }
            *len_ptr = (idx + 1) as u64;
        }

        // Set the value
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<f64>()) as *mut f64;
        *elem_ptr = v;
    }
}

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
            let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
            let nulls_start =
                data_start.add(len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
            let nulls_count = idx - len;
            for i in 0..nulls_count {
                *nulls_start.add(i) = ptr::null_mut();
            }
            *len_ptr = (idx + 1) as u64;
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
            runtime_index_oob_panic(arr, idx, len);
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
            let _ = io::stderr().write_all(b"OATS runtime: array push capacity exceeded\n");
            let _ = io::stderr().write_all(b"Length: ");
            let s = len.to_string();
            let _ = io::stderr().write_all(s.as_bytes());
            let _ = io::stderr().write_all(b"\nCapacity: ");
            let s2 = capacity.to_string();
            let _ = io::stderr().write_all(s2.as_bytes());
            let _ =
                io::stderr().write_all(b"\nConsider using array[i] = value for dynamic growth\n");
            panic!(
                "Array push capacity exceeded: length={}, capacity={}",
                len, capacity
            );
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

#[unsafe(no_mangle)]
pub extern "C" fn array_push_ptr(arr: *mut c_void, value: *mut c_void) {
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

#[unsafe(no_mangle)]
pub extern "C" fn array_push_ptr_weak(arr: *mut c_void, value: *mut c_void) {
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

#[unsafe(no_mangle)]
pub extern "C" fn array_set(arr: *mut c_void, idx: usize, value: *mut c_void) {
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

pub fn array_set_pub(arr: *mut c_void, idx: usize, value: *mut c_void) {
    array_set(arr, idx, value)
}
