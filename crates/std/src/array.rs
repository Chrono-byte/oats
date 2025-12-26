//! Array prototype methods for JavaScript Array object

use libc::c_void;
use runtime::array::{array_alloc, array_get_ptr, array_set_ptr};
use runtime::rc::{rc_dec, rc_inc};
use std::mem;
use std::ptr;

/// Get array length
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object (base pointer at offset 0).
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_length(arr: *mut c_void) -> usize {
    if arr.is_null() {
        return 0;
    }

    unsafe {
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let len_u64 = ptr::read_unaligned(len_ptr_u8 as *const u64);
        usize::try_from(len_u64).unwrap_or(0)
    }
}

/// Get element at index (returns pointer, caller must manage RC)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_get(arr: *mut c_void, idx: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    // Check if it's a number array or pointer array
    unsafe {
        let header_u64 = ptr::read_unaligned(arr.cast::<u64>());
        let elem_is_number = ((header_u64 >> 32) & 1) != 0;

        if elem_is_number {
            // For number arrays, we'd need to handle f64 values differently
            // For now, return null as number arrays need special handling
            ptr::null_mut()
        } else {
            // Pointer array
            array_get_ptr(arr, idx)
        }
    }
}

/// Set element at index
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
/// `value` must be a valid pointer or null.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_set(arr: *mut c_void, idx: usize, value: *mut c_void) {
    if arr.is_null() {
        return;
    }

    unsafe {
        let header_u64 = ptr::read_unaligned(arr.cast::<u64>());
        let elem_is_number = ((header_u64 >> 32) & 1) != 0;

        if elem_is_number {
            // Number arrays need special handling with f64 values
            // For now, do nothing
        } else {
            // Pointer array
            let mut arr_ptr = arr;
            array_set_ptr(&raw mut arr_ptr, idx, value);
        }
    }
}

/// Push element to end of array (returns new length)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
/// `value` must be a valid pointer or null.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_push(arr: *mut c_void, value: *mut c_void) -> usize {
    if arr.is_null() {
        return 0;
    }

    unsafe {
        let len = oats_std_array_length(arr);
        oats_std_array_set(arr, len, value);
        len + 1
    }
}

/// Pop element from end of array (returns pointer, caller must manage RC)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_pop(arr: *mut c_void) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let len = oats_std_array_length(arr);
        if len == 0 {
            return ptr::null_mut();
        }

        let last_idx = len - 1;
        let value = oats_std_array_get(arr, last_idx);

        // Decrease length (by writing to length field)
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let new_len = (len - 1) as u64;
        ptr::write_unaligned(len_ptr_u8.cast::<u64>(), new_len);

        value
    }
}

/// Shift - remove first element (returns pointer, caller must manage RC)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_shift(arr: *mut c_void) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let len = oats_std_array_length(arr);
        if len == 0 {
            return ptr::null_mut();
        }

        let first = oats_std_array_get(arr, 0);

        // Shift all elements left
        let mut arr_mut = arr;
        for i in 1..len {
            let elem = array_get_ptr(arr_mut, i);
            array_set_ptr(&mut arr_mut, i - 1, elem);
            if !elem.is_null() {
                rc_dec(elem);
            }
        }

        // Decrease length
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let new_len = (len - 1) as u64;
        ptr::write_unaligned(len_ptr_u8.cast::<u64>(), new_len);

        first
    }
}

/// Unshift - add element to beginning (returns new length)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
/// `value` must be a valid pointer or null.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_unshift(arr: *mut c_void, value: *mut c_void) -> usize {
    if arr.is_null() {
        return 0;
    }

    unsafe {
        let len = oats_std_array_length(arr);

        // Shift all elements right
        let mut arr_mut = arr;
        for i in (0..len).rev() {
            let elem = array_get_ptr(arr_mut, i);
            array_set_ptr(&mut arr_mut, i + 1, elem);
            if !elem.is_null() {
                rc_dec(elem);
            }
        }

        // Set first element
        array_set_ptr(&mut arr_mut, 0, value);
        if !value.is_null() {
            rc_inc(value);
        }

        // Increase length
        let len_ptr_u8 = arr.cast::<u8>().add(mem::size_of::<u64>());
        let new_len = (len + 1) as u64;
        ptr::write_unaligned(len_ptr_u8.cast::<u64>(), new_len);

        len + 1
    }
}

/// Reverse array in place (returns array pointer)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_reverse(arr: *mut c_void) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let len = oats_std_array_length(arr);
        let half = len / 2;

        let mut arr_mut = arr;
        for i in 0..half {
            let j = len - 1 - i;
            let left = array_get_ptr(arr_mut, i);
            let right = array_get_ptr(arr_mut, j);

            array_set_ptr(&mut arr_mut, i, right);
            if !right.is_null() {
                rc_dec(right);
            }

            array_set_ptr(&mut arr_mut, j, left);
            if !left.is_null() {
                rc_dec(left);
            }
        }

        arr
    }
}

/// Slice array - extract subarray (returns new array, caller must manage RC)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_slice(
    arr: *mut c_void,
    start: usize,
    end: usize,
) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let len = oats_std_array_length(arr);
        let start = start.min(len);
        let end = end.min(len);
        let start = start.min(end);
        let end = end.max(start);

        let slice_len = end - start;
        if slice_len == 0 {
            // Return empty array
            return array_alloc(0, mem::size_of::<*mut c_void>(), 0);
        }

        // Check if it's a number array
        let header_u64 = ptr::read_unaligned(arr.cast::<u64>());
        let elem_is_number = ((header_u64 >> 32) & 1) != 0;

        if elem_is_number {
            // Number arrays need special handling
            return ptr::null_mut();
        }

        // Create new array
        let new_arr = array_alloc(slice_len, mem::size_of::<*mut c_void>(), 0);
        if new_arr.is_null() {
            return ptr::null_mut();
        }

        // Copy elements
        let mut new_arr_mut = new_arr;
        for i in 0..slice_len {
            let elem = array_get_ptr(arr, start + i);
            array_set_ptr(&mut new_arr_mut, i, elem);
            if !elem.is_null() {
                rc_dec(elem);
            }
        }

        new_arr
    }
}

/// Concatenate arrays (returns new array, caller must manage RC)
///
/// # Safety
/// `arr1` and `arr2` must be valid pointers to Oats array objects.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_concat(
    arr1: *mut c_void,
    arr2: *mut c_void,
) -> *mut c_void {
    if arr1.is_null() && arr2.is_null() {
        return array_alloc(0, mem::size_of::<*mut c_void>(), 0);
    }

    unsafe {
        let len1 = if arr1.is_null() {
            0
        } else {
            oats_std_array_length(arr1)
        };

        let len2 = if arr2.is_null() {
            0
        } else {
            oats_std_array_length(arr2)
        };

        let total_len = len1 + len2;
        let new_arr = array_alloc(total_len, mem::size_of::<*mut c_void>(), 0);
        if new_arr.is_null() {
            return ptr::null_mut();
        }

        // Copy elements from first array
        let mut new_arr_mut = new_arr;
        if !arr1.is_null() {
            for i in 0..len1 {
                let elem = array_get_ptr(arr1, i);
                array_set_ptr(&mut new_arr_mut, i, elem);
                if !elem.is_null() {
                    rc_dec(elem);
                }
            }
        }

        // Copy elements from second array
        if !arr2.is_null() {
            for i in 0..len2 {
                let elem = array_get_ptr(arr2, i);
                array_set_ptr(&mut new_arr_mut, len1 + i, elem);
                if !elem.is_null() {
                    rc_dec(elem);
                }
            }
        }

        new_arr
    }
}

/// Index of element (returns -1 if not found)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
/// `value` must be a valid pointer or null.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_indexOf(arr: *mut c_void, value: *mut c_void) -> i64 {
    if arr.is_null() {
        return -1;
    }

    unsafe {
        let len = oats_std_array_length(arr);
        for i in 0..len {
            let elem = array_get_ptr(arr, i);
            let found = if value.is_null() {
                elem.is_null()
            } else if elem.is_null() {
                false
            } else {
                // Simple pointer comparison (for now)
                // In a full implementation, this would need to compare object values
                elem == value
            };
            if !elem.is_null() {
                rc_dec(elem);
            }
            if found {
                return i as i64;
            }
        }

        -1
    }
}

/// Last index of element (returns -1 if not found)
///
/// # Safety
/// `arr` must be a valid pointer to an Oats array object.
/// `value` must be a valid pointer or null.
#[no_mangle]
pub unsafe extern "C" fn oats_std_array_lastIndexOf(arr: *mut c_void, value: *mut c_void) -> i64 {
    if arr.is_null() {
        return -1;
    }

    unsafe {
        let len = oats_std_array_length(arr);
        for i in (0..len).rev() {
            let elem = array_get_ptr(arr, i);
            let found = if value.is_null() {
                elem.is_null()
            } else if elem.is_null() {
                false
            } else {
                // Simple pointer comparison
                elem == value
            };
            if !elem.is_null() {
                rc_dec(elem);
            }
            if found {
                return i as i64;
            }
        }

        -1
    }
}

// Note: Callback-based methods (map, filter, reduce, forEach, etc.) require
// compiler support to call JavaScript functions from C. These will need to be
// implemented with special compiler integration.
