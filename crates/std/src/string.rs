//! String manipulation operations

use libc::c_char;
use std::ffi::{CStr, CString};

/// Split a string by a delimiter (returns array of strings, caller must free each)
///
/// # Safety
///
/// `str` and `delimiter` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_split(
    str: *const c_char,
    delimiter: *const c_char,
) -> *mut *mut c_char {
    if str.is_null() || delimiter.is_null() {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let delim = match unsafe { CStr::from_ptr(delimiter).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let parts: Vec<String> = str_val.split(delim).map(|s| s.to_string()).collect();
    let count = parts.len();

    if count == 0 {
        return std::ptr::null_mut();
    }

    // Allocate array of pointers
    let ptr_size = std::mem::size_of::<*mut c_char>();
    let array_ptr = unsafe {
        libc::malloc((count + 1) * ptr_size) as *mut *mut c_char
    };

    if array_ptr.is_null() {
        return std::ptr::null_mut();
    }

    // Store count at first element (as a sentinel)
    unsafe {
        *array_ptr = count as *mut c_char;
    }

    // Convert each part to CString and store pointer
    for (i, part) in parts.iter().enumerate() {
        match CString::new(part.as_str()) {
            Ok(cstring) => {
                unsafe {
                    *array_ptr.add(i + 1) = cstring.into_raw();
                }
            }
            Err(_) => {
                // Cleanup on error
                unsafe {
                    for j in 0..i {
                        libc::free(*array_ptr.add(j + 1) as *mut libc::c_void);
                    }
                    libc::free(array_ptr as *mut libc::c_void);
                }
                return std::ptr::null_mut();
            }
        }
    }

    array_ptr
}

/// Get the count of split results
///
/// # Safety
///
/// `split_result` must be a valid pointer returned from `oats_std_string_split`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_split_count(split_result: *mut *mut c_char) -> usize {
    if split_result.is_null() {
        return 0;
    }
    unsafe {
        *split_result as usize
    }
}

/// Get a split result at index (caller must free result)
///
/// # Safety
///
/// `split_result` must be a valid pointer returned from `oats_std_string_split`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_split_get(
    split_result: *mut *mut c_char,
    index: usize,
) -> *mut c_char {
    if split_result.is_null() {
        return std::ptr::null_mut();
    }

    let count = unsafe { *split_result as usize };
    if index >= count {
        return std::ptr::null_mut();
    }

    unsafe {
        *split_result.add(index + 1)
    }
}

/// Free a split result array
///
/// # Safety
///
/// `split_result` must be a valid pointer returned from `oats_std_string_split`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_split_free(split_result: *mut *mut c_char) {
    if split_result.is_null() {
        return;
    }

    let count = unsafe { *split_result as usize };
    unsafe {
        for i in 0..count {
            let ptr = *split_result.add(i + 1);
            if !ptr.is_null() {
                libc::free(ptr as *mut libc::c_void);
            }
        }
        libc::free(split_result as *mut libc::c_void);
    }
}

/// Replace all occurrences of a substring (caller must free result)
///
/// # Safety
///
/// `str`, `from`, and `to` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_replace(
    str: *const c_char,
    from: *const c_char,
    to: *const c_char,
) -> *mut c_char {
    if str.is_null() || from.is_null() || to.is_null() {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let from_str = match unsafe { CStr::from_ptr(from).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let to_str = match unsafe { CStr::from_ptr(to).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let replaced = str_val.replace(from_str, to_str);

    match CString::new(replaced) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Trim whitespace from both ends (caller must free result)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_trim(str: *const c_char) -> *mut c_char {
    if str.is_null() {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let trimmed = str_val.trim();

    match CString::new(trimmed) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get length of string
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_len(str: *const c_char) -> usize {
    if str.is_null() {
        return 0;
    }

    match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s.len(),
        Err(_) => 0,
    }
}

/// Check if string contains substring
///
/// # Safety
///
/// `str` and `substring` must be valid pointers to null-terminated C strings, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_contains(
    str: *const c_char,
    substring: *const c_char,
) -> libc::c_int {
    if str.is_null() || substring.is_null() {
        return 0;
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let sub = match unsafe { CStr::from_ptr(substring).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if str_val.contains(sub) {
        1
    } else {
        0
    }
}

/// Convert string to uppercase (caller must free result)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_to_uppercase(str: *const c_char) -> *mut c_char {
    if str.is_null() {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let upper = str_val.to_uppercase();

    match CString::new(upper) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Convert string to lowercase (caller must free result)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_to_lowercase(str: *const c_char) -> *mut c_char {
    if str.is_null() {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let lower = str_val.to_lowercase();

    match CString::new(lower) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Check if string starts with prefix
///
/// # Safety
///
/// `str` and `prefix` must be valid pointers to null-terminated C strings, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_starts_with(
    str: *const c_char,
    prefix: *const c_char,
) -> libc::c_int {
    if str.is_null() || prefix.is_null() {
        return 0;
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let prefix_str = match unsafe { CStr::from_ptr(prefix).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if str_val.starts_with(prefix_str) {
        1
    } else {
        0
    }
}

/// Check if string ends with suffix
///
/// # Safety
///
/// `str` and `suffix` must be valid pointers to null-terminated C strings, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_ends_with(
    str: *const c_char,
    suffix: *const c_char,
) -> libc::c_int {
    if str.is_null() || suffix.is_null() {
        return 0;
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let suffix_str = match unsafe { CStr::from_ptr(suffix).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if str_val.ends_with(suffix_str) {
        1
    } else {
        0
    }
}

/// Find index of substring (returns -1 if not found)
///
/// # Safety
///
/// `str` and `substring` must be valid pointers to null-terminated C strings, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_find(
    str: *const c_char,
    substring: *const c_char,
) -> i64 {
    if str.is_null() || substring.is_null() {
        return -1;
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let sub = match unsafe { CStr::from_ptr(substring).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    match str_val.find(sub) {
        Some(idx) => idx as i64,
        None => -1,
    }
}

/// Get substring (caller must free result)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_substring(
    str: *const c_char,
    start: usize,
    end: usize,
) -> *mut c_char {
    if str.is_null() {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    if start >= str_val.len() || end > str_val.len() || start > end {
        return std::ptr::null_mut();
    }

    let substring = &str_val[start..end];

    match CString::new(substring) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Repeat string n times (caller must free result)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_repeat(
    str: *const c_char,
    count: usize,
) -> *mut c_char {
    if str.is_null() || count == 0 {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let repeated = str_val.repeat(count);

    match CString::new(repeated) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

