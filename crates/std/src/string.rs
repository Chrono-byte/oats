//! String manipulation operations

use libc::c_char;
use std::ffi::{CStr, CString};

/// Get length of string
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub const unsafe extern "C" fn oats_std_string_len(str: *const c_char) -> usize {
    if str.is_null() {
        return 0;
    }

    match CStr::from_ptr(str).to_str() {
        Ok(s) => s.len(),
        Err(_) => 0,
    }
}

/// Check if string contains substring
///
/// # Safety
///
/// `str` and `substring` must be valid pointers to null-terminated C strings, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_contains(
    str: *const c_char,
    substring: *const c_char,
) -> libc::c_int {
    if str.is_null() || substring.is_null() {
        return 0;
    }

    let Ok(str_val) = CStr::from_ptr(str).to_str() else {
        return 0;
    };

    let Ok(sub) = CStr::from_ptr(substring).to_str() else {
        return 0;
    };

    i32::from(str_val.contains(sub))
}

/// Convert string to uppercase (caller must free result)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_to_uppercase(str: *const c_char) -> *mut c_char {
    if str.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = CStr::from_ptr(str).to_str() else {
        return std::ptr::null_mut();
    };

    let upper = str_val.to_uppercase();

    CString::new(upper).map_or(std::ptr::null_mut(), CString::into_raw)
}

/// Convert string to lowercase (caller must free result)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_to_lowercase(str: *const c_char) -> *mut c_char {
    if str.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = CStr::from_ptr(str).to_str() else {
        return std::ptr::null_mut();
    };

    let lower = str_val.to_lowercase();

    CString::new(lower).map_or(std::ptr::null_mut(), CString::into_raw)
}

/// Check if string starts with prefix
///
/// # Safety
///
/// `str` and `prefix` must be valid pointers to null-terminated C strings, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_starts_with(
    str: *const c_char,
    prefix: *const c_char,
) -> libc::c_int {
    if str.is_null() || prefix.is_null() {
        return 0;
    }

    let Ok(str_val) = CStr::from_ptr(str).to_str() else {
        return 0;
    };

    let Ok(prefix_str) = CStr::from_ptr(prefix).to_str() else {
        return 0;
    };

    i32::from(str_val.starts_with(prefix_str))
}

/// Check if string ends with suffix
///
/// # Safety
///
/// `str` and `suffix` must be valid pointers to null-terminated C strings, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_ends_with(
    str: *const c_char,
    suffix: *const c_char,
) -> libc::c_int {
    if str.is_null() || suffix.is_null() {
        return 0;
    }

    let Ok(str_val) = CStr::from_ptr(str).to_str() else {
        return 0;
    };

    let Ok(suffix_str) = CStr::from_ptr(suffix).to_str() else {
        return 0;
    };

    i32::from(str_val.ends_with(suffix_str))
}

/// Find index of substring (returns -1 if not found)
///
/// # Safety
///
/// `str` and `substring` must be valid pointers to null-terminated C strings, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_find(str: *const c_char, substring: *const c_char) -> i64 {
    if str.is_null() || substring.is_null() {
        return -1;
    }

    let Ok(str_val) = CStr::from_ptr(str).to_str() else {
        return -1;
    };

    let Ok(sub) = CStr::from_ptr(substring).to_str() else {
        return -1;
    };

    str_val.find(sub).map_or(-1, |idx| {
        if idx > i64::MAX as usize {
            -1 // Indicate overflow (very unlikely)
        } else {
            idx as i64
        }
    })
}
