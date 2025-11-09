//! String formatting operations

use libc::c_char;
use std::ffi::{CStr, CString};

/// Format a string with arguments (caller must free result)
///
/// # Safety
///
/// `format` must be a valid pointer to a null-terminated C string.
/// `args` must be a valid pointer to an array of `args_len` pointers to null-terminated C strings.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_format_string(
    format: *const c_char,
    args: *const *const c_char,
    args_len: usize,
) -> *mut c_char {
    if format.is_null() {
        return std::ptr::null_mut();
    }

    let format_str = match unsafe { CStr::from_ptr(format).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    // Simple placeholder replacement: {} -> argument
    let mut result = String::new();
    let mut arg_index = 0;
    let mut chars = format_str.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '{' && chars.peek() == Some(&'}') {
            chars.next(); // consume '}'
            if !args.is_null() && arg_index < args_len {
                let arg_ptr = unsafe { *args.add(arg_index) };
                if !arg_ptr.is_null() {
                    if let Ok(arg_str) = unsafe { CStr::from_ptr(arg_ptr).to_str() } {
                        result.push_str(arg_str);
                    }
                }
                arg_index += 1;
            } else {
                result.push_str("{}");
            }
        } else {
            result.push(ch);
        }
    }

    match CString::new(result) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Format with a number (caller must free result)
///
/// # Safety
///
/// `format` must be a valid pointer to a null-terminated C string.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_format_number(format: *const c_char, value: f64) -> *mut c_char {
    if format.is_null() {
        return std::ptr::null_mut();
    }

    let format_str = match unsafe { CStr::from_ptr(format).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let result = format_str.replace("{}", &value.to_string());

    match CString::new(result) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Format with an integer (caller must free result)
///
/// # Safety
///
/// `format` must be a valid pointer to a null-terminated C string.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_format_int(format: *const c_char, value: i64) -> *mut c_char {
    if format.is_null() {
        return std::ptr::null_mut();
    }

    let format_str = match unsafe { CStr::from_ptr(format).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let result = format_str.replace("{}", &value.to_string());

    match CString::new(result) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}
