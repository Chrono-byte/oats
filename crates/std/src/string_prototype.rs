//! String prototype methods for JavaScript String object

use libc::c_char;
use runtime::string::{heap_str_from_cstr, rc_dec_str};
use std::ffi::{CStr, CString};

const EMPTY_C_STR: &CStr = c"";

/// Get character at index (returns empty string if out of bounds)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data (offset +16 from object base).
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_charAt(
    str_data: *const c_char,
    index: usize,
) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    if index >= str_val.len() {
        // Return empty string
        return unsafe { heap_str_from_cstr(EMPTY_C_STR.as_ptr()) };
    }

    // Get character at index (handles UTF-8)
    let ch = str_val.chars().nth(index).unwrap_or('\0');
    let ch_str = ch.to_string();
    match CString::new(ch_str) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get character code at index (returns NaN as -1 if out of bounds)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_charCodeAt(str_data: *const c_char, index: usize) -> i64 {
    if str_data.is_null() {
        return -1;
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return -1;
    };

    if index >= str_val.len() {
        return -1; // NaN equivalent
    }

    str_val.chars().nth(index).map_or(-1, |ch| ch as u32 as i64)
}

/// Extract substring (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_substring(
    str_data: *const c_char,
    start: usize,
    end: usize,
) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let len = str_val.len();
    let start = start.min(len);
    let end = end.min(len);
    let start = start.min(end);
    let end = end.max(start);

    if start >= len {
        return unsafe { heap_str_from_cstr(EMPTY_C_STR.as_ptr()) };
    }

    let substring = &str_val[start..end];
    match CString::new(substring) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Extract substring with negative indices (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_slice(
    str_data: *const c_char,
    start: i64,
    end: i64,
) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let len = str_val.len() as i64;
    let mut start_idx = if start < 0 {
        ((len + start).max(0)) as usize
    } else {
        (start.min(len)) as usize
    };

    let mut end_idx = if end < 0 {
        ((len + end).max(0)) as usize
    } else if end == i64::MAX {
        len as usize
    } else {
        (end.min(len)) as usize
    };

    if start_idx > end_idx {
        std::mem::swap(&mut start_idx, &mut end_idx);
    }

    if start_idx >= str_val.len() {
        return unsafe { heap_str_from_cstr(EMPTY_C_STR.as_ptr()) };
    }

    let substring = &str_val[start_idx..end_idx];
    match CString::new(substring) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Split string by delimiter (returns array of strings, caller must free each)
///
/// # Safety
/// `str_data` and `delimiter` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_split(
    str_data: *const c_char,
    delimiter: *const c_char,
) -> *mut *mut c_char {
    if str_data.is_null() || delimiter.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let Ok(delim) = (unsafe { CStr::from_ptr(delimiter).to_str() }) else {
        return std::ptr::null_mut();
    };

    let parts: Vec<String> = str_val
        .split(delim)
        .map(std::string::ToString::to_string)
        .collect();
    let count = parts.len();

    if count == 0 {
        return std::ptr::null_mut();
    }

    // Allocate array of pointers
    let ptr_size = std::mem::size_of::<*mut c_char>();
    let array_ptr = unsafe { libc::malloc((count + 1) * ptr_size).cast::<*mut c_char>() };

    if array_ptr.is_null() {
        return std::ptr::null_mut();
    }

    // Store count at first element
    unsafe {
        *array_ptr = count as *mut c_char;
    }

    // Convert each part to Oats string and store pointer
    for (i, part) in parts.iter().enumerate() {
        if let Ok(cstring) = CString::new(part.as_str()) {
            let str_ptr = unsafe { heap_str_from_cstr(cstring.as_ptr()) };
            if str_ptr.is_null() {
                // Cleanup on error
                unsafe {
                    for j in 0..i {
                        let ptr = *array_ptr.add(j + 1);
                        if !ptr.is_null() {
                            rc_dec_str(ptr);
                        }
                    }
                    libc::free(array_ptr.cast::<libc::c_void>());
                }
                return std::ptr::null_mut();
            }
            unsafe {
                *array_ptr.add(i + 1) = str_ptr;
            }
        } else {
            // Cleanup on error
            unsafe {
                for j in 0..i {
                    let ptr = *array_ptr.add(j + 1);
                    if !ptr.is_null() {
                        rc_dec_str(ptr);
                    }
                }
                libc::free(array_ptr.cast::<libc::c_void>());
            }
            return std::ptr::null_mut();
        }
    }

    array_ptr
}

/// Replace first occurrence of substring (caller must free result)
///
/// # Safety
/// `str_data`, `from`, and `to` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_replace(
    str_data: *const c_char,
    from: *const c_char,
    to: *const c_char,
) -> *mut c_char {
    if str_data.is_null() || from.is_null() || to.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let Ok(from_str) = (unsafe { CStr::from_ptr(from).to_str() }) else {
        return std::ptr::null_mut();
    };

    let Ok(to_str) = (unsafe { CStr::from_ptr(to).to_str() }) else {
        return std::ptr::null_mut();
    };

    let replaced = str_val.replacen(from_str, to_str, 1);
    match CString::new(replaced) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Replace all occurrences of substring (caller must free result)
///
/// # Safety
/// `str_data`, `from`, and `to` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_replaceAll(
    str_data: *const c_char,
    from: *const c_char,
    to: *const c_char,
) -> *mut c_char {
    if str_data.is_null() || from.is_null() || to.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let Ok(from_str) = (unsafe { CStr::from_ptr(from).to_str() }) else {
        return std::ptr::null_mut();
    };

    let Ok(to_str) = (unsafe { CStr::from_ptr(to).to_str() }) else {
        return std::ptr::null_mut();
    };

    let replaced = str_val.replace(from_str, to_str);
    match CString::new(replaced) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Convert to lowercase (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_toLowerCase(str_data: *const c_char) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let lower = str_val.to_lowercase();
    match CString::new(lower) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Convert to uppercase (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_toUpperCase(str_data: *const c_char) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let upper = str_val.to_uppercase();
    match CString::new(upper) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Trim whitespace from both ends (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_trim(str_data: *const c_char) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let trimmed = str_val.trim();
    match CString::new(trimmed) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Trim leading whitespace (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_trimStart(str_data: *const c_char) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let trimmed = str_val.trim_start();
    match CString::new(trimmed) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Trim trailing whitespace (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_trimEnd(str_data: *const c_char) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let trimmed = str_val.trim_end();
    match CString::new(trimmed) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Check if string starts with prefix
///
/// # Safety
/// `str_data` and `prefix` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_startsWith(
    str_data: *const c_char,
    prefix: *const c_char,
) -> libc::c_int {
    if str_data.is_null() || prefix.is_null() {
        return 0;
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return 0;
    };

    let Ok(prefix_str) = (unsafe { CStr::from_ptr(prefix).to_str() }) else {
        return 0;
    };

    i32::from(str_val.starts_with(prefix_str))
}

/// Check if string ends with suffix
///
/// # Safety
/// `str_data` and `suffix` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_endsWith(
    str_data: *const c_char,
    suffix: *const c_char,
) -> libc::c_int {
    if str_data.is_null() || suffix.is_null() {
        return 0;
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return 0;
    };

    let Ok(suffix_str) = (unsafe { CStr::from_ptr(suffix).to_str() }) else {
        return 0;
    };

    i32::from(str_val.ends_with(suffix_str))
}

/// Check if string includes substring
///
/// # Safety
/// `str_data` and `substring` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_includes(
    str_data: *const c_char,
    substring: *const c_char,
) -> libc::c_int {
    if str_data.is_null() || substring.is_null() {
        return 0;
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return 0;
    };

    let Ok(sub) = (unsafe { CStr::from_ptr(substring).to_str() }) else {
        return 0;
    };

    i32::from(str_val.contains(sub))
}

/// Find index of substring (returns -1 if not found)
///
/// # Safety
/// `str_data` and `substring` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_indexOf(
    str_data: *const c_char,
    substring: *const c_char,
) -> i64 {
    if str_data.is_null() || substring.is_null() {
        return -1;
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return -1;
    };

    let Ok(sub) = (unsafe { CStr::from_ptr(substring).to_str() }) else {
        return -1;
    };

    str_val.find(sub).map_or(-1, |idx| {
        if idx > i64::MAX as usize {
            -1
        } else {
            idx as i64
        }
    })
}

/// Find last index of substring (returns -1 if not found)
///
/// # Safety
/// `str_data` and `substring` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_lastIndexOf(
    str_data: *const c_char,
    substring: *const c_char,
) -> i64 {
    if str_data.is_null() || substring.is_null() {
        return -1;
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return -1;
    };

    let Ok(sub) = (unsafe { CStr::from_ptr(substring).to_str() }) else {
        return -1;
    };

    str_val.rfind(sub).map_or(-1, |idx| {
        if idx > i64::MAX as usize {
            -1
        } else {
            idx as i64
        }
    })
}

/// Pad start of string (caller must free result)
///
/// # Safety
/// `str_data` and `pad_string` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_padStart(
    str_data: *const c_char,
    target_length: usize,
    pad_string: *const c_char,
) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let pad = if pad_string.is_null() {
        " "
    } else {
        unsafe { CStr::from_ptr(pad_string).to_str().unwrap_or(" ") }
    };

    let len = str_val.len();
    if len >= target_length {
        // Return copy of original string
        match CString::new(str_val) {
            Ok(cstr) => return unsafe { heap_str_from_cstr(cstr.as_ptr()) },
            Err(_) => return std::ptr::null_mut(),
        }
    }

    let pad_len = target_length - len;
    let pad_repeat = (pad_len as f64 / pad.len() as f64).ceil() as usize;
    let padding: String = pad.repeat(pad_repeat);
    let padding = &padding[..pad_len.min(padding.len())];

    let result = format!("{padding}{str_val}");
    match CString::new(result) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Pad end of string (caller must free result)
///
/// # Safety
/// `str_data` and `pad_string` must be valid pointers to Oats string data.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_padEnd(
    str_data: *const c_char,
    target_length: usize,
    pad_string: *const c_char,
) -> *mut c_char {
    if str_data.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let pad = if pad_string.is_null() {
        " "
    } else {
        unsafe { CStr::from_ptr(pad_string).to_str().unwrap_or(" ") }
    };

    let len = str_val.len();
    if len >= target_length {
        // Return copy of original string
        match CString::new(str_val) {
            Ok(cstr) => return unsafe { heap_str_from_cstr(cstr.as_ptr()) },
            Err(_) => return std::ptr::null_mut(),
        }
    }

    let pad_len = target_length - len;
    let pad_repeat = (pad_len as f64 / pad.len() as f64).ceil() as usize;
    let padding: String = pad.repeat(pad_repeat);
    let padding = &padding[..pad_len.min(padding.len())];

    let result = format!("{str_val}{padding}");
    match CString::new(result) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Repeat string n times (caller must free result)
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
///
/// # Panics
/// This function will panic if the string contains interior nul bytes.
#[no_mangle]
pub unsafe extern "C" fn oats_std_string_repeat(
    str_data: *const c_char,
    count: usize,
) -> *mut c_char {
    if str_data.is_null() || count == 0 {
        return unsafe { heap_str_from_cstr(EMPTY_C_STR.as_ptr()) };
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return std::ptr::null_mut();
    };

    let repeated = str_val.repeat(count);
    match CString::new(repeated) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}
/// Get string length
///
/// # Safety
/// `str_data` must be a valid pointer to Oats string data.
#[no_mangle]
pub const unsafe extern "C" fn oats_std_string_length(str_data: *const c_char) -> usize {
    if str_data.is_null() {
        return 0;
    }

    let Ok(str_val) = (unsafe { CStr::from_ptr(str_data).to_str() }) else {
        return 0;
    };

    str_val.len()
}
