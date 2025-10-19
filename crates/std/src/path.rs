//! Path manipulation utilities

use libc::c_char;
use std::ffi::{CStr, CString};
use std::path::Path;

/// Join two paths (caller must free result)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_path_join(base: *const c_char, component: *const c_char) -> *mut c_char {
    if base.is_null() || component.is_null() {
        return std::ptr::null_mut();
    }

    let base_str = match unsafe { CStr::from_ptr(base).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let component_str = match unsafe { CStr::from_ptr(component).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let joined = Path::new(base_str).join(component_str);
    let joined_str = joined.to_string_lossy();

    match CString::new(joined_str.as_ref()) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get file name from path (caller must free result)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_path_file_name(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    match Path::new(path_str).file_name() {
        Some(name) => {
            let name_str = name.to_string_lossy();
            match CString::new(name_str.as_ref()) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        None => std::ptr::null_mut(),
    }
}

/// Check if path is absolute
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_path_is_absolute(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return 0;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if Path::new(path_str).is_absolute() {
        1
    } else {
        0
    }
}
