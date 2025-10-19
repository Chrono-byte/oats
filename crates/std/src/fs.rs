//! File system operations

use libc::c_char;
use std::ffi::{CStr, CString};
use std::fs;

/// Read entire file into a string
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_fs_read_file(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    match fs::read_to_string(path_str) {
        Ok(content) => match CString::new(content) {
            Ok(cstring) => cstring.into_raw(),
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Write string to file
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_fs_write_file(path: *const c_char, content: *const c_char) -> libc::c_int {
    if path.is_null() || content.is_null() {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let content_str = unsafe { CStr::from_ptr(content).to_string_lossy() };

    match fs::write(path_str, content_str.as_bytes()) {
        Ok(_) => 0,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => -2,
            std::io::ErrorKind::PermissionDenied => -3,
            _ => -1,
        },
    }
}

/// Check if file exists
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_fs_file_exists(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return 0;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if std::path::Path::new(path_str).exists() { 1 } else { 0 }
}

/// Create directory recursively
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_fs_create_dir_all(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    match fs::create_dir_all(path_str) {
        Ok(_) => 0,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => -2,
            std::io::ErrorKind::PermissionDenied => -3,
            _ => -1,
        },
    }
}

/// Remove file
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_fs_remove_file(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    match fs::remove_file(path_str) {
        Ok(_) => 0,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => -2,
            std::io::ErrorKind::PermissionDenied => -3,
            _ => -1,
        },
    }
}