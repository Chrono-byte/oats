//! Path manipulation utilities

use libc::c_char;
use std::ffi::{CStr, CString};
use std::path::Path;

/// Join two paths (caller must free result)
///
/// # Safety
///
/// `base` and `component` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
/// #[oats_export]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn oats_std_path_join(
    base: *const c_char,
    component: *const c_char,
) -> *mut c_char {
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
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn oats_std_path_file_name(path: *const c_char) -> *mut c_char {
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
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn oats_std_path_is_absolute(path: *const c_char) -> libc::c_int {
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

/// Get directory name from path (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn oats_std_path_dir_name(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    match Path::new(path_str).parent() {
        Some(parent) => {
            let parent_str = parent.to_string_lossy();
            match CString::new(parent_str.as_ref()) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        None => std::ptr::null_mut(),
    }
}

/// Get file extension (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn oats_std_path_extension(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    match Path::new(path_str).extension() {
        Some(ext) => {
            let ext_str = ext.to_string_lossy();
            match CString::new(ext_str.as_ref()) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        None => std::ptr::null_mut(),
    }
}

/// Get file stem (filename without extension) (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn oats_std_path_stem(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    match Path::new(path_str).file_stem() {
        Some(stem) => {
            let stem_str = stem.to_string_lossy();
            match CString::new(stem_str.as_ref()) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        None => std::ptr::null_mut(),
    }
}

/// Normalize a path (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[unsafe(no_mangle)]
pub unsafe extern "C" fn oats_std_path_normalize(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let normalized = Path::new(path_str)
        .components()
        .collect::<std::path::PathBuf>();
    let normalized_str = normalized.to_string_lossy();

    match CString::new(normalized_str.as_ref()) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}
