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
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_path_join(
    base: *const c_char,
    component: *const c_char,
) -> *mut c_char {
    if base.is_null() || component.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(base_str) = (unsafe { CStr::from_ptr(base).to_str() }) else {
        return std::ptr::null_mut();
    };

    let Ok(component_str) = (unsafe { CStr::from_ptr(component).to_str() }) else {
        return std::ptr::null_mut();
    };

    let joined = Path::new(base_str).join(component_str);
    let joined_str = joined.to_string_lossy();

    CString::new(joined_str.as_ref()).map_or(std::ptr::null_mut(), CString::into_raw)
}

/// Get file name from path (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_path_file_name(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(path_str) = (unsafe { CStr::from_ptr(path).to_str() }) else {
        return std::ptr::null_mut();
    };

    Path::new(path_str)
        .file_name()
        .map_or(std::ptr::null_mut(), |name| {
            let name_str = name.to_string_lossy();
            CString::new(name_str.as_ref()).map_or(std::ptr::null_mut(), CString::into_raw)
        })
}

/// Check if path is absolute
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_path_is_absolute(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return 0;
    }

    let Ok(path_str) = (unsafe { CStr::from_ptr(path).to_str() }) else {
        return 0;
    };

    i32::from(Path::new(path_str).is_absolute())
}

/// Get directory name from path (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_path_dir_name(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(path_str) = (unsafe { CStr::from_ptr(path).to_str() }) else {
        return std::ptr::null_mut();
    };

    Path::new(path_str)
        .parent()
        .map_or(std::ptr::null_mut(), |parent| {
            let parent_str = parent.to_string_lossy();
            CString::new(parent_str.as_ref()).map_or(std::ptr::null_mut(), CString::into_raw)
        })
}

/// Get file extension (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_path_extension(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(path_str) = (unsafe { CStr::from_ptr(path).to_str() }) else {
        return std::ptr::null_mut();
    };

    Path::new(path_str)
        .extension()
        .map_or(std::ptr::null_mut(), |ext| {
            let ext_str = ext.to_string_lossy();
            CString::new(ext_str.as_ref()).map_or(std::ptr::null_mut(), CString::into_raw)
        })
}

/// Get file stem (filename without extension) (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_path_stem(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(path_str) = (unsafe { CStr::from_ptr(path).to_str() }) else {
        return std::ptr::null_mut();
    };

    Path::new(path_str)
        .file_stem()
        .map_or(std::ptr::null_mut(), |stem| {
            let stem_str = stem.to_string_lossy();
            CString::new(stem_str.as_ref()).map_or(std::ptr::null_mut(), CString::into_raw)
        })
}

/// Normalize a path (caller must free result)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_path_normalize(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(path_str) = (unsafe { CStr::from_ptr(path).to_str() }) else {
        return std::ptr::null_mut();
    };

    let normalized = Path::new(path_str)
        .components()
        .collect::<std::path::PathBuf>();
    let normalized_str = normalized.to_string_lossy();

    CString::new(normalized_str.as_ref()).map_or(std::ptr::null_mut(), CString::into_raw)
}
