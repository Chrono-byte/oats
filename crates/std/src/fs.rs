//! File system operations

use libc::c_char;
use std::ffi::{CStr, CString};
use std::fs;
use runtime::heap::{runtime_malloc, runtime_free};

/// Read entire file into a string.
///
/// The returned pointer must be freed using `runtime_free_cstring()`.
/// Passing the pointer to `libc::free()` or not freeing it will cause a memory leak.
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
///
/// # Returns
/// A pointer to a null-terminated C string containing the file contents, or null on error.
/// The caller is responsible for freeing the string using `runtime_free_cstring()`.
///
/// # Example
/// ```c
/// char* content = oats_std_fs_read_file(path);
/// if (content != NULL) {
///     // use content...
///     runtime_free_cstring(content);
/// }
/// ```
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_read_file(path: *const c_char) -> *mut c_char {
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
///
/// # Safety
///
/// `path` and `content` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_write_file(
    path: *const c_char,
    content: *const c_char,
) -> libc::c_int {
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
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_file_exists(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return 0;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if std::path::Path::new(path_str).exists() {
        1
    } else {
        0
    }
}

/// Create directory recursively
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_create_dir_all(path: *const c_char) -> libc::c_int {
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
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_remove_file(path: *const c_char) -> libc::c_int {
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

/// Remove directory (must be empty)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_remove_dir(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    match fs::remove_dir(path_str) {
        Ok(_) => 0,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => -2,
            std::io::ErrorKind::PermissionDenied => -3,
            _ => -1,
        },
    }
}

/// Remove directory recursively
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_remove_dir_all(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    match fs::remove_dir_all(path_str) {
        Ok(_) => 0,
        Err(e) => match e.kind() {
            std::io::ErrorKind::NotFound => -2,
            std::io::ErrorKind::PermissionDenied => -3,
            _ => -1,
        },
    }
}

/// Check if path is a directory
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_is_dir(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return 0;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if std::path::Path::new(path_str).is_dir() {
        1
    } else {
        0
    }
}

/// Check if path is a file
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_is_file(path: *const c_char) -> libc::c_int {
    if path.is_null() {
        return 0;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    if std::path::Path::new(path_str).is_file() {
        1
    } else {
        0
    }
}

/// Get file size in bytes
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_file_size(path: *const c_char) -> i64 {
    if path.is_null() {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    match fs::metadata(path_str) {
        Ok(metadata) => metadata.len() as i64,
        Err(_) => -1,
    }
}

/// List directory entries (returns array of strings, caller must free each)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_read_dir(path: *const c_char) -> *mut *mut c_char {
    if path.is_null() {
        return std::ptr::null_mut();
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let entries: Vec<String> = match fs::read_dir(path_str) {
        Ok(dir) => {
            let mut names = Vec::new();
            for entry in dir.flatten() {
                if let Some(name) = entry.file_name().to_str() {
                    names.push(name.to_string());
                }
            }
            names
        }
        Err(_) => return std::ptr::null_mut(),
    };

    let count = entries.len();
    if count == 0 {
        return std::ptr::null_mut();
    }

    // Allocate array of pointers using runtime allocator for consistency
    let ptr_size = std::mem::size_of::<*mut c_char>();
    let array_ptr = unsafe {
        runtime_malloc((count + 1) * ptr_size) as *mut *mut c_char
    };

    if array_ptr.is_null() {
        return std::ptr::null_mut();
    }

    // Store count at first element
    unsafe {
        *array_ptr = count as *mut c_char;
    }

    // Convert each entry to CString and store pointer
    for (i, entry) in entries.iter().enumerate() {
        match CString::new(entry.as_str()) {
            Ok(cstring) => unsafe {
                *array_ptr.add(i + 1) = cstring.into_raw();
            },
            Err(_) => {
                // Cleanup on error - free CStrings and array
                unsafe {
                    for j in 0..i {
                        let cstr_ptr = *array_ptr.add(j + 1);
                        if !cstr_ptr.is_null() {
                            // CString::from_raw will take ownership and free on drop
                            let _ = CString::from_raw(cstr_ptr);
                        }
                    }
                    runtime_free(array_ptr as *mut std::ffi::c_void);
                }
                return std::ptr::null_mut();
            }
        }
    }

    array_ptr
}

/// Get count of directory entries
///
/// # Safety
///
/// `dir_result` must be a valid pointer returned from `oats_std_fs_read_dir`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_read_dir_count(dir_result: *mut *mut c_char) -> usize {
    if dir_result.is_null() {
        return 0;
    }
    unsafe { *dir_result as usize }
}

/// Get a directory entry at index (caller must free result)
///
/// # Safety
///
/// `dir_result` must be a valid pointer returned from `oats_std_fs_read_dir`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_read_dir_get(
    dir_result: *mut *mut c_char,
    index: usize,
) -> *mut c_char {
    if dir_result.is_null() {
        return std::ptr::null_mut();
    }

    let count = unsafe { *dir_result as usize };
    if index >= count {
        return std::ptr::null_mut();
    }

    unsafe { *dir_result.add(index + 1) }
}

/// Free a directory listing array
///
/// # Safety
///
/// `dir_result` must be a valid pointer returned from `oats_std_fs_read_dir`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_fs_read_dir_free(dir_result: *mut *mut c_char) {
    if dir_result.is_null() {
        return;
    }

    let count = unsafe { *dir_result as usize };
    unsafe {
        for i in 0..count {
            let ptr = *dir_result.add(i + 1);
            if !ptr.is_null() {
                // CString::from_raw will take ownership and free on drop
                let _ = CString::from_raw(ptr);
            }
        }
        runtime_free(dir_result as *mut std::ffi::c_void);
    }
}
