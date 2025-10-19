//! Environment variable operations

use libc::c_char;

/// Get environment variable (caller must free result)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_env_get_var(name: *const c_char) -> *mut c_char {
    if name.is_null() {
        return std::ptr::null_mut();
    }

    let name_str = match unsafe { std::ffi::CStr::from_ptr(name).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    match std::env::var(name_str) {
        Ok(value) => match std::ffi::CString::new(value) {
            Ok(cstring) => cstring.into_raw(),
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get current executable path (caller must free)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_env_current_exe() -> *mut c_char {
    match std::env::current_exe() {
        Ok(path) => {
            let path_str = path.to_string_lossy();
            match std::ffi::CString::new(path_str.as_ref()) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        Err(_) => std::ptr::null_mut(),
    }
}