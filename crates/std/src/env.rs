//! Environment variable operations

use libc::c_char;

/// Get environment variable (caller must free result)
///
/// # Safety
///
/// `name` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_env_get_var(name: *const c_char) -> *mut c_char {
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

/// Set environment variable
///
/// # Safety
///
/// `name` and `value` must be valid pointers to null-terminated C strings, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_env_set_var(
    name: *const c_char,
    value: *const c_char,
) -> libc::c_int {
    if name.is_null() {
        return -1;
    }

    let name_str = match unsafe { std::ffi::CStr::from_ptr(name).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let value_str = if value.is_null() {
        ""
    } else {
        match unsafe { std::ffi::CStr::from_ptr(value).to_str() } {
            Ok(s) => s,
            Err(_) => return -1,
        }
    };

    std::env::set_var(name_str, value_str);
    0
}

/// Remove environment variable
///
/// # Safety
///
/// `name` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_env_remove_var(name: *const c_char) -> libc::c_int {
    if name.is_null() {
        return -1;
    }

    let name_str = match unsafe { std::ffi::CStr::from_ptr(name).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    std::env::remove_var(name_str);
    0
}

/// Get all environment variables (returns array of "KEY=VALUE" strings, caller must free each)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_env_vars() -> *mut *mut c_char {
    let vars: Vec<String> = std::env::vars()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect();

    let count = vars.len();
    if count == 0 {
        return std::ptr::null_mut();
    }

    let ptr_size = std::mem::size_of::<*mut c_char>();
    let array_ptr = unsafe {
        libc::malloc((count + 1) * ptr_size) as *mut *mut c_char
    };

    if array_ptr.is_null() {
        return std::ptr::null_mut();
    }

    unsafe {
        *array_ptr = count as *mut c_char;
    }

    for (i, var) in vars.iter().enumerate() {
        match std::ffi::CString::new(var.as_str()) {
            Ok(cstring) => {
                unsafe {
                    *array_ptr.add(i + 1) = cstring.into_raw();
                }
            }
            Err(_) => {
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

/// Get count of environment variables
///
/// # Safety
///
/// `vars_result` must be a valid pointer returned from `oats_std_env_vars`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_env_vars_count(vars_result: *mut *mut c_char) -> usize {
    if vars_result.is_null() {
        return 0;
    }
    unsafe {
        *vars_result as usize
    }
}

/// Get an environment variable at index (caller must free result)
///
/// # Safety
///
/// `vars_result` must be a valid pointer returned from `oats_std_env_vars`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_env_vars_get(
    vars_result: *mut *mut c_char,
    index: usize,
) -> *mut c_char {
    if vars_result.is_null() {
        return std::ptr::null_mut();
    }

    let count = unsafe { *vars_result as usize };
    if index >= count {
        return std::ptr::null_mut();
    }

    unsafe {
        *vars_result.add(index + 1)
    }
}

/// Free environment variables array
///
/// # Safety
///
/// `vars_result` must be a valid pointer returned from `oats_std_env_vars`.
#[no_mangle]
pub unsafe extern "C" fn oats_std_env_vars_free(vars_result: *mut *mut c_char) {
    if vars_result.is_null() {
        return;
    }

    let count = unsafe { *vars_result as usize };
    unsafe {
        for i in 0..count {
            let ptr = *vars_result.add(i + 1);
            if !ptr.is_null() {
                libc::free(ptr as *mut libc::c_void);
            }
        }
        libc::free(vars_result as *mut libc::c_void);
    }
}
