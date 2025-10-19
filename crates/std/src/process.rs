//! Process execution and management

use libc::{c_char, c_int, size_t};
use std::ffi::{CStr, CString};
use std::process::Command;

/// Execute a command and return its output as a string (caller must free)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_process_exec(
    program: *const c_char,
    args: *const *const c_char,
    args_len: size_t,
) -> *mut c_char {
    if program.is_null() {
        return std::ptr::null_mut();
    }

    let program_str = match unsafe { CStr::from_ptr(program).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let mut cmd = Command::new(program_str);

    // Add arguments
    if !args.is_null() && args_len > 0 {
        for i in 0..args_len {
            let arg_ptr = unsafe { *args.add(i) };
            if !arg_ptr.is_null() {
                if let Ok(arg_str) = unsafe { CStr::from_ptr(arg_ptr).to_str() } {
                    cmd.arg(arg_str);
                }
            }
        }
    }

    match cmd.output() {
        Ok(output) => {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            let combined = format!("STDOUT:\n{}\nSTDERR:\n{}", stdout, stderr);

            match CString::new(combined) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        Err(_) => std::ptr::null_mut(),
    }
}

/// Execute a command and return exit code
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_process_exec_status(
    program: *const c_char,
    args: *const *const c_char,
    args_len: size_t,
) -> c_int {
    if program.is_null() {
        return -1;
    }

    let program_str = match unsafe { CStr::from_ptr(program).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let mut cmd = Command::new(program_str);

    // Add arguments
    if !args.is_null() && args_len > 0 {
        for i in 0..args_len {
            let arg_ptr = unsafe { *args.add(i) };
            if !arg_ptr.is_null() {
                if let Ok(arg_str) = unsafe { CStr::from_ptr(arg_ptr).to_str() } {
                    cmd.arg(arg_str);
                }
            }
        }
    }

    match cmd.status() {
        Ok(status) => status.code().unwrap_or(-1),
        Err(_) => -1,
    }
}

/// Check if a command exists in PATH
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_process_command_exists(cmd: *const c_char) -> c_int {
    if cmd.is_null() {
        return 0;
    }

    let cmd_str = match unsafe { CStr::from_ptr(cmd).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    match Command::new(cmd_str).arg("--version").output() {
        Ok(output) => {
            if output.status.success() {
                1
            } else {
                0
            }
        }
        Err(_) => 0,
    }
}

/// Get current working directory (caller must free)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_process_get_current_dir() -> *mut c_char {
    match std::env::current_dir() {
        Ok(path) => {
            let path_str = path.to_string_lossy();
            match CString::new(path_str.as_ref()) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        Err(_) => std::ptr::null_mut(),
    }
}
