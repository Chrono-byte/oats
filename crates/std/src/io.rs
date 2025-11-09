//! Input/Output operations

use libc::c_char;
use std::ffi::{CStr, CString};
use std::io::{self, Read, Write};

/// Read from stdin into a buffer.
///
/// The returned pointer must be freed using `runtime_free_cstring()`.
/// Passing the pointer to `libc::free()` or not freeing it will cause a memory leak.
///
/// # Returns
/// A pointer to a null-terminated C string, or null on error.
/// The caller is responsible for freeing the string using `runtime_free_cstring()`.
///
/// # Example
/// ```c
/// char* line = oats_std_io_read_line();
/// if (line != NULL) {
///     // use line...
///     runtime_free_cstring(line);
/// }
/// ```
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_io_read_line() -> *mut c_char {
    let mut buffer = String::new();
    match io::stdin().read_line(&mut buffer) {
        Ok(_) => {
            // Remove trailing newline if present
            let trimmed = buffer.trim_end_matches('\n').trim_end_matches('\r');
            match CString::new(trimmed) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            }
        }
        Err(_) => std::ptr::null_mut(),
    }
}

/// Read bytes from stdin (returns number of bytes read, -1 on error)
///
/// # Safety
///
/// `buffer` must be a valid pointer to at least `len` bytes of writable memory.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_io_read_bytes(buffer: *mut u8, len: usize) -> i64 {
    if buffer.is_null() || len == 0 {
        return -1;
    }

    let slice = unsafe { std::slice::from_raw_parts_mut(buffer, len) };
    match io::stdin().read(slice) {
        Ok(n) => n as i64,
        Err(_) => -1,
    }
}

/// Write bytes to stdout (returns number of bytes written, -1 on error)
///
/// # Safety
///
/// `buffer` must be a valid pointer to at least `len` bytes of readable memory.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_io_write_bytes(buffer: *const u8, len: usize) -> i64 {
    if buffer.is_null() || len == 0 {
        return -1;
    }

    let slice = unsafe { std::slice::from_raw_parts(buffer, len) };
    match io::stdout().write_all(slice) {
        Ok(_) => len as i64,
        Err(_) => -1,
    }
}

/// Flush stdout
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_io_flush() -> libc::c_int {
    match io::stdout().flush() {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// Read entire file as bytes (returns number of bytes read, -1 on error)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string.
/// `buffer` must be a valid pointer to at least `len` bytes of writable memory.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_io_read_file_bytes(
    path: *const c_char,
    buffer: *mut u8,
    len: usize,
) -> i64 {
    if path.is_null() || buffer.is_null() || len == 0 {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let slice = unsafe { std::slice::from_raw_parts_mut(buffer, len) };
    match std::fs::File::open(path_str) {
        Ok(mut file) => match file.read(slice) {
            Ok(n) => n as i64,
            Err(_) => -1,
        },
        Err(_) => -1,
    }
}

/// Write bytes to file (returns number of bytes written, -1 on error)
///
/// # Safety
///
/// `path` must be a valid pointer to a null-terminated C string.
/// `buffer` must be a valid pointer to at least `len` bytes of readable memory.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_io_write_file_bytes(
    path: *const c_char,
    buffer: *const u8,
    len: usize,
) -> i64 {
    if path.is_null() || buffer.is_null() || len == 0 {
        return -1;
    }

    let path_str = match unsafe { CStr::from_ptr(path).to_str() } {
        Ok(s) => s,
        Err(_) => return -1,
    };

    let slice = unsafe { std::slice::from_raw_parts(buffer, len) };
    match std::fs::File::create(path_str) {
        Ok(mut file) => match file.write_all(slice) {
            Ok(_) => len as i64,
            Err(_) => -1,
        },
        Err(_) => -1,
    }
}
