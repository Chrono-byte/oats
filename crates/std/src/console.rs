//! Console output functions

/// Log a message to the console
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log(msg: *mut libc::c_char) {
    if !msg.is_null() {
        unsafe {
            crate::sys::println(std::ffi::CStr::from_ptr(msg).to_string_lossy().as_ref());
        }
    }
}

/// Log a number to the console
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log_number(num: f64) {
    crate::sys::println(num);
}

/// Log an integer to the console
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log_int(num: i32) {
    crate::sys::println(num);
}

/// Print a newline
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log_newline() {
    crate::sys::println("");
}
