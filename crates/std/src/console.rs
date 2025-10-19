//! Console output functions

/// Log a message to the console
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log(msg: *mut libc::c_char) {
    if !msg.is_null() {
        // For now, print directly. In a real implementation,
        // we'd wrap this in an OatsString and use safe methods
        unsafe {
            crate::sys::print_string(std::ffi::CStr::from_ptr(msg).to_string_lossy().as_ref());
        }
    }
}

/// Log a number to the console
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log_number(num: f64) {
    crate::sys::print_number_f64(num);
}

/// Log an integer to the console
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log_int(num: i32) {
    crate::sys::print_number_i32(num);
}

/// Print a newline
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_console_log_newline() {
    crate::sys::print_nl();
}
