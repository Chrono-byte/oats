//! Console output functions

/// Log a message to the console
///
/// # Safety
///
/// `msg` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_console_log(msg: *mut libc::c_char) {
    if !msg.is_null() {
        // Use a let binding to extend the lifetime of the value
        let s = unsafe { std::ffi::CStr::from_ptr(msg).to_string_lossy() };
        crate::sys::println(s);
    }
}

/// Log a number to the console
/// #[oats_export]
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_console_log_number(num: f64) {
    crate::sys::println(num);
}

/// Log an integer to the console
/// #[oats_export]
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_console_log_int(num: i32) {
    crate::sys::println(num);
}

/// Print a newline
/// #[oats_export]
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_console_log_newline() {
    crate::sys::println("");
}
