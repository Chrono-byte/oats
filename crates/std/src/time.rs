//! Time and date operations

use libc::c_longlong;

/// Get current Unix timestamp (seconds since epoch)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_time_now() -> c_longlong {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs() as c_longlong
}

/// Sleep for specified milliseconds
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_time_sleep_ms(ms: c_longlong) {
    if ms > 0 {
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    }
}
