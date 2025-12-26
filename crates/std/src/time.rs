//! Time and date operations

use libc::c_longlong;

/// Get current Unix timestamp (seconds since epoch)
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_time_now() -> c_longlong {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .map_or(0, |duration| {
            duration.as_secs().min(i64::MAX as u64) as c_longlong
        })
}

/// Sleep for specified milliseconds
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_time_sleep_ms(ms: c_longlong) {
    if ms > 0 {
        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
    }
}

/// Get current Unix timestamp in nanoseconds
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_time_now_nanos() -> c_longlong {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .map_or(0, |duration| duration.as_nanos() as c_longlong)
}

/// Get current Unix timestamp in milliseconds
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_time_now_millis() -> c_longlong {
    std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .map_or(0, |duration| duration.as_millis() as c_longlong)
}

/// Sleep for specified seconds
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_time_sleep_secs(secs: c_longlong) {
    if secs > 0 {
        std::thread::sleep(std::time::Duration::from_secs(secs as u64));
    }
}

/// Sleep for specified nanoseconds
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_time_sleep_nanos(nanos: c_longlong) {
    if nanos > 0 {
        std::thread::sleep(std::time::Duration::from_nanos(nanos as u64));
    }
}

/// Format timestamp as RFC 3339 string (caller must free result)
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_time_format_rfc3339(timestamp: c_longlong) -> *mut libc::c_char {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_opt(timestamp, 0);
    match dt {
        chrono::LocalResult::Single(dt) => {
            let formatted = dt.to_rfc3339();
            std::ffi::CString::new(formatted)
                .map_or(std::ptr::null_mut(), std::ffi::CString::into_raw)
        }
        _ => std::ptr::null_mut(),
    }
}

/// Parse RFC 3339 string to timestamp (returns -1 on error)
///
/// # Safety
///
/// `rfc3339_str` must be a valid pointer to a null-terminated C string, or null.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_time_parse_rfc3339(
    rfc3339_str: *const libc::c_char,
) -> c_longlong {
    if rfc3339_str.is_null() {
        return -1;
    }

    let str_val = (unsafe { std::ffi::CStr::from_ptr(rfc3339_str).to_str() }).unwrap_or_default();

    chrono::DateTime::parse_from_rfc3339(str_val).map_or(-1, |dt| dt.timestamp())
}
