//! Date object implementation for JavaScript Date

use chrono::{Datelike, Timelike};
use libc::c_char;
use runtime::string::heap_str_from_cstr;
use std::ffi::CString;

/// Create Date from timestamp (milliseconds since epoch)
///
/// Returns timestamp as i64.
#[no_mangle]
pub extern "C" fn oats_std_date_new(timestamp: i64) -> i64 {
    timestamp
}

/// Get current timestamp (milliseconds since epoch)
#[no_mangle]
pub extern "C" fn oats_std_date_now() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_or(0, |d| d.as_millis() as i64)
}

/// Get timestamp from Date (caller must free result)
///
/// # Safety
/// `date_obj` is the timestamp value (i64).
#[no_mangle]
pub extern "C" fn oats_std_date_getTime(date_obj: i64) -> i64 {
    date_obj
}

/// Get year from Date
#[no_mangle]
pub extern "C" fn oats_std_date_getFullYear(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single().map_or(1970, |d| d.year())
}

/// Get month from Date (0-11)
#[no_mangle]
pub extern "C" fn oats_std_date_getMonth(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single().map_or(0, |d| d.month0() as i32)
}

/// Get day of month from Date
#[no_mangle]
pub extern "C" fn oats_std_date_getDate(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single().map_or(1, |d| d.day() as i32)
}

/// Get day of week from Date (0-6, Sunday = 0)
#[no_mangle]
pub extern "C" fn oats_std_date_getDay(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single()
        .map_or(0, |d| d.weekday().num_days_from_sunday() as i32)
}

/// Get hours from Date (0-23)
#[no_mangle]
pub extern "C" fn oats_std_date_getHours(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single().map_or(0, |d| d.hour() as i32)
}

/// Get minutes from Date (0-59)
#[no_mangle]
pub extern "C" fn oats_std_date_getMinutes(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single().map_or(0, |d| d.minute() as i32)
}

/// Get seconds from Date (0-59)
#[no_mangle]
pub extern "C" fn oats_std_date_getSeconds(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single().map_or(0, |d| d.second() as i32)
}

/// Get milliseconds from Date (0-999)
#[no_mangle]
pub extern "C" fn oats_std_date_getMilliseconds(timestamp: i64) -> i32 {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    dt.single()
        .map_or(0, |d| d.timestamp_subsec_millis() as i32)
}

/// Get timezone offset in minutes
#[no_mangle]
pub extern "C" fn oats_std_date_getTimezoneOffset(_timestamp: i64) -> i32 {
    // For now, return 0 (UTC). Full implementation would need local timezone
    0
}

/// Convert Date to string (caller must free result)
#[no_mangle]
pub extern "C" fn oats_std_date_toString(timestamp: i64) -> *mut c_char {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    let formatted = dt
        .single()
        .map_or_else(|| "Invalid Date".to_string(), |d| d.to_string());
    match CString::new(formatted) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Convert Date to ISO string (caller must free result)
#[no_mangle]
pub extern "C" fn oats_std_date_toISOString(timestamp: i64) -> *mut c_char {
    use chrono::{TimeZone, Utc};
    let dt = Utc.timestamp_millis_opt(timestamp);
    let formatted = dt
        .single()
        .map_or_else(|| "Invalid Date".to_string(), |d| d.to_rfc3339());
    match CString::new(formatted) {
        Ok(cstr) => unsafe { heap_str_from_cstr(cstr.as_ptr()) },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Get primitive value (timestamp)
#[no_mangle]
pub extern "C" fn oats_std_date_valueOf(timestamp: i64) -> i64 {
    timestamp
}
