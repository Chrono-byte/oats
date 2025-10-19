//! Networking and HTTP operations

use libc::c_char;

/// HTTP GET request (returns response body as string, caller must free)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_net_http_get(url: *const c_char) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }

    let url_str = match unsafe { std::ffi::CStr::from_ptr(url).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    // Stub implementation - would use reqwest in real implementation
    let response = format!("HTTP GET stub response from: {}", url_str);

    match std::ffi::CString::new(response) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Check if URL is reachable
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_net_url_reachable(url: *const c_char) -> libc::c_int {
    if url.is_null() {
        return 0;
    }

    let url_str = match unsafe { std::ffi::CStr::from_ptr(url).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    // Stub implementation - would actually test connectivity
    if url_str.starts_with("http") { 1 } else { 0 }
}