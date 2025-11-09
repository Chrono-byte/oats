//! Networking and HTTP operations

use libc::c_char;

/// HTTP GET request (returns response body as string, caller must free)
///
/// # Safety
///
/// `url` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_http_get(url: *const c_char) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }

    let url_str = match unsafe { std::ffi::CStr::from_ptr(url).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    // Use reqwest blocking client for HTTP requests
    match reqwest::blocking::get(url_str) {
        Ok(response) => match response.text() {
            Ok(body) => match std::ffi::CString::new(body) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            },
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}

/// Check if URL is reachable
///
/// # Safety
///
/// `url` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_url_reachable(url: *const c_char) -> libc::c_int {
    if url.is_null() {
        return 0;
    }

    let url_str = match unsafe { std::ffi::CStr::from_ptr(url).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    // Use reqwest to actually test connectivity with a HEAD request
    // HEAD is more efficient than GET for just checking reachability
    let client = match reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(5))
        .build()
    {
        Ok(c) => c,
        Err(_) => return 0,
    };

    match client.head(url_str).send() {
        Ok(response) => {
            if response.status().is_success() || response.status().is_redirection() {
                1
            } else {
                0
            }
        }
        Err(_) => 0,
    }
}

/// HTTP POST request (returns response body as string, caller must free)
///
/// # Safety
///
/// `url` and `body` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_http_post(
    url: *const c_char,
    body: *const c_char,
) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }

    let url_str = match unsafe { std::ffi::CStr::from_ptr(url).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let body_str = if body.is_null() {
        ""
    } else {
        match unsafe { std::ffi::CStr::from_ptr(body).to_str() } {
            Ok(s) => s,
            Err(_) => return std::ptr::null_mut(),
        }
    };

    let client = reqwest::blocking::Client::new();

    match client.post(url_str).body(body_str.to_string()).send() {
        Ok(response) => match response.text() {
            Ok(body) => match std::ffi::CString::new(body) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            },
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}

/// HTTP POST request with JSON body (returns response body as string, caller must free)
///
/// # Safety
///
/// `url` and `json_body` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_http_post_json(
    url: *const c_char,
    json_body: *const c_char,
) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }

    let url_str = match unsafe { std::ffi::CStr::from_ptr(url).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    let json_str = if json_body.is_null() {
        "{}"
    } else {
        match unsafe { std::ffi::CStr::from_ptr(json_body).to_str() } {
            Ok(s) => s,
            Err(_) => return std::ptr::null_mut(),
        }
    };

    let client = reqwest::blocking::Client::new();

    match client
        .post(url_str)
        .header("Content-Type", "application/json")
        .body(json_str.to_string())
        .send()
    {
        Ok(response) => match response.text() {
            Ok(body) => match std::ffi::CString::new(body) {
                Ok(cstring) => cstring.into_raw(),
                Err(_) => std::ptr::null_mut(),
            },
            Err(_) => std::ptr::null_mut(),
        },
        Err(_) => std::ptr::null_mut(),
    }
}
