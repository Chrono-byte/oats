//! Networking and HTTP operations

use libc::c_char;

/// HTTP GET request (returns response body as string).
///
/// The returned pointer must be freed using `runtime_free_cstring()`.
/// Passing the pointer to `libc::free()` or not freeing it will cause a memory leak.
///
/// # Safety
///
/// `url` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
///
/// # Returns
/// A pointer to a null-terminated C string containing the response body, or null on error.
/// The caller is responsible for freeing the string using `runtime_free_cstring()`.
///
/// # Example
/// ```c
/// char* response = oats_std_net_http_get(url);
/// if (response != NULL) {
///     // use response...
///     runtime_free_cstring(response);
/// }
/// ```
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_http_get(url: *const c_char) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(url_str) = (unsafe { std::ffi::CStr::from_ptr(url).to_str() }) else {
        return std::ptr::null_mut();
    };

    // Use reqwest blocking client for HTTP requests
    reqwest::blocking::get(url_str)
        .ok()
        .and_then(|response| response.text().ok())
        .and_then(|body| std::ffi::CString::new(body).ok())
        .map_or(std::ptr::null_mut(), std::ffi::CString::into_raw)
}

/// Check if URL is reachable
///
/// # Safety
///
/// `url` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_url_reachable(url: *const c_char) -> libc::c_int {
    if url.is_null() {
        return 0;
    }

    let Ok(url_str) = (unsafe { std::ffi::CStr::from_ptr(url).to_str() }) else {
        return 0;
    };

    // Use reqwest to actually test connectivity with a HEAD request
    // HEAD is more efficient than GET for just checking reachability
    let Ok(client) = reqwest::blocking::Client::builder()
        .timeout(std::time::Duration::from_secs(5))
        .build()
    else {
        return 0;
    };

    client.head(url_str).send().map_or(0, |response| {
        i32::from(response.status().is_success() || response.status().is_redirection())
    })
}

/// HTTP POST request (returns response body as string).
///
/// The returned pointer must be freed using `runtime_free_cstring()`.
/// Passing the pointer to `libc::free()` or not freeing it will cause a memory leak.
///
/// # Safety
///
/// `url` and `body` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
///
/// # Returns
/// A pointer to a null-terminated C string containing the response body, or null on error.
/// The caller is responsible for freeing the string using `runtime_free_cstring()`.
///
/// # Example
/// ```c
/// char* response = oats_std_net_http_post(url, body);
/// if (response != NULL) {
///     // use response...
///     runtime_free_cstring(response);
/// }
/// ```
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_http_post(
    url: *const c_char,
    body: *const c_char,
) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(url_str) = (unsafe { std::ffi::CStr::from_ptr(url).to_str() }) else {
        return std::ptr::null_mut();
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

    client
        .post(url_str)
        .body(body_str.to_string())
        .send()
        .map_or(std::ptr::null_mut(), |response| {
            response.text().map_or(std::ptr::null_mut(), |body| {
                std::ffi::CString::new(body)
                    .map_or(std::ptr::null_mut(), std::ffi::CString::into_raw)
            })
        })
}

/// HTTP POST request with JSON body (returns response body as string).
///
/// The returned pointer must be freed using `runtime_free_cstring()`.
/// Passing the pointer to `libc::free()` or not freeing it will cause a memory leak.
///
/// # Safety
///
/// `url` and `json_body` must be valid pointers to null-terminated C strings, or null.
/// If non-null, the strings must remain valid for the duration of this call.
///
/// # Returns
/// A pointer to a null-terminated C string containing the response body, or null on error.
/// The caller is responsible for freeing the string using `runtime_free_cstring()`.
///
/// # Example
/// ```c
/// char* response = oats_std_net_http_post_json(url, json_body);
/// if (response != NULL) {
///     // use response...
///     runtime_free_cstring(response);
/// }
/// ```
/// #[`oats_export`]
#[no_mangle]
pub unsafe extern "C" fn oats_std_net_http_post_json(
    url: *const c_char,
    json_body: *const c_char,
) -> *mut c_char {
    if url.is_null() {
        return std::ptr::null_mut();
    }

    let Ok(url_str) = (unsafe { std::ffi::CStr::from_ptr(url).to_str() }) else {
        return std::ptr::null_mut();
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

    client
        .post(url_str)
        .header("Content-Type", "application/json")
        .body(json_str.to_string())
        .send()
        .map_or(std::ptr::null_mut(), |response| {
            response.text().map_or(std::ptr::null_mut(), |body| {
                std::ffi::CString::new(body)
                    .map_or(std::ptr::null_mut(), std::ffi::CString::into_raw)
            })
        })
}
