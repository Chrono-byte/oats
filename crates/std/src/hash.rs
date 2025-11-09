//! Hashing operations

use libc::c_char;
use std::ffi::{CStr, CString};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// Compute hash of a string (returns hash as u64)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_hash_string(str: *const c_char) -> u64 {
    if str.is_null() {
        return 0;
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return 0,
    };

    let mut hasher = DefaultHasher::new();
    str_val.hash(&mut hasher);
    hasher.finish()
}

/// Compute hash of bytes (returns hash as u64)
///
/// # Safety
///
/// `bytes` must be a valid pointer to at least `len` bytes of readable memory.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_hash_bytes(bytes: *const u8, len: usize) -> u64 {
    if bytes.is_null() || len == 0 {
        return 0;
    }

    let slice = unsafe { std::slice::from_raw_parts(bytes, len) };
    let mut hasher = DefaultHasher::new();
    slice.hash(&mut hasher);
    hasher.finish()
}

/// Compute hash of a number (returns hash as u64)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_hash_f64(value: f64) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.to_bits().hash(&mut hasher);
    hasher.finish()
}

/// Compute hash of an integer (returns hash as u64)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_hash_i64(value: i64) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

/// Compute SHA-256 hash of a string (returns hex string, caller must free)
///
/// # Safety
///
/// `str` must be a valid pointer to a null-terminated C string, or null.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_hash_sha256_string(str: *const c_char) -> *mut c_char {
    if str.is_null() {
        return std::ptr::null_mut();
    }

    let str_val = match unsafe { CStr::from_ptr(str).to_str() } {
        Ok(s) => s,
        Err(_) => return std::ptr::null_mut(),
    };

    use sha2::{Sha256, Digest};
    let mut hasher = Sha256::new();
    hasher.update(str_val.as_bytes());
    let result = hasher.finalize();
    let hex = hex::encode(result);

    match CString::new(hex) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Compute SHA-256 hash of bytes (returns hex string, caller must free)
///
/// # Safety
///
/// `bytes` must be a valid pointer to at least `len` bytes of readable memory.
/// #[oats_export]
#[no_mangle]
pub unsafe extern "C" fn oats_std_hash_sha256_bytes(
    bytes: *const u8,
    len: usize,
) -> *mut c_char {
    if bytes.is_null() || len == 0 {
        return std::ptr::null_mut();
    }

    let slice = unsafe { std::slice::from_raw_parts(bytes, len) };
    use sha2::{Sha256, Digest};
    let mut hasher = Sha256::new();
    hasher.update(slice);
    let result = hasher.finalize();
    let hex = hex::encode(result);

    match CString::new(hex) {
        Ok(cstring) => cstring.into_raw(),
        Err(_) => std::ptr::null_mut(),
    }
}

