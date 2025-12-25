//! String helpers for runtime

use libc::{c_char, c_void, size_t};
use std::ffi::{CStr, CString};
use std::io::{self, Write};
use std::ptr;
use std::sync::atomic::Ordering;

use crate::header::make_heap_header;
use crate::{RUNTIME_LOG, rc_dec, rc_inc};

#[unsafe(no_mangle)]
/// # Safety
/// Allocates a heap string object. The caller is responsible for managing
/// the lifetime of the returned pointer. The returned pointer must be freed
/// using appropriate reference counting mechanisms.
pub unsafe extern "C" fn heap_str_alloc(str_len: size_t) -> *mut c_void {
    let Some(total_size) = 16usize.checked_add(str_len).and_then(|s| s.checked_add(1)) else {
        if RUNTIME_LOG.load(Ordering::Relaxed) {
            let _ = io::stderr().write_all(
                format!("[oats runtime] heap_str_alloc: integer overflow (str_len={str_len})\n")
                    .as_bytes(),
            );
        }
        return ptr::null_mut();
    };

    let p = crate::runtime_malloc(total_size);
    if p.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let header_ptr_u8 = p.cast::<u8>();
        ptr::write_unaligned(header_ptr_u8.cast::<u64>(), make_heap_header(1));

        let len_ptr_u8 = header_ptr_u8.add(8);
        ptr::write_unaligned(len_ptr_u8.cast::<u64>(), str_len as u64);
    }

    p
}

#[unsafe(no_mangle)]
/// # Safety
/// `s` must be a valid nul-terminated C string. The returned pointer is an
/// owning heap string data pointer and must be released via `rc_dec_str`.
pub unsafe extern "C" fn heap_str_from_cstr(s: *const c_char) -> *mut c_char {
    unsafe {
        if s.is_null() {
            return ptr::null_mut();
        }
        let cstr = CStr::from_ptr(s);
        let bytes = cstr.to_bytes_with_nul();
        let len = bytes.len() - 1;
        let obj = heap_str_alloc(len as size_t);
        if obj.is_null() {
            return ptr::null_mut();
        }
        let data_ptr = obj.cast::<u8>().add(16).cast::<c_char>();
        ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr.cast::<u8>(), bytes.len());
        data_ptr
    }
}

/// # Safety
///
/// `s` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
#[must_use]
pub unsafe fn heap_str_from_cstr_pub(s: *const c_char) -> *mut c_char {
    unsafe { heap_str_from_cstr(s) }
}

/// # Safety
///
/// `data` must be a pointer to string data previously returned by this runtime.
/// The pointer must point to valid string data with the expected memory layout.
#[inline]
#[must_use]
pub const unsafe fn heap_str_to_obj(data: *const c_char) -> *mut c_void {
    if data.is_null() {
        return ptr::null_mut();
    }
    unsafe { (data as *mut u8).sub(16).cast::<c_void>() }
}

#[unsafe(no_mangle)]
/// # Safety
/// `data` must be a pointer previously returned by this runtime for string data
/// (either a static literal or a heap-allocated string). Passing invalid
/// pointers is undefined behavior.
pub unsafe extern "C" fn rc_inc_str(data: *mut c_char) {
    unsafe {
        if data.is_null() {
            return;
        }
        let obj = heap_str_to_obj(data);
        rc_inc(obj);
    }
}

/// # Safety
///
/// `data` must be a pointer previously returned by this runtime for string data
/// (either a static literal or a heap-allocated string). Passing invalid
/// pointers is undefined behavior.
pub unsafe fn rc_inc_str_pub(data: *mut c_char) {
    unsafe { rc_inc_str(data) }
}

#[unsafe(no_mangle)]
/// # Safety
/// `data` must be a pointer previously returned by this runtime for string data
/// (either a static literal or a heap-allocated string). Passing invalid
/// pointers is undefined behavior.
pub unsafe extern "C" fn rc_dec_str(data: *mut c_char) {
    unsafe {
        if data.is_null() {
            return;
        }
        let obj = heap_str_to_obj(data);
        rc_dec(obj);
    }
}

/// # Safety
///
/// `data` must be a pointer previously returned by this runtime for string data
/// (either a static literal or a heap-allocated string). Passing invalid
/// pointers is undefined behavior.
pub unsafe fn rc_dec_str_pub(data: *mut c_char) {
    unsafe { rc_dec_str(data) }
}

/// # Safety
///
/// `s` must be a valid pointer to a null-terminated C string, or null.
/// If non-null, the string must remain valid for the duration of this call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn str_dup(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    let len = unsafe { libc::strlen(s) } + 1;
    let dst = crate::runtime_malloc(len).cast::<c_char>();
    if dst.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        ptr::copy_nonoverlapping(s.cast::<u8>(), dst.cast::<u8>(), len);
    }
    dst
}

/// # Safety
/// `a` and `b` must be valid nul-terminated C string pointers. The returned
/// pointer is an owning C string which should be released with `runtime_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    unsafe {
        if a.is_null() || b.is_null() {
            return ptr::null_mut();
        }
        let la = CStr::from_ptr(a).to_bytes().len();
        let lb = CStr::from_ptr(b).to_bytes().len();

        let Some(total_len) = la.checked_add(lb) else {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] str_concat: integer overflow (len_a={la}, len_b={lb})\n"
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        };

        let obj = heap_str_alloc(total_len);
        if obj.is_null() {
            return ptr::null_mut();
        }

        let data_ptr = obj.cast::<u8>().add(16).cast::<c_char>();

        let aslice = CStr::from_ptr(a).to_bytes();
        let bslice = CStr::from_ptr(b).to_bytes();
        ptr::copy_nonoverlapping(aslice.as_ptr(), data_ptr.cast::<u8>(), la);
        ptr::copy_nonoverlapping(bslice.as_ptr(), data_ptr.add(la).cast::<u8>(), lb);

        *data_ptr.add(la + lb) = 0;

        data_ptr
    }
}

/// Convert number to string.
/// Returns a heap-allocated Oats string.
#[unsafe(no_mangle)]
pub extern "C" fn number_to_string(num: f64) -> *mut c_char {
    let s = if num.is_nan() {
        "NaN".to_string()
    } else if num.is_infinite() {
        if num.is_sign_positive() {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        }
    } else {
        // JavaScript-style formatting for regular numbers
        format_js_number(num)
    };
    let c_str = CString::new(s).unwrap_or_default();
    // SAFETY: c_str.as_ptr() is valid for the duration of this call
    unsafe { heap_str_from_cstr(c_str.as_ptr()) }
}

/// Format a number according to JavaScript's `toString()` behavior
fn format_js_number(num: f64) -> String {
    // Handle special cases first
    if num == 0.0 {
        return "0".to_string();
    }
    if num == -0.0 {
        return "0".to_string(); // JavaScript normalizes -0 to 0
    }

    // Use scientific notation for very large/small numbers
    let abs_num = num.abs();
    if abs_num >= 1e21 || (abs_num < 1e-6 && abs_num > 0.0) {
        return format!("{num:.14e}");
    }

    // For other numbers, use fixed-point notation
    // JavaScript typically shows up to 15 significant digits
    let formatted = if num.fract() == 0.0 {
        // For whole numbers, format as integer
        // Check if number fits in i64 range to avoid truncation
        // i64::MAX = 9223372036854775807, i64::MIN = -9223372036854775808
        const MAX_I64_AS_F64: f64 = 9_223_372_036_854_775_807.0;
        const MIN_I64_AS_F64: f64 = -9_223_372_036_854_775_808.0;
        if (MIN_I64_AS_F64..=MAX_I64_AS_F64).contains(&num) {
            // Safe to convert: number is within i64 range
            // Use truncation which is safe here since we've verified the range
            #[allow(clippy::cast_possible_truncation)]
            let num_i64 = num.trunc() as i64;
            format!("{num_i64}")
        } else {
            // For numbers outside i64 range, format as float without decimal
            format!("{num:.0}")
        }
    } else {
        format!("{num:.15}")
    };

    // Remove trailing zeros after decimal point, but keep at least one digit
    if formatted.contains('.') {
        let trimmed = formatted.trim_end_matches('0').trim_end_matches('.');
        if trimmed.is_empty() {
            "0".to_string()
        } else {
            trimmed.to_string()
        }
    } else {
        formatted
    }
}

/// Check if a number is NaN.
/// Returns 1 if NaN, 0 otherwise.
#[unsafe(no_mangle)]
pub extern "C" fn number_is_nan(num: f64) -> i64 {
    i64::from(num.is_nan())
}

/// Check if a number is finite.
/// Returns 1 if finite, 0 if infinite or NaN.
#[unsafe(no_mangle)]
pub extern "C" fn number_is_finite(num: f64) -> i64 {
    i64::from(num.is_finite())
}

/// Check if a number is infinite.
/// Returns 1 if positive infinity, -1 if negative infinity, 0 otherwise.
#[unsafe(no_mangle)]
pub const extern "C" fn number_is_infinite(num: f64) -> i64 {
    if num.is_infinite() {
        if num.is_sign_positive() { 1 } else { -1 }
    } else {
        0
    }
}

/// Parse a string to a number.
/// Returns NaN if parsing fails.
///
/// # Safety
///
/// `str_ptr` must be a valid pointer to a null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn number_parse_float(str_ptr: *const c_char) -> f64 {
    // Helper function to convert u64 to f64 with proper handling of precision loss
    // f64 can exactly represent integers up to 2^53 (9007199254740992)
    // For values <= 2^53, the conversion is exact (no precision loss)
    // For larger values, precision loss is unavoidable but we round to nearest representable value
    #[allow(clippy::cast_precision_loss, clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    fn u64_to_f64_safe(n: u64) -> f64 {
        // Helper function to convert u64 to f64 with proper handling of precision loss
        // f64 can exactly represent integers up to 2^53 (9007199254740992)
        // For values <= 2^53, the conversion is exact (no precision loss)
        // For larger values, precision loss is unavoidable but we round to nearest representable value
        const MAX_EXACT_F64_U64: u64 = 1u64 << 53; // 2^53 = 9007199254740992
        // Convert u64 to f64
        if n <= MAX_EXACT_F64_U64 {
            // Exact conversion for values <= 2^53
            // This conversion is lossless for integers in this range
            let result = n as f64;
            // Verify the conversion was exact (sanity check)
            let check_value = result as u64;
            debug_assert_eq!(
                check_value, n,
                "u64 to f64 conversion should be exact for n <= 2^53"
            );
            result
        } else {
            // For values > 2^53, precision loss is unavoidable
            // This rounds to the nearest representable f64 value
            n as f64
        }
    }

    if str_ptr.is_null() {
        return f64::NAN;
    }

    // SAFETY: The string is assumed to be valid UTF-8 as per Oats string encoding
    let c_str = unsafe { CStr::from_ptr(str_ptr) };
    let Ok(str_slice) = c_str.to_str() else {
        return f64::NAN;
    };

    // Handle hexadecimal notation (0x, 0X)
    if str_slice.starts_with("0x") || str_slice.starts_with("0X") {
        // Parse as hex and convert to f64
        u64::from_str_radix(&str_slice[2..], 16).map_or(f64::NAN, u64_to_f64_safe)
    } else if str_slice.starts_with("0o") || str_slice.starts_with("0O") {
        // Parse as octal and convert to f64
        u64::from_str_radix(&str_slice[2..], 8).map_or(f64::NAN, u64_to_f64_safe)
    } else if str_slice.starts_with("0b") || str_slice.starts_with("0B") {
        // Parse as binary and convert to f64
        u64::from_str_radix(&str_slice[2..], 2).map_or(f64::NAN, u64_to_f64_safe)
    } else {
        // Parse as decimal (float)
        str_slice.parse::<f64>().unwrap_or(f64::NAN)
    }
}
