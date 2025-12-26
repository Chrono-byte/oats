//! Number-related runtime functions
//!
//! This module implements JavaScript Number object functionality including
//! static properties, static methods, and instance methods.

use libc::c_char;
use std::ffi::CStr;

use crate::string::{number_parse_float, number_to_string};

/// Safely convert a clamped f64 to usize for formatting precision values
/// Values must be in range 0.0..=21.0 (clamped)
fn clamped_f64_to_usize(value: f64) -> usize {
    // Values are guaranteed to be in 0.0..=21.0 range due to clamping
    // Match on exact values to avoid any casting
    match value {
        v if (v - v.round()).abs() < f64::EPSILON => {
            // Value is effectively an integer
            let int_val = v.round();
            match int_val {
                0.0 => 0,
                1.0 => 1,
                2.0 => 2,
                3.0 => 3,
                4.0 => 4,
                5.0 => 5,
                6.0 => 6,
                7.0 => 7,
                8.0 => 8,
                9.0 => 9,
                10.0 => 10,
                11.0 => 11,
                12.0 => 12,
                13.0 => 13,
                14.0 => 14,
                15.0 => 15,
                16.0 => 16,
                17.0 => 17,
                18.0 => 18,
                19.0 => 19,
                20.0 => 20,
                21.0 => 21,
                _ => 0, // Shouldn't happen with clamping
            }
        }
        _ => 0, // Non-integer values default to 0
    }
}

/// Number.EPSILON - The smallest interval between two representable numbers
#[unsafe(no_mangle)]
pub const extern "C" fn number_epsilon() -> f64 {
    f64::EPSILON
}

/// `Number.MAX_SAFE_INTEGER` - The maximum safe integer in JavaScript (2^53 - 1)
#[unsafe(no_mangle)]
pub const extern "C" fn number_max_safe_integer() -> f64 {
    9_007_199_254_740_991.0 // 2^53 - 1
}

/// `Number.MAX_VALUE` - The largest positive representable number
#[unsafe(no_mangle)]
pub const extern "C" fn number_max_value() -> f64 {
    f64::MAX
}

/// `Number.MIN_SAFE_INTEGER` - The minimum safe integer in JavaScript (-(2^53 - 1))
#[unsafe(no_mangle)]
pub const extern "C" fn number_min_safe_integer() -> f64 {
    -9_007_199_254_740_991.0 // -(2^53 - 1)
}

/// `Number.MIN_VALUE` - The smallest positive representable number
#[unsafe(no_mangle)]
pub const extern "C" fn number_min_value() -> f64 {
    f64::MIN_POSITIVE
}

/// Number.NaN - Special "Not a Number" value
#[unsafe(no_mangle)]
pub const extern "C" fn number_nan() -> f64 {
    f64::NAN
}

/// `Number.NEGATIVE_INFINITY` - Special negative infinity value
#[unsafe(no_mangle)]
pub const extern "C" fn number_negative_infinity() -> f64 {
    f64::NEG_INFINITY
}

/// `Number.POSITIVE_INFINITY` - Special positive infinity value
#[unsafe(no_mangle)]
pub const extern "C" fn number_positive_infinity() -> f64 {
    f64::INFINITY
}

/// Number.isSafeInteger - Check if a number is a safe integer
/// Returns 1 if the number is a safe integer, 0 otherwise
#[unsafe(no_mangle)]
pub extern "C" fn number_is_safe_integer(num: f64) -> i64 {
    i64::from(
        num.is_finite()
            && num.fract() == 0.0
            && num >= number_min_safe_integer()
            && num <= number_max_safe_integer(),
    )
}

/// Number.isNaN static method - Check if value is NaN (same as global isNaN)
/// Returns 1 if NaN, 0 otherwise
#[unsafe(no_mangle)]
pub extern "C" fn number_is_nan_static(num: f64) -> i64 {
    i64::from(num.is_nan())
}

/// Number.isFinite static method - Check if value is finite
/// Returns 1 if finite, 0 if infinite or NaN
#[unsafe(no_mangle)]
pub extern "C" fn number_is_finite_static(num: f64) -> i64 {
    i64::from(num.is_finite())
}

/// Number.isInteger static method - Check if value is an integer
/// Returns 1 if the number is an integer, 0 otherwise
#[unsafe(no_mangle)]
pub extern "C" fn number_is_integer(num: f64) -> i64 {
    i64::from(num.is_finite() && num.fract() == 0.0)
}

/// Number.parseFloat static method - Parse string to float
/// Returns NaN if parsing fails
///
/// # Safety
/// `str_ptr` must be a valid pointer to a null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn number_parse_float_static(str_ptr: *const c_char) -> f64 {
    unsafe { number_parse_float(str_ptr) }
}

/// Number.parseInt static method - Parse string to integer with optional radix
/// Returns NaN if parsing fails
///
/// # Safety
/// `str_ptr` must be a valid pointer to a null-terminated C string.
/// `radix` must be between 2 and 36 inclusive.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn number_parse_int(str_ptr: *const c_char, radix: f64) -> f64 {
    if str_ptr.is_null() {
        return f64::NAN;
    }

    // Convert radix to integer, defaulting to 10
    // If radix is NaN, has a fractional part, or is out of valid range, default to 10
    let radix_int = if radix.is_nan() || radix.fract() != 0.0 {
        10
    } else {
        match radix {
            2.0 => 2,
            3.0 => 3,
            4.0 => 4,
            5.0 => 5,
            6.0 => 6,
            7.0 => 7,
            8.0 => 8,
            9.0 => 9,
            10.0 => 10,
            11.0 => 11,
            12.0 => 12,
            13.0 => 13,
            14.0 => 14,
            15.0 => 15,
            16.0 => 16,
            17.0 => 17,
            18.0 => 18,
            19.0 => 19,
            20.0 => 20,
            21.0 => 21,
            22.0 => 22,
            23.0 => 23,
            24.0 => 24,
            25.0 => 25,
            26.0 => 26,
            27.0 => 27,
            28.0 => 28,
            29.0 => 29,
            30.0 => 30,
            31.0 => 31,
            32.0 => 32,
            33.0 => 33,
            34.0 => 34,
            35.0 => 35,
            36.0 => 36,
            _ => 10, // Out of valid range
        }
    };

    // Validate radix range (2-36)
    if !(2..=36).contains(&radix_int) {
        return f64::NAN;
    }

    // SAFETY: The string is assumed to be valid UTF-8 as per Oats string encoding
    let c_str = unsafe { CStr::from_ptr(str_ptr) };
    let Ok(str_slice) = c_str.to_str() else {
        return f64::NAN;
    };

    // Trim whitespace from start and end
    let trimmed = str_slice.trim();

    if trimmed.is_empty() {
        return f64::NAN;
    }

    // Handle sign
    let (sign, rest) = trimmed.strip_prefix('-').map_or_else(
        || {
            trimmed
                .strip_prefix('+')
                .map_or((1, trimmed), |stripped| (1, stripped))
        },
        |stripped| (-1, stripped),
    );

    // Parse the number digit by digit according to radix
    // JavaScript parseInt can return numbers outside safe integer range
    let mut result: f64 = 0.0;
    let mut has_digits = false;

    for ch in rest.chars() {
        let digit_value = match ch {
            '0'..='9' => (ch as u32) - ('0' as u32),
            'a'..='z' => (ch as u32) - ('a' as u32) + 10,
            'A'..='Z' => (ch as u32) - ('A' as u32) + 10,
            _ => break, // Stop at first invalid character
        };

        if digit_value >= radix_int {
            break; // Invalid digit for this radix
        }

        result = f64::from(radix_int).mul_add(result, f64::from(digit_value));
        has_digits = true;

        // Check for overflow (similar to JavaScript behavior)
        if !result.is_finite() {
            return f64::INFINITY * f64::from(sign);
        }
    }

    if !has_digits {
        return f64::NAN;
    }

    result * f64::from(sign)
}

/// `Number.prototype.toFixed` - Format number with fixed decimal places
/// Returns a heap-allocated string
///
/// # Safety
/// The returned pointer must be freed with `rc_dec_str`
#[unsafe(no_mangle)]
pub extern "C" fn number_to_fixed(num: f64, fraction_digits: f64) -> *mut c_char {
    // Convert fraction_digits to usize, defaulting to 0
    let digits = if fraction_digits.is_nan() {
        0
    } else {
        clamped_f64_to_usize(fraction_digits.clamp(0.0, 20.0))
    };

    // Format the number
    let formatted = if num.is_nan() {
        "NaN".to_string()
    } else if num.is_infinite() {
        if num.is_sign_positive() {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        }
    } else {
        format!("{num:.digits$}")
    };

    // Convert to C string
    let c_str = std::ffi::CString::new(formatted).unwrap_or_default();
    unsafe { crate::heap_str_from_cstr(c_str.as_ptr()) }
}

/// `Number.prototype.toExponential` - Format number in exponential notation
/// Returns a heap-allocated string
///
/// # Safety
/// The returned pointer must be freed with `rc_dec_str`
#[unsafe(no_mangle)]
pub extern "C" fn number_to_exponential(num: f64, fraction_digits: f64) -> *mut c_char {
    // Convert fraction_digits to usize if provided
    let digits = if fraction_digits.is_nan() || !(0.0..=20.0).contains(&fraction_digits) {
        None
    } else {
        Some(clamped_f64_to_usize(fraction_digits.clamp(0.0, 20.0))) // Safe: already validated to be in 0..=20 range
    };

    // Format the number
    let formatted = if num.is_nan() {
        "NaN".to_string()
    } else if num.is_infinite() {
        if num.is_sign_positive() {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        }
    } else {
        digits.map_or_else(|| format!("{num:.e}"), |d| format!("{num:.d$e}"))
    };

    // Convert to C string
    let c_str = std::ffi::CString::new(formatted).unwrap_or_default();
    unsafe { crate::heap_str_from_cstr(c_str.as_ptr()) }
}

/// `Number.prototype.toPrecision` - Format number with specified precision
/// Returns a heap-allocated string
///
/// # Safety
/// The returned pointer must be freed with `rc_dec_str`
#[unsafe(no_mangle)]
pub extern "C" fn number_to_precision(num: f64, precision: f64) -> *mut c_char {
    // Convert precision to usize
    let prec = if precision.is_nan() || !(1.0..=21.0).contains(&precision) {
        None
    } else {
        Some(clamped_f64_to_usize(precision.clamp(1.0, 21.0))) // Safe: already validated to be in 1..=21 range
    };

    // Format the number
    let formatted = if num.is_nan() {
        "NaN".to_string()
    } else if num.is_infinite() {
        if num.is_sign_positive() {
            "Infinity".to_string()
        } else {
            "-Infinity".to_string()
        }
    } else {
        prec.map_or_else(
            || {
                // Default precision behavior
                if num.fract() == 0.0 {
                    format!("{num:.0}")
                } else {
                    format!("{num}")
                }
            },
            |p| {
                // Use scientific notation for very large/small numbers
                let abs_num = num.abs();
                if abs_num >= 1e21 || (abs_num < 1e-6 && abs_num > 0.0) {
                    format!("{num:.p$e}")
                } else {
                    // For other numbers, format with the specified precision
                    let formatted_str = format!("{num:.p$}");
                    // Remove trailing zeros after decimal point
                    if formatted_str.contains('.') {
                        formatted_str
                            .trim_end_matches('0')
                            .trim_end_matches('.')
                            .to_string()
                    } else {
                        formatted_str
                    }
                }
            },
        )
    };

    // Convert to C string
    let c_str = std::ffi::CString::new(formatted).unwrap_or_default();
    unsafe { crate::heap_str_from_cstr(c_str.as_ptr()) }
}

/// Number.prototype.toLocaleString - Format number for locale
/// For now, this is the same as toString (English locale)
/// Returns a heap-allocated string
///
/// # Safety
/// The returned pointer must be freed with `rc_dec_str`
#[unsafe(no_mangle)]
pub extern "C" fn number_to_locale_string(num: f64) -> *mut c_char {
    // For now, use the same implementation as toString
    // In a full implementation, this would respect locale settings
    number_to_string(num)
}

/// `Number.prototype.valueOf` - Return the primitive number value
/// This is essentially a no-op since numbers are already primitives
#[unsafe(no_mangle)]
#[allow(clippy::missing_const_for_fn)]
pub extern "C" fn number_value_of(num: f64) -> f64 {
    num
}

/// Create a new Number object (boxing)
/// Returns a union-boxed f64
#[unsafe(no_mangle)]
pub extern "C" fn number_new(value: f64) -> *mut std::ffi::c_void {
    crate::union_box_f64(value)
}

/// Get the value of a Number object
/// Unboxes a union value to get the f64
///
/// # Safety
/// `num_obj` must be a valid union object containing a number
#[unsafe(no_mangle)]
pub extern "C" fn number_value(num_obj: *mut std::ffi::c_void) -> f64 {
    crate::union_unbox_f64(num_obj)
}
