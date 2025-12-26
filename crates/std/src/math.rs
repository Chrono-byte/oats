//! Mathematical operations

use libc::c_double;

/// Calculate square root
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_sqrt(x: c_double) -> c_double {
    x.sqrt()
}

/// Calculate power (x^y)
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_pow(x: c_double, y: c_double) -> c_double {
    x.powf(y)
}

/// Calculate natural logarithm
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_ln(x: c_double) -> c_double {
    x.ln()
}

/// Calculate base-10 logarithm
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_log10(x: c_double) -> c_double {
    x.log10()
}

/// Calculate sine
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_sin(x: c_double) -> c_double {
    x.sin()
}

/// Calculate cosine
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_cos(x: c_double) -> c_double {
    x.cos()
}

/// Calculate tangent
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_tan(x: c_double) -> c_double {
    x.tan()
}

/// Calculate arcsine
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_asin(x: c_double) -> c_double {
    x.asin()
}

/// Calculate arccosine
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_acos(x: c_double) -> c_double {
    x.acos()
}

/// Calculate arctangent
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_atan(x: c_double) -> c_double {
    x.atan()
}

/// Calculate arctangent of y/x
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_atan2(y: c_double, x: c_double) -> c_double {
    y.atan2(x)
}

/// Calculate exponential (e^x)
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_exp(x: c_double) -> c_double {
    x.exp()
}

/// Calculate absolute value
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_abs(x: c_double) -> c_double {
    x.abs()
}

/// Calculate ceiling
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_ceil(x: c_double) -> c_double {
    x.ceil()
}

/// Calculate floor
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_floor(x: c_double) -> c_double {
    x.floor()
}

/// Round to nearest integer
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_round(x: c_double) -> c_double {
    x.round()
}

/// Get maximum of two values
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_max(a: c_double, b: c_double) -> c_double {
    a.max(b)
}

/// Get minimum of two values
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_min(a: c_double, b: c_double) -> c_double {
    a.min(b)
}

/// Calculate hyperbolic sine
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_sinh(x: c_double) -> c_double {
    x.sinh()
}

/// Calculate hyperbolic cosine
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_cosh(x: c_double) -> c_double {
    x.cosh()
}

/// Calculate hyperbolic tangent
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_tanh(x: c_double) -> c_double {
    x.tanh()
}

/// Get value of π
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_pi() -> c_double {
    std::f64::consts::PI
}

/// Get value of e
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_e() -> c_double {
    std::f64::consts::E
}

/// Get value of LN2 (natural log of 2)
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_ln2() -> c_double {
    std::f64::consts::LN_2
}

/// Get value of LN10 (natural log of 10)
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_ln10() -> c_double {
    std::f64::consts::LN_10
}

/// Get value of LOG2E (log base 2 of E)
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_log2e() -> c_double {
    std::f64::consts::LOG2_E
}

/// Get value of LOG10E (log base 10 of E)
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_log10e() -> c_double {
    std::f64::consts::LOG10_E
}

/// Get value of SQRT2 (square root of 2)
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_sqrt2() -> c_double {
    std::f64::consts::SQRT_2
}

/// Get value of `SQRT1_2` (square root of 1/2)
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_sqrt1_2() -> c_double {
    std::f64::consts::FRAC_1_SQRT_2
}

/// Get sign of number (-1, 0, or 1)
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_sign(x: c_double) -> c_double {
    if x.is_nan() {
        return f64::NAN;
    }
    if x > 0.0 {
        1.0
    } else if x < 0.0 {
        -1.0
    } else {
        0.0
    }
}

/// Calculate cube root
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_cbrt(x: c_double) -> c_double {
    x.cbrt()
}

/// Calculate hypotenuse (sqrt(x^2 + y^2))
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_hypot(x: c_double, y: c_double) -> c_double {
    x.hypot(y)
}

/// Integer multiplication (32-bit)
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_imul(a: i32, b: i32) -> i32 {
    a.wrapping_mul(b)
}

/// Round to nearest float32
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_fround(x: c_double) -> c_double {
    x as f32 as f64
}

/// Truncate to integer
/// #[`oats_export`]
#[no_mangle]
pub extern "C" fn oats_std_math_trunc(x: c_double) -> c_double {
    x.trunc()
}

/// Integer division
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_div(a: i64, b: i64) -> i64 {
    if b == 0 {
        return 0;
    }
    a / b
}

/// Integer modulo
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_mod(a: i64, b: i64) -> i64 {
    if b == 0 {
        return 0;
    }
    a % b
}

/// Bitwise AND
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_bit_and(a: i64, b: i64) -> i64 {
    a & b
}

/// Bitwise OR
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_bit_or(a: i64, b: i64) -> i64 {
    a | b
}

/// Bitwise XOR
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_bit_xor(a: i64, b: i64) -> i64 {
    a ^ b
}

/// Bitwise NOT
/// #[`oats_export`]
#[no_mangle]
pub const extern "C" fn oats_std_math_bit_not(a: i64) -> i64 {
    !a
}

/// Left shift
/// #[`oats_export`]
///
/// # Panics
///
/// Panics if the shift amount cannot be converted to u32 (which should never happen
/// due to clamping).
#[no_mangle]
pub extern "C" fn oats_std_math_shl(a: i64, b: i64) -> i64 {
    let shift_amount = b.clamp(0, 63);
    // shift_amount is guaranteed to be 0 <= shift_amount <= 63 due to clamp(0, 63)
    let shift = shift_amount as u32;
    a << shift
}

/// Right shift
/// #[`oats_export`]
///
/// # Panics
///
/// Panics if the shift amount cannot be converted to u32 (which should never happen
/// due to clamping).
#[no_mangle]
pub extern "C" fn oats_std_math_shr(a: i64, b: i64) -> i64 {
    let shift_amount = b.clamp(0, 63);
    // shift_amount is guaranteed to be 0 <= shift_amount <= 63 due to clamp(0, 63)
    let shift = shift_amount as u32;
    a >> shift
}
