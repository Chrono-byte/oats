//! Minimal primitives crate exposing C ABI symbols used by generated code
//!
//! These functions are thin wrappers around the `runtime` crate and provide
//! the C ABI that the code generator expects. They were previously part of
//! `crates/std` and are being moved here to form a stable primitives layer.

use libc::c_char;
use std::ffi::CStr;
use std::io::{self, Write};

/// Print a C string followed by a newline
///
/// # Safety
/// `s` must be a valid nul-terminated C string pointer or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        // Validate pointer before creating CStr to avoid undefined behavior
        // Note: We can't use runtime::is_plausible_addr here as this is a separate crate
        // Basic validation: check pointer is not in obviously invalid ranges
        let addr = s as usize;
        #[cfg(target_pointer_width = "64")]
        {
            const LOW_BOUND: usize = 0x10000; // 64 KiB
            const HIGH_BOUND: usize = 0x0001_0000_0000_0000; // 2^48
            if !(LOW_BOUND..HIGH_BOUND).contains(&addr) || (addr & 7) != 0 {
                // Invalid address range or not aligned - skip printing
                return;
            }
        }
        #[cfg(target_pointer_width = "32")]
        {
            const LOW_BOUND: usize = 0x10000; // 64 KiB
            if addr < LOW_BOUND || (addr & 7) != 0 {
                return;
            }
        }
        // CStr::from_ptr can still fail if string is not null-terminated, but we've validated the pointer
        let cstr = CStr::from_ptr(s);
        // Use runtime-safe println via libc stdout
        let _ = io::stdout().write_all(cstr.to_bytes());
        let _ = io::stdout().write_all(b"\n");
    }
}

/// Print a f64 followed by a newline
#[unsafe(no_mangle)]
pub extern "C" fn print_f64(v: f64) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
}

/// Print an i32 followed by a newline
#[unsafe(no_mangle)]
pub extern "C" fn print_i32(v: i32) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
}

/// Print a C string without newline
///
/// # Safety
/// `s` must be a valid nul-terminated C string pointer or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str_no_nl(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        // Validate pointer before creating CStr to avoid undefined behavior
        let addr = s as usize;
        #[cfg(target_pointer_width = "64")]
        {
            const LOW_BOUND: usize = 0x10000; // 64 KiB
            const HIGH_BOUND: usize = 0x0001_0000_0000_0000; // 2^48
            if !(LOW_BOUND..HIGH_BOUND).contains(&addr) || (addr & 7) != 0 {
                return;
            }
        }
        #[cfg(target_pointer_width = "32")]
        {
            const LOW_BOUND: usize = 0x10000; // 64 KiB
            if addr < LOW_BOUND || (addr & 7) != 0 {
                return;
            }
        }
        let cstr = CStr::from_ptr(s);
        let _ = io::stdout().write_all(cstr.to_bytes());
    }
}

/// Print a f64 without newline
#[unsafe(no_mangle)]
pub extern "C" fn print_f64_no_nl(v: f64) {
    let _ = io::stdout().write_all(format!("{}", v).as_bytes());
    let _ = io::stdout().flush();
}

/// Print a newline
#[unsafe(no_mangle)]
pub extern "C" fn print_newline() {
    let _ = io::stdout().write_all(b"\n");
}

/// Sleep for the given number of milliseconds
#[unsafe(no_mangle)]
pub extern "C" fn sleep_ms(ms: f64) {
    if ms <= 0.0 {
        return;
    }
    let mut ms_clamped = ms;
    if ms_clamped > 10_000.0 {
        ms_clamped = 10_000.0; // cap at 10s
    }
    let dur = std::time::Duration::from_millis(ms_clamped as u64);
    std::thread::sleep(dur);
}

/// Convert a number to a string
#[allow(clippy::manual_c_str_literals)]
#[unsafe(no_mangle)]
pub extern "C" fn number_to_string(num: f64) -> *mut c_char {
    let s = format!("{}", num);
    let c = std::ffi::CString::new(s).unwrap_or_default();
    // Use runtime helper to create a heap string and return its data pointer
    (unsafe { runtime::string::heap_str_from_cstr_pub(c.as_ptr()) }) as *mut c_char
}
