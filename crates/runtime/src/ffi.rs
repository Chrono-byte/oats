//! C ABI exports for the runtime. This module centralizes all C-callable
//! symbols (#[no_mangle] extern "C" functions) so the public ABI is easy
//! to audit and maintain.

use libc::{c_char, size_t};
use std::ffi::{CStr, CString};
use std::io::{self, Write};

use crate::*;

// Small usage anchor for the module during staged refactors. Calling
// `crate::ffi::ffi_init()` from the crate root ensures the `ffi` module
// is considered used and avoids `unused_imports` warnings while we
// incrementally move symbols out of the monolith.
#[allow(dead_code)]
pub(crate) fn ffi_init() {}

// Re-exported from this module by `lib.rs`.

/// Initialize the background collector (idempotent).
#[unsafe(no_mangle)]
pub extern "C" fn collector_init() {
    let _ = init_collector();
}

#[unsafe(no_mangle)]
pub extern "C" fn collector_shutdown() {
    let c = init_collector();
    c.stop();
}

#[unsafe(no_mangle)]
pub extern "C" fn collector_collect_now() {
    let c = init_collector();
    let roots = c.drain_now();
    crate::collector::Collector::process_roots(&roots);
}

// collector_test_enqueue is provided under the feature gate by the original
// file; keep it here as well.
#[cfg(feature = "collector-test")]
#[unsafe(no_mangle)]
pub extern "C" fn collector_test_enqueue() {
    // body moved from original lib.rs
    let size = std::mem::size_of::<u64>() * 2;
    let mem = runtime_malloc(size as size_t) as *mut u8;
    if mem.is_null() {
        return;
    }
    unsafe {
        let header_ptr = mem as *mut u64;
        *header_ptr = make_heap_header(1);
        let second = mem.add(std::mem::size_of::<u64>()) as *mut u64;
        *second = 0u64;
    }
    add_root_candidate(mem as *mut c_void);
}

/// # Safety
/// `s` must be a valid nul-terminated C string pointer or null.
#[unsafe(no_mangle)]
pub unsafe fn runtime_strlen(s: *const c_char) -> size_t {
    if s.is_null() {
        return 0;
    }
    let c = unsafe { CStr::from_ptr(s) };
    c.to_bytes().len() as size_t
}

#[unsafe(no_mangle)]
pub extern "C" fn print_f64(v: f64) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
    let _ = io::stdout().flush();
}

/// # Safety
/// `s` must be a valid nul-terminated C string pointer or null. Passing
/// invalid pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        let _ = io::stdout().write_all(CStr::from_ptr(s).to_bytes());
        let _ = io::stdout().write_all(b"\n");
        let _ = io::stdout().flush();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn print_i32(v: i32) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
    let _ = io::stdout().flush();
}

#[unsafe(no_mangle)]
pub extern "C" fn print_f64_no_nl(v: f64) {
    let _ = io::stdout().write_all(format!("{}", v).as_bytes());
    let _ = io::stdout().flush();
}

/// # Safety
/// `s` must be a valid nul-terminated C string pointer or null. Passing
/// invalid pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str_no_nl(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        let _ = io::stdout().write_all(CStr::from_ptr(s).to_bytes());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn print_newline() {
    let _ = io::stdout().write_all(b"\n");
    let _ = io::stdout().flush();
}

#[allow(clippy::manual_c_str_literals)]
#[unsafe(no_mangle)]
pub extern "C" fn number_to_string(num: f64) -> *mut c_char {
    let s = format!("{}", num);
    let c = CString::new(s).unwrap_or_default();
    unsafe { heap_str_from_cstr(c.as_ptr()) }
}
