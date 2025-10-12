//! Runtime host binary for executing Oats AOT-compiled programs.
//!
//! This binary serves as the entry point for Oats programs compiled to native
//! code. It provides a uniform interface for calling the AOT-generated `oats_entry`
//! function, supporting both static linking and dynamic symbol resolution modes.
//!
//! # Compilation Modes
//!
//! The binary supports two distinct compilation and execution modes:
//!
//! ## Static Linking Mode (`--features link_entry`)
//! - Directly calls the `oats_entry` function at link time
//! - Requires the symbol to be resolved during the linking phase
//! - Provides maximum performance with no runtime symbol lookup overhead
//!
//! ## Dynamic Resolution Mode (default)
//! - Uses `dlsym` to resolve `oats_entry` at runtime
//! - Gracefully handles missing symbols by exiting with code 0
//! - Enables flexible deployment without requiring symbol resolution at link time
//!
//! # Entry Point Contract
//!
//! All Oats programs must provide an `oats_entry` function with the signature:
//! ```c
//! extern "C" fn oats_entry() -> i32
//! ```
//!
//! The return value is used as the process exit code, following standard
//! Unix conventions where 0 indicates success and non-zero indicates failure.

#[cfg(feature = "link_entry")]
unsafe extern "C" {
    fn oats_entry() -> i32;
}

/// Attempts to dynamically resolve and call the `oats_entry` symbol.
///
/// This function provides runtime symbol resolution for the default compilation
/// mode, allowing the binary to gracefully handle cases where the AOT-compiled
/// program is not available or doesn't export the expected entry point.
///
/// # Returns
/// The exit code from `oats_entry` if the symbol is found and callable,
/// or `None` if the symbol cannot be resolved
///
/// # Safety
/// This function uses unsafe operations to perform symbol lookup and function
/// pointer transmutation. The caller is responsible for ensuring that any
/// resolved symbol matches the expected `extern "C" fn() -> i32` signature.
#[cfg(not(feature = "link_entry"))]
fn try_call_oats_entry() -> Option<i32> {
    let name = std::ffi::CString::new("oats_entry").unwrap();
    unsafe {
        use std::ffi::c_void;
        libc::dlerror();
        let sym = libc::dlsym(libc::RTLD_DEFAULT, name.as_ptr());
        if sym.is_null() {
            return None;
        }
        // SAFETY: The resolved symbol must match the `extern "C" fn() -> i32` signature.
        // This contract is enforced by the Oats compiler during code generation.
        let f = std::mem::transmute::<*mut c_void, extern "C" fn() -> i32>(sym);
        Some(f())
    }
}

fn main() {
    #[cfg(feature = "link_entry")]
    unsafe {
        std::process::exit(oats_entry());
    }

    #[cfg(not(feature = "link_entry"))]
    {
        if let Some(code) = try_call_oats_entry() {
            std::process::exit(code);
        }
        // Exit successfully if no oats_entry symbol is found (graceful degradation)
        std::process::exit(0);
    }
}
