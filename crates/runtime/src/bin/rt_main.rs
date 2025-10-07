// Small host object that calls a uniform entrypoint emitted by the AOT
// generator. The host does not need to know the script's function
// signature; it simply calls `oats_entry()` which the AOT module provides as
// a void, no-argument wrapper around whatever `oats_main` the user exported.

use std::ffi::CString;
use libc::c_void;

#[cfg(feature = "link_entry")]
unsafe extern "C" {
    // When the feature is enabled, `oats_entry` must be provided at link time
    // by the AOT module or another object. This allows a direct call without
    // runtime symbol lookup.
    fn oats_entry() -> i32;
}

fn main() {
    #[cfg(feature = "link_entry")]
    unsafe {
        let code = oats_entry();
        std::process::exit(code);
    }

    #[cfg(not(feature = "link_entry"))]
    {
        // Look up "oats_entry" dynamically so this host binary can be built
        // and linked even if no AOT module (providing oats_entry) is present.
        // If the symbol is present, call it and exit with its returned code.
        // Otherwise exit 0.
        let name = CString::new("oats_entry").expect("CString::new failed");
        unsafe {
            let sym = libc::dlsym(libc::RTLD_DEFAULT, name.as_ptr()) as *mut c_void;
            if sym.is_null() {
                std::process::exit(0);
            }
            // Cast to function pointer and call
            let f: extern "C" fn() -> i32 = std::mem::transmute(sym);
            let code = f();
            std::process::exit(code);
        }
    }
}
