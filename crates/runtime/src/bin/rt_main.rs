// Small host object that calls a uniform entrypoint emitted by the AOT
// generator. The host does not need to know the script's function
// signature; it simply calls `oats_entry()` which the AOT module provides as
// a void, no-argument wrapper around whatever `oats_main` the user exported.


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
        // The unsafe operations (dlsym + transmute) are isolated to a
        // single helper below which returns an Option containing the
        // exit code if the symbol is present.

        // Helper: attempt to locate and call `oats_entry`. All unsafe
        // interaction with the dynamic loader is contained here.
        fn try_call_oats_entry() -> Option<i32> {
            let name = std::ffi::CString::new("oats_entry").unwrap();
            unsafe {
                // Clear any existing dlerror state before lookup.

                use std::ffi::c_void;
                libc::dlerror();
                let sym = libc::dlsym(libc::RTLD_DEFAULT, name.as_ptr()) as *mut c_void;
                if sym.is_null() {
                    return None;
                }
                // SAFETY: `sym` is non-null and (if provided) should point to a
                // function with the signature `extern "C" fn() -> i32`.
                // We perform the minimal unsafe transmute here and call the
                // function immediately.
                let f = std::mem::transmute::<*mut c_void, extern "C" fn() -> i32>(sym);
                Some(f())
            }
        }

        if let Some(code) = try_call_oats_entry() {
            std::process::exit(code);
        }
        // No symbol found -> exit 0
        std::process::exit(0);
    }
}
