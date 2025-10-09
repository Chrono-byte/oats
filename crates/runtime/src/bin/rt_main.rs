// Small host binary that calls a uniform AOT entrypoint `oats_entry`.
// Two modes are supported:
// - link time: when built with `--features link_entry` we call the linked
//   `oats_entry()` directly.
// - dynamic: otherwise we attempt to resolve `oats_entry` via dlsym and call
//   it if present; if not present we exit zero.

#[cfg(feature = "link_entry")]
unsafe extern "C" {
    fn oats_entry() -> i32;
}

// Dynamic lookup helper (kept minimal and unsafe). Only compiled in the
// non-linking build.
#[cfg(not(feature = "link_entry"))]
fn try_call_oats_entry() -> Option<i32> {
    let name = std::ffi::CString::new("oats_entry").unwrap();
    unsafe {
        use std::ffi::c_void;
        libc::dlerror();
        let sym = libc::dlsym(libc::RTLD_DEFAULT, name.as_ptr()) as *mut c_void;
        if sym.is_null() {
            return None;
        }
        // SAFETY: caller-provided symbol should match `extern "C" fn() -> i32`.
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
        std::process::exit(0);
    }
}
