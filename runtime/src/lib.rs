use std::ffi::CStr;
use libc::{c_char, c_void, size_t};
use std::ptr;
use std::io::{self, Write};

pub extern "C" fn runtime_malloc(size: size_t) -> *mut c_void {
    unsafe { libc::malloc(size) }
}

pub extern "C" fn runtime_free(p: *mut c_void) {
    unsafe { libc::free(p) }
}

pub extern "C" fn runtime_strlen(s: *const c_char) -> size_t {
    if s.is_null() {
        return 0;
    }
    unsafe { libc::strlen(s) }
}

pub extern "C" fn str_dup(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len = libc::strlen(s) + 1;
        let dst = libc::malloc(len) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        libc::memcpy(dst as *mut c_void, s as *const c_void, len);
        dst
    }
}

pub extern "C" fn str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    if a.is_null() || b.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let la = libc::strlen(a);
        let lb = libc::strlen(b);
        let total = la + lb + 1;
        let dst = libc::malloc(total) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        libc::memcpy(dst as *mut c_void, a as *const c_void, la);
        libc::memcpy(dst.add(la as usize) as *mut c_void, b as *const c_void, lb);
        *dst.add((la + lb) as usize) = 0;
        dst
    }
}

pub extern "C" fn print_f64(v: f64) {
    // simple printf wrapper
    unsafe {
        libc::printf(b"%f\n\0".as_ptr() as *const c_char, v);
    }
}

pub extern "C" fn print_str(s: *const c_char) {
    if s.is_null() { return; }
    unsafe {
        let _ = io::stdout().write_all(CStr::from_ptr(s).to_bytes());
        let _ = io::stdout().write_all(b"\n");
    }
}
