//! String helpers for runtime

use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::io::{self, Write};
use std::ptr;
use std::sync::atomic::Ordering;

use crate::header::make_heap_header;
use crate::{RUNTIME_LOG, rc_dec, rc_inc};

#[unsafe(no_mangle)]
pub extern "C" fn heap_str_alloc(str_len: size_t) -> *mut c_void {
    unsafe {
        let total_size = match 16usize.checked_add(str_len).and_then(|s| s.checked_add(1)) {
            Some(total) => total,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] heap_str_alloc: integer overflow (str_len={})\n",
                            str_len
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        let p = crate::runtime_malloc(total_size);
        if p.is_null() {
            return ptr::null_mut();
        }

        let header_ptr = p as *mut u64;
        *header_ptr = make_heap_header(1);

        let len_ptr = (p as *mut u8).add(8) as *mut u64;
        *len_ptr = str_len as u64;

        p
    }
}

#[unsafe(no_mangle)]
/// # Safety
/// `s` must be a valid nul-terminated C string. The returned pointer is an
/// owning heap string data pointer and must be released via `rc_dec_str`.
pub unsafe extern "C" fn heap_str_from_cstr(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let cstr = CStr::from_ptr(s);
        let bytes = cstr.to_bytes_with_nul();
        let len = bytes.len() - 1;
        let obj = heap_str_alloc(len as size_t);
        if obj.is_null() {
            return ptr::null_mut();
        }
        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;
        ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr as *mut u8, bytes.len());
        data_ptr
    }
}

pub fn heap_str_from_cstr_pub(s: *const c_char) -> *mut c_char {
    unsafe { heap_str_from_cstr(s) }
}

#[inline]
pub unsafe fn heap_str_to_obj(data: *const c_char) -> *mut c_void {
    if data.is_null() {
        return ptr::null_mut();
    }
    unsafe { (data as *mut u8).sub(16) as *mut c_void }
}

#[unsafe(no_mangle)]
/// # Safety
/// `data` must be a pointer previously returned by this runtime for string data
/// (either a static literal or a heap-allocated string). Passing invalid
/// pointers is undefined behavior.
pub unsafe extern "C" fn rc_inc_str(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    unsafe {
        let obj = heap_str_to_obj(data);
        rc_inc(obj);
    }
}

pub fn rc_inc_str_pub(data: *mut c_char) {
    unsafe { rc_inc_str(data) }
}

#[unsafe(no_mangle)]
/// # Safety
/// `data` must be a pointer previously returned by this runtime for string data
/// (either a static literal or a heap-allocated string). Passing invalid
/// pointers is undefined behavior.
pub unsafe extern "C" fn rc_dec_str(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    unsafe {
        let obj = heap_str_to_obj(data);
        rc_dec(obj);
    }
}

pub fn rc_dec_str_pub(data: *mut c_char) {
    unsafe { rc_dec_str(data) }
}

#[unsafe(no_mangle)]
pub unsafe fn str_dup(s: *const c_char) -> *mut c_char {
    unsafe {
        if s.is_null() {
            return ptr::null_mut();
        }
        let len = libc::strlen(s) + 1;
        let dst = crate::runtime_malloc(len) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(s as *const u8, dst as *mut u8, len);
        dst
    }
}

/// # Safety
/// `a` and `b` must be valid nul-terminated C string pointers. The returned
/// pointer is an owning C string which should be released with `runtime_free`.
#[unsafe(no_mangle)]
pub unsafe fn str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    unsafe {
        if a.is_null() || b.is_null() {
            return ptr::null_mut();
        }
        let la = CStr::from_ptr(a).to_bytes().len();
        let lb = CStr::from_ptr(b).to_bytes().len();

        let total_len = match la.checked_add(lb) {
            Some(len) => len,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] str_concat: integer overflow (len_a={}, len_b={})\n",
                            la, lb
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        let obj = heap_str_alloc(total_len);
        if obj.is_null() {
            return ptr::null_mut();
        }

        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;

        let aslice = CStr::from_ptr(a).to_bytes();
        let bslice = CStr::from_ptr(b).to_bytes();
        ptr::copy_nonoverlapping(aslice.as_ptr(), data_ptr as *mut u8, la);
        ptr::copy_nonoverlapping(bslice.as_ptr(), data_ptr.add(la) as *mut u8, lb);

        *data_ptr.add(la + lb) = 0;

        data_ptr
    }
}
