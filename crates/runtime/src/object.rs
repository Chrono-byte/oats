//! Object/class helpers for runtime

use libc::c_char;
use std::os::raw::c_void;
use std::ptr;

use crate::{get_object_base, heap_str_from_cstr, validate_meta_block};

/// Convert a tuple/object into a printable C string.
///
/// Iterates the tuple metadata block and stringifies each 8-byte field.
/// The returned pointer is an owning data pointer (offset +16) and must
/// be released with `rc_dec_str`.
///
/// # Safety
/// `obj` must be a valid object pointer allocated by this runtime and contain a
/// valid metadata pointer at +8. Passing arbitrary pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn tuple_to_string(obj: *mut c_void) -> *mut c_char {
    if obj.is_null() {
        return ptr::null_mut();
    }
    // Resolve base pointer (handles string-data pointers too)
    let base = unsafe { get_object_base(obj) };
    if base.is_null() {
        return ptr::null_mut();
    }
    // meta pointer stored at offset +8
    let meta_ptr_ptr = unsafe { (base as *mut u8).add(8) as *mut *mut u64 };
    let meta = unsafe { *meta_ptr_ptr };
    if meta.is_null() {
        return ptr::null_mut();
    }
    if unsafe { !validate_meta_block(meta, 1024) } {
        return ptr::null_mut();
    }

    let len = unsafe { ((*meta) & 0xffffffffu64) as usize };
    let offsets_ptr = unsafe { meta.add(1) as *const i32 };

    let mut parts: Vec<String> = Vec::new();
    for i in 0..len {
        let off_i32 = unsafe { *offsets_ptr.add(i) };
        let off = off_i32 as isize as usize;
        let field_addr = unsafe { (base as *mut u8).add(off) as *const u64 };
        let raw = unsafe { *field_addr as u64 };
        // Reuse the stringify helper from crate root if available
        let s = crate::stringify_value_raw(raw, 0);
        parts.push(s);
    }
    let joined = parts.join(", ");
    let out = format!("({})", joined);
    let c = std::ffi::CString::new(out).unwrap_or_default();
    unsafe { heap_str_from_cstr(c.as_ptr()) }
}

#[allow(dead_code)]
pub(crate) fn init_object_placeholders() {
    // no-op shim
}
