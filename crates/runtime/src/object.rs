//! Object/class helpers for runtime

use libc::c_char;
use std::os::raw::c_void;
use std::ptr;

use crate::{
    HEADER_TYPE_TAG_SHIFT, get_object_base, header_with_weak, heap_str_from_cstr, rc_dec, rc_inc,
    runtime_malloc, validate_meta_block,
};
use libc::size_t;

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

/// Destructor used for union-boxed objects.
///
/// Reads the discriminant at offset +16 and if the payload is a pointer
/// (discriminant == 1) calls `rc_dec` on the stored pointer to release
/// the nested object reference.
fn union_dtor(obj_ptr: *mut c_void) {
    if obj_ptr.is_null() {
        return;
    }
    unsafe {
        // discriminant stored at offset +16
        let discrim_ptr = (obj_ptr as *mut u8).add(16) as *mut u64;
        let discrim = *discrim_ptr;
        if discrim == 1 {
            // pointer payload at offset +24
            let payload_ptr_ptr = (obj_ptr as *mut u8).add(24) as *mut *mut c_void;
            let payload_raw = *payload_ptr_ptr;
            if !payload_raw.is_null() {
                rc_dec(payload_raw);
            }
        }
        // nothing to do for f64 payload
    }
}

/// Box a numeric payload into a union heap object.
///
/// The returned control block pointer follows the runtime union layout
/// and must be released with `rc_dec` when no longer needed.
#[unsafe(no_mangle)]
pub extern "C" fn union_box_f64(v: f64) -> *mut c_void {
    unsafe {
        // layout: header(8) | dtor_ptr(8) | discrim(8) | payload(8)
        let total = 8 * 4;
        let mem = runtime_malloc(total as size_t) as *mut u8;
        if mem.is_null() {
            return ptr::null_mut();
        }

        // header: type_tag=1 (dtor present) | rc=1 | weak=1
        // weak=1 represents the object's own existence
        let header_ptr = mem as *mut u64;
        let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
        let refcount: u64 = 1u64;
        *header_ptr = header_with_weak(type_tag | refcount, 1);

        // store dtor pointer at offset +8
        let dtor_ptr = mem.add(8) as *mut *mut c_void;
        *dtor_ptr = union_dtor as *mut c_void;

        // discriminant = 0 for f64
        let discrim_ptr = mem.add(16) as *mut u64;
        *discrim_ptr = 0u64;

        // payload f64 at offset +24
        let payload = mem.add(24) as *mut f64;
        *payload = v;

        mem as *mut c_void
    }
}

/// Box a pointer payload into a union heap object (increasing the nested pointer's RC).
///
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime or NULL. The
/// returned control block is an owning object and must be released with `rc_dec`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn union_box_ptr(p: *mut c_void) -> *mut c_void {
    unsafe {
        let total = 8 * 4;
        let mem = runtime_malloc(total as size_t) as *mut u8;
        if mem.is_null() {
            return ptr::null_mut();
        }

        let header_ptr = mem as *mut u64;
        let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
        let refcount: u64 = 1u64;
        *header_ptr = header_with_weak(type_tag | refcount, 1);

        // store dtor
        let dtor_ptr = mem.add(8) as *mut *mut c_void;
        *dtor_ptr = union_dtor as *mut c_void;

        // discriminant = 1 for pointer
        let discrim_ptr = mem.add(16) as *mut u64;
        *discrim_ptr = 1u64;

        // payload pointer at offset +24
        let payload_ptr_ptr = mem.add(24) as *mut *mut c_void;
        *payload_ptr_ptr = p;
        // Ensure the boxed union owns a strong reference to the nested pointer
        // so the payload remains live for the lifetime of the union object.
        if !p.is_null() {
            rc_inc(p);
        }

        mem as *mut c_void
    }
}

/// Unbox a numeric payload from a union object.
///
/// # Safety
/// `u` must be a valid union object previously returned by the runtime.
#[unsafe(no_mangle)]
pub extern "C" fn union_unbox_f64(u: *mut c_void) -> f64 {
    unsafe {
        if u.is_null() {
            return 0.0;
        }
        let payload = (u as *mut u8).add(24) as *mut f64;
        *payload
    }
}

/// Unbox a pointer payload from a union object and return an owning pointer
/// (increments nested object's refcount before returning).
///
/// # Safety
/// `u` must be a valid union object previously returned by the runtime.
#[unsafe(no_mangle)]
pub extern "C" fn union_unbox_ptr(u: *mut c_void) -> *mut c_void {
    unsafe {
        if u.is_null() {
            return ptr::null_mut();
        }
        let payload_ptr_ptr = (u as *mut u8).add(24) as *mut *mut c_void;
        let p = *payload_ptr_ptr;
        if !p.is_null() {
            rc_inc(p);
        }
        p
    }
}

/// Read the discriminant of a union object.
///
/// Returns an integer code describing the payload kind (e.g., 0=f64, 1=pointer).
///
/// # Safety
/// `u` must be a valid union object previously returned by the runtime.
#[unsafe(no_mangle)]
pub extern "C" fn union_get_discriminant(u: *mut c_void) -> i64 {
    unsafe {
        if u.is_null() {
            return -1;
        }
        let discrim_ptr = (u as *mut u8).add(16) as *mut u64;
        *discrim_ptr as i64
    }
}
