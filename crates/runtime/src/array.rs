//! Array helpers for runtime (shims)

#[allow(unused_imports)]
pub use crate::array_alloc;
#[allow(unused_imports)]
pub(crate) use crate::array_get_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_get_ptr;
#[allow(unused_imports)]
pub(crate) use crate::array_get_ptr_borrow;
#[allow(unused_imports)]
pub(crate) use crate::array_pop_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_pop_ptr;
#[allow(unused_imports)]
pub(crate) use crate::array_push_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_push_ptr;
#[allow(unused_imports)]
pub(crate) use crate::array_push_ptr_weak;
#[allow(unused_imports)]
pub(crate) use crate::array_set_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_set_ptr;
#[allow(unused_imports)]
pub(crate) use crate::array_set_ptr_weak;

pub(crate) fn init_array_placeholders() {
    // no-op shim
}

// Internal grow helper moved from the monolithic ffi module. Kept as
// `pub(crate)` so other runtime modules can call it during incremental
// refactor without exposing it as part of the public C API.
use libc::c_void;
use std::mem;
use std::ptr;

use crate::rc::rc_inc;

#[allow(dead_code)]
pub(crate) fn array_grow(arr: *mut c_void, min_capacity: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    let header_ptr = arr as *const u64;
    let header = unsafe { *header_ptr };
    let elem_is_number = ((header >> 32) & 1) as i32;

    let len_ptr = unsafe { (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64 };
    let len = unsafe { *len_ptr as usize };

    let cap_ptr = unsafe { (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *const u64 };
    let old_capacity = unsafe { *cap_ptr as usize };

    let mut new_capacity = old_capacity + (old_capacity / 2).max(1);
    if new_capacity < min_capacity {
        new_capacity = min_capacity;
    }

    let elem_size = if elem_is_number != 0 {
        mem::size_of::<f64>()
    } else {
        mem::size_of::<*mut c_void>()
    };

    let new_arr = array_alloc(new_capacity, elem_size, elem_is_number);
    if new_arr.is_null() {
        return ptr::null_mut();
    }

    let new_len_ptr = unsafe { (new_arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64 };
    unsafe { *new_len_ptr = len as u64 };

    let old_data = unsafe { (arr as *mut u8).add(crate::ARRAY_HEADER_SIZE) };
    let new_data = unsafe { (new_arr as *mut u8).add(crate::ARRAY_HEADER_SIZE) };
    unsafe { ptr::copy_nonoverlapping(old_data, new_data, len * elem_size) };

    if elem_is_number == 0 {
        let ptrs = new_data as *mut *mut c_void;
        for i in 0..len {
            let p = unsafe { *ptrs.add(i) };
            if !p.is_null() {
                unsafe { rc_inc(p) };
            }
        }
    }

    new_arr
}
