//! FFI abstraction layer for runtime functions
//!
//! This module provides safe wrappers around the unsafe FFI calls to the runtime.
//! All unsafe operations are contained here to provide a safe API to the rest of std.

use libc::{c_char, c_void};
use std::ptr::NonNull;

// Safe wrappers

/// Allocate memory using the runtime allocator
pub fn malloc(size: usize) -> Option<NonNull<c_void>> {
    let ptr = runtime::heap::runtime_malloc(size);
    NonNull::new(ptr)
}

/// Free memory using the runtime allocator
pub fn free(ptr: NonNull<c_void>) {
    unsafe {
        runtime::heap::runtime_free(ptr.as_ptr());
    }
}

/// Create a new string in the runtime heap
pub fn create_string(content: &str) -> Option<NonNull<c_char>> {
    let cstr = std::ffi::CString::new(content).ok()?;
    let ptr = unsafe { runtime::string::heap_str_from_cstr_pub(cstr.as_ptr()) };
    NonNull::new(ptr)
}

/// Increment reference count for a string
pub fn rc_inc_string(s: NonNull<c_char>) {
    unsafe { runtime::string::rc_inc_str_pub(s.as_ptr()) };
}

/// Decrement reference count for a string
pub fn rc_dec_string(s: NonNull<c_char>) {
    unsafe { runtime::string::rc_dec_str_pub(s.as_ptr()) };
}

/// Create a new array in the runtime heap
pub fn create_array(element_size: usize, length: usize) -> Option<NonNull<c_void>> {
    let ptr = runtime::array::array_alloc_pub(element_size, length);
    NonNull::new(ptr)
}

/// Get an element from an array
pub fn array_get_element(array: NonNull<c_void>, index: usize) -> Option<NonNull<c_void>> {
    let ptr = runtime::array::array_get_pub(array.as_ptr(), index);
    NonNull::new(ptr)
}

/// Set an element in an array
pub fn array_set_element(array: NonNull<c_void>, index: usize, value: NonNull<c_void>) {
    unsafe { runtime::array::array_set_pub(array.as_ptr(), index, value.as_ptr()) };
}

/// Increment reference count for any object
pub fn rc_inc_object(obj: NonNull<c_void>) {
    unsafe { runtime::rc::rc_inc_pub(obj.as_ptr()) };
}

/// Decrement reference count for any object
pub fn rc_dec_object(obj: NonNull<c_void>) {
    unsafe { runtime::rc::rc_dec_pub(obj.as_ptr()) };
}

/// Print function that abstracts away the type
pub fn println<T: std::fmt::Display>(value: T) {
    println!("{}", value);
}
