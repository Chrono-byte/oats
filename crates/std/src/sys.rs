//! FFI abstraction layer for runtime functions
//!
//! This module provides safe wrappers around the unsafe FFI calls to the runtime.
//! All unsafe operations are contained here to provide a safe API to the rest of std.

use libc::{c_char, c_void, size_t};
use std::ptr::NonNull;

// Runtime FFI declarations
extern "C" {
    // Memory management
    fn runtime_malloc(size: size_t) -> *mut c_void;
    fn runtime_free(ptr: *mut c_void);

    // String operations
    #[allow(dead_code)]
    fn heap_str_alloc(str_len: size_t) -> *mut c_void;
    fn heap_str_from_cstr(s: *const c_char) -> *mut c_char;
    fn rc_inc_str(s: *mut c_char);
    fn rc_dec_str(s: *mut c_char);

    // Array operations
    fn array_alloc(element_size: size_t, length: size_t) -> *mut c_void;
    fn array_get(array: *mut c_void, index: size_t) -> *mut c_void;
    fn array_set(array: *mut c_void, index: size_t, value: *mut c_void);

    // Reference counting
    fn rc_inc(ptr: *mut c_void);
    fn rc_dec(ptr: *mut c_void);

    // Print functions
    fn print_str(s: *const c_char);
    fn print_f64(v: f64);
    fn print_i32(v: i32);
    fn print_newline();
}

// Safe wrappers

/// Allocate memory using the runtime allocator
pub fn malloc(size: usize) -> Option<NonNull<c_void>> {
    unsafe {
        let ptr = runtime_malloc(size);
        NonNull::new(ptr)
    }
}

/// Free memory using the runtime allocator
pub fn free(ptr: NonNull<c_void>) {
    unsafe {
        runtime_free(ptr.as_ptr());
    }
}

/// Create a new string in the runtime heap
pub fn create_string(content: &str) -> Option<NonNull<c_char>> {
    unsafe {
        let cstr = std::ffi::CString::new(content).ok()?;
        let ptr = heap_str_from_cstr(cstr.as_ptr());
        NonNull::new(ptr)
    }
}

/// Increment reference count for a string
pub fn rc_inc_string(s: NonNull<c_char>) {
    unsafe {
        rc_inc_str(s.as_ptr());
    }
}

/// Decrement reference count for a string
pub fn rc_dec_string(s: NonNull<c_char>) {
    unsafe {
        rc_dec_str(s.as_ptr());
    }
}

/// Create a new array in the runtime heap
pub fn create_array(element_size: usize, length: usize) -> Option<NonNull<c_void>> {
    unsafe {
        let ptr = array_alloc(element_size, length);
        NonNull::new(ptr)
    }
}

/// Get an element from an array
pub fn array_get_element(array: NonNull<c_void>, index: usize) -> Option<NonNull<c_void>> {
    unsafe {
        let ptr = array_get(array.as_ptr(), index);
        NonNull::new(ptr)
    }
}

/// Set an element in an array
pub fn array_set_element(array: NonNull<c_void>, index: usize, value: NonNull<c_void>) {
    unsafe {
        array_set(array.as_ptr(), index, value.as_ptr());
    }
}

/// Increment reference count for any object
pub fn rc_inc_object(obj: NonNull<c_void>) {
    unsafe {
        rc_inc(obj.as_ptr());
    }
}

/// Decrement reference count for any object
pub fn rc_dec_object(obj: NonNull<c_void>) {
    unsafe {
        rc_dec(obj.as_ptr());
    }
}

/// Print functions
pub fn print_string(s: &str) {
    if let Ok(cstr) = std::ffi::CString::new(s) {
        unsafe {
            print_str(cstr.as_ptr());
        }
    }
}

pub fn print_number_f64(v: f64) {
    unsafe {
        print_f64(v);
    }
}

pub fn print_number_i32(v: i32) {
    unsafe {
        print_i32(v);
    }
}

pub fn print_nl() {
    unsafe {
        print_newline();
    }
}