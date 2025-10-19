//! Core Oats types with safe Rust wrappers

use crate::sys;
use libc::c_char;
use std::ptr::NonNull;

/// Safe wrapper for Oats strings
#[derive(Debug)]
pub struct OatsString {
    ptr: NonNull<c_char>,
}

impl OatsString {
    /// Create a new Oats string from a Rust string
    pub fn new(content: &str) -> Option<Self> {
        sys::create_string(content).map(|ptr| Self { ptr })
    }

    /// Get the underlying pointer (for FFI)
    pub fn as_ptr(&self) -> *mut c_char {
        self.ptr.as_ptr()
    }

    /// Convert to a Rust string (borrowed)
    pub fn as_str(&self) -> &str {
        // This is unsafe and would need proper implementation
        // For now, this is a placeholder
        ""
    }
}

impl Clone for OatsString {
    fn clone(&self) -> Self {
        sys::rc_inc_string(self.ptr);
        Self { ptr: self.ptr }
    }
}

impl Drop for OatsString {
    fn drop(&mut self) {
        sys::rc_dec_string(self.ptr);
    }
}

/// Safe wrapper for Oats arrays
#[derive(Debug)]
pub struct OatsArray {
    ptr: NonNull<libc::c_void>,
    #[allow(dead_code)]
    element_size: usize,
    length: usize,
}

impl OatsArray {
    /// Create a new Oats array
    pub fn new(element_size: usize, length: usize) -> Option<Self> {
        sys::create_array(element_size, length).map(|ptr| Self {
            ptr,
            element_size,
            length,
        })
    }

    /// Get the length of the array
    pub fn len(&self) -> usize {
        self.length
    }

    /// Check if the array is empty
    pub fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// Get an element at the specified index
    pub fn get(&self, index: usize) -> Option<NonNull<libc::c_void>> {
        if index >= self.length {
            return None;
        }
        sys::array_get_element(self.ptr, index)
    }

    /// Set an element at the specified index
    pub fn set(&self, index: usize, value: NonNull<libc::c_void>) {
        if index < self.length {
            sys::array_set_element(self.ptr, index, value);
        }
    }
}

impl Drop for OatsArray {
    fn drop(&mut self) {
        sys::rc_dec_object(self.ptr);
    }
}

/// Generic Oats object wrapper
#[derive(Debug)]
pub struct OatsObject {
    ptr: NonNull<libc::c_void>,
}

impl OatsObject {
    /// Create from a raw pointer
    pub unsafe fn from_ptr(ptr: NonNull<libc::c_void>) -> Self {
        Self { ptr }
    }

    /// Get the underlying pointer
    pub fn as_ptr(&self) -> *mut libc::c_void {
        self.ptr.as_ptr()
    }
}

impl Clone for OatsObject {
    fn clone(&self) -> Self {
        sys::rc_inc_object(self.ptr);
        Self { ptr: self.ptr }
    }
}

impl Drop for OatsObject {
    fn drop(&mut self) {
        sys::rc_dec_object(self.ptr);
    }
}

/// Error type for standard library operations
#[derive(Debug, Clone)]
pub struct Error {
    pub code: i32,
    pub message: String,
}

impl Error {
    pub fn new(code: i32, message: String) -> Self {
        Self { code, message }
    }
}

/// Result type alias
pub type Result<T> = std::result::Result<T, Error>;