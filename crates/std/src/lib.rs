//! Oats Standard Library
//!
//! This crate provides standard library functions for the Oats programming language,
//! implementing system interfaces, file I/O, networking, and other OS-level functionality
//! that would be needed to port tools like Toasty from Rust to Oats.

// Core modules
pub mod core;
pub mod raw;
pub mod sys;

// Public API modules
pub mod actor;
pub mod actor_safe;
pub mod console;
pub mod env;
pub mod format;
pub mod fs;
pub mod fs_safe;
pub mod hash;
pub mod io;
pub mod math;
pub mod net;
pub mod path;
pub mod process;
pub mod random;
pub mod string;
pub mod temporal;
pub mod time;

// Re-export commonly used types
pub use core::{Error, OatsArray, OatsObject, OatsString, Result};

// Initialize the standard library
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_init() {
    // Initialize any global state needed by the std library
    // This would be called during program startup
}

/// Clean up standard library resources
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_cleanup() {
    // Clean up any global state
}
