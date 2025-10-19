//! Error handling for the Oats standard library

use libc::c_int;

/// Standard library error type
#[derive(Debug, Clone)]
pub struct Error {
    pub code: c_int,
    pub message: String,
}

impl Error {
    pub fn new(code: c_int, message: String) -> Self {
        Self { code, message }
    }
}

/// Result type alias
pub type Result<T> = std::result::Result<T, Error>;

/// Error codes
pub const ERROR_IO: c_int = 1;
pub const ERROR_NOT_FOUND: c_int = 2;
pub const ERROR_PERMISSION_DENIED: c_int = 3;
pub const ERROR_INVALID_ARGUMENT: c_int = 4;
pub const ERROR_NETWORK: c_int = 5;
pub const ERROR_TIMEOUT: c_int = 6;
pub const ERROR_UNKNOWN: c_int = 99;