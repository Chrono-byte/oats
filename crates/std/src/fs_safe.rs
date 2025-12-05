//! Safe Rust API wrappers for file system operations.
//!
//! This module provides safe, idiomatic Rust interfaces for file I/O operations,
//! using `Path` and `PathBuf` instead of raw C strings.

use std::ffi::{CStr, CString};
use std::path::{Path, PathBuf};

use crate::fs::{
    oats_std_fs_create_dir_all, oats_std_fs_file_exists, oats_std_fs_file_size, oats_std_fs_is_dir,
    oats_std_fs_is_file, oats_std_fs_read_dir, oats_std_fs_read_dir_count,
    oats_std_fs_read_dir_free, oats_std_fs_read_file,
    oats_std_fs_remove_dir, oats_std_fs_remove_dir_all, oats_std_fs_remove_file,
    oats_std_fs_write_file,
};

/// Error type for file system operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FsError {
    /// File or directory not found
    NotFound,
    /// Permission denied
    PermissionDenied,
    /// Invalid path
    InvalidPath,
    /// I/O error
    IoError(String),
    /// Other error
    Other(String),
}

impl std::fmt::Display for FsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FsError::NotFound => write!(f, "File or directory not found"),
            FsError::PermissionDenied => write!(f, "Permission denied"),
            FsError::InvalidPath => write!(f, "Invalid path"),
            FsError::IoError(msg) => write!(f, "I/O error: {}", msg),
            FsError::Other(msg) => write!(f, "File system error: {}", msg),
        }
    }
}

impl std::error::Error for FsError {}

impl From<std::io::Error> for FsError {
    fn from(err: std::io::Error) -> Self {
        match err.kind() {
            std::io::ErrorKind::NotFound => FsError::NotFound,
            std::io::ErrorKind::PermissionDenied => FsError::PermissionDenied,
            _ => FsError::IoError(err.to_string()),
        }
    }
}

/// Read the entire contents of a file into a string.
pub fn read_file(path: &Path) -> Result<String, FsError> {
    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;

    let result_ptr = oats_std_fs_read_file(c_path.as_ptr());
    if result_ptr.is_null() {
        return Err(FsError::NotFound);
    }

    // Convert C string to Rust string
    let c_str = unsafe { CStr::from_ptr(result_ptr) };
    let result = match c_str.to_str() {
        Ok(s) => Ok(s.to_string()),
        Err(_) => Err(FsError::IoError("Invalid UTF-8 in file".to_string())),
    };

    // Free the C string allocated by the FFI function
    unsafe {
        libc::free(result_ptr as *mut std::ffi::c_void);
    }

    result
}

/// Write a string to a file, creating it if it doesn't exist or overwriting if it does.
pub fn write_file(path: &Path, content: &str) -> Result<(), FsError> {
    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;
    let c_content = CString::new(content).map_err(|_| FsError::InvalidPath)?;

    let result = oats_std_fs_write_file(c_path.as_ptr(), c_content.as_ptr());
    if result == 0 {
        Ok(())
    } else {
        Err(FsError::IoError("Failed to write file".to_string()))
    }
}

/// Check if a file exists.
pub fn file_exists(path: &Path) -> bool {
    let path_str = match path.to_str() {
        Some(s) => s,
        None => return false,
    };

    let c_path = match CString::new(path_str) {
        Ok(s) => s,
        Err(_) => return false,
    };

    let result = oats_std_fs_file_exists(c_path.as_ptr());
    result != 0
}

/// Create a directory and all parent directories if they don't exist.
pub fn create_dir_all(path: &Path) -> Result<(), FsError> {
    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;

    let result = oats_std_fs_create_dir_all(c_path.as_ptr());
    if result != 0 {
        Ok(())
    } else {
        Err(FsError::IoError("Failed to create directory".to_string()))
    }
}

/// Remove a file.
pub fn remove_file(path: &Path) -> Result<(), FsError> {
    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;

    let result = oats_std_fs_remove_file(c_path.as_ptr());
    if result == 0 {
        Ok(())
    } else {
        Err(FsError::NotFound)
    }
}

/// Remove an empty directory.
pub fn remove_dir(path: &Path) -> Result<(), FsError> {
    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;

    let result = oats_std_fs_remove_dir(c_path.as_ptr());
    if result == 0 {
        Ok(())
    } else {
        Err(FsError::NotFound)
    }
}

/// Remove a directory and all its contents.
pub fn remove_dir_all(path: &Path) -> Result<(), FsError> {
    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;

    let result = oats_std_fs_remove_dir_all(c_path.as_ptr());
    if result == 0 {
        Ok(())
    } else {
        Err(FsError::NotFound)
    }
}

/// Check if a path is a directory.
pub fn is_dir(path: &Path) -> bool {
    let path_str = match path.to_str() {
        Some(s) => s,
        None => return false,
    };

    let c_path = match CString::new(path_str) {
        Ok(s) => s,
        Err(_) => return false,
    };

    let result = oats_std_fs_is_dir(c_path.as_ptr());
    result != 0
}

/// Check if a path is a regular file.
pub fn is_file(path: &Path) -> bool {
    let path_str = match path.to_str() {
        Some(s) => s,
        None => return false,
    };

    let c_path = match CString::new(path_str) {
        Ok(s) => s,
        Err(_) => return false,
    };

    let result = oats_std_fs_is_file(c_path.as_ptr());
    result != 0
}

/// Get the size of a file in bytes.
pub fn file_size(path: &Path) -> Result<u64, FsError> {
    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;

    let result = oats_std_fs_file_size(c_path.as_ptr());
    if result < 0 {
        Err(FsError::NotFound)
    } else {
        Ok(result as u64)
    }
}

pub fn read_dir(path: &Path) -> Result<Vec<PathBuf>, FsError> {
    use std::ffi::CStr;
    use std::os::raw::c_char;

    let path_str = path.to_str().ok_or(FsError::InvalidPath)?;
    let c_path = CString::new(path_str).map_err(|_| FsError::InvalidPath)?;
    let c_path_ptr = c_path.as_ptr();

    // Pass the raw pointer, cast to the expected type
    let mut c_path_mut = c_path_ptr as *mut c_char;
    let count = oats_std_fs_read_dir_count(&mut c_path_mut);

    if count == 0 {
        return Ok(Vec::new());
    }

    let entries_ptr = oats_std_fs_read_dir(c_path_ptr);
    if entries_ptr.is_null() {
        return Err(FsError::NotFound);
    }

    let mut paths = Vec::with_capacity(count);

    // SAFETY: entries_ptr is valid for `count` elements per C runtime contract.
    // Pointer arithmetic is within bounds.
    for i in 0..count {
        let entry_ptr = unsafe { *entries_ptr.add(i) };
        if entry_ptr.is_null() {
            continue;
        }

        // SAFETY: entry_ptr is a valid C string allocated by the C runtime
        let c_str = unsafe { CStr::from_ptr(entry_ptr as *const c_char) };

        // Handle UTF-8 conversion explicitly instead of silently dropping invalid paths
        match c_str.to_str() {
            Ok(path_str) => paths.push(PathBuf::from(path_str)),
            Err(_) => continue, // Skip non-UTF-8 paths
        }
    }

    // SAFETY: entries_ptr was allocated by the C runtime and is valid for deallocation
    oats_std_fs_read_dir_free(entries_ptr);

    Ok(paths)
}
