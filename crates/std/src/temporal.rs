//! Temporal API implementation
//!
//! Provides Temporal date/time functionality as specified in the TC39 proposal.
//! Currently implements basic Temporal.Instant and Temporal.now().

use libc::{c_longlong, c_void};

/// Represents a Temporal.Instant - a point in time with nanosecond precision
/// Layout: [header][timestamp_ns: i64]
pub struct TemporalInstant {
    ptr: *mut c_void,
}

impl TemporalInstant {
    /// Create a new TemporalInstant from a timestamp in nanoseconds since Unix epoch
    pub fn new(timestamp_ns: i64) -> Option<Self> {
        // Allocate object with header + i64 field
        let obj_size = 16 + 8; // header (16 bytes) + i64 field
        let ptr = runtime::heap::runtime_malloc(obj_size);
        if ptr.is_null() {
            return None;
        }

        // Initialize header (strong RC = 1, static = 0, weak RC = 0, type = 0)
        unsafe {
            let header_ptr = ptr as *mut u64;
            *header_ptr = 1; // strong RC = 1
        }

        // Set the timestamp field at offset 16
        unsafe {
            let field_ptr = (ptr as *mut u8).add(16) as *mut i64;
            *field_ptr = timestamp_ns;
        }

        Some(Self { ptr })
    }

    /// Get the timestamp in nanoseconds
    pub fn timestamp_ns(&self) -> i64 {
        unsafe {
            let field_ptr = (self.ptr as *mut u8).add(16) as *mut i64;
            *field_ptr
        }
    }

    /// Get the underlying pointer
    pub fn as_ptr(&self) -> *mut c_void {
        self.ptr
    }
}

impl Drop for TemporalInstant {
    fn drop(&mut self) {
        if !self.ptr.is_null() {
            unsafe { runtime::rc::rc_dec_pub(self.ptr) };
        }
    }
}

/// Get the current instant (Temporal.now.instant())
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_temporal_now_instant() -> *mut c_void {
    let now = std::time::SystemTime::now();
    let timestamp_ns = now
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as i64;

    match TemporalInstant::new(timestamp_ns) {
        Some(instant) => {
            // Return the pointer, ownership transferred to caller
            let ptr = instant.as_ptr();
            std::mem::forget(instant); // Don't drop
            ptr
        }
        None => std::ptr::null_mut(),
    }
}

/// Get epoch nanoseconds from a Temporal.Instant
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_temporal_instant_epoch_nanoseconds(instant: *mut c_void) -> c_longlong {
    if instant.is_null() {
        return 0;
    }

    // Read the timestamp field
    unsafe {
        let field_ptr = (instant as *mut u8).add(16) as *mut i64;
        *field_ptr
    }
}

/// Create a Temporal.Instant from nanoseconds
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_temporal_instant_from_epoch_nanoseconds(ns: c_longlong) -> *mut c_void {
    match TemporalInstant::new(ns) {
        Some(instant) => {
            let ptr = instant.as_ptr();
            std::mem::forget(instant);
            ptr
        }
        None => std::ptr::null_mut(),
    }
}

/// Add duration to instant (basic implementation)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_temporal_instant_add(
    instant: *mut c_void,
    nanoseconds: c_longlong,
) -> *mut c_void {
    if instant.is_null() {
        return std::ptr::null_mut();
    }

    let current_ns = unsafe {
        let field_ptr = (instant as *mut u8).add(16) as *mut i64;
        *field_ptr
    };

    let new_ns = current_ns + nanoseconds;

    match TemporalInstant::new(new_ns) {
        Some(new_instant) => {
            let ptr = new_instant.as_ptr();
            std::mem::forget(new_instant);
            ptr
        }
        None => std::ptr::null_mut(),
    }
}

/// Get the current instant (Temporal.now() - simplified)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_temporal_now() -> *mut c_void {
    oats_std_temporal_now_instant()
}
