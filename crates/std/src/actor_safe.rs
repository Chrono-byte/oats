//! Safe Rust API wrappers for the actor/process model.
//!
//! This module provides safe, idiomatic Rust interfaces for process management,
//! message passing, and supervision. These wrappers call the underlying unsafe
//! FFI functions but provide type safety and error handling.

use std::ffi::CString;

use crate::actor::{MonitorRef, ProcessId};

/// Process priority levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProcessPriority {
    Normal = 0,
    High = 1,
}

impl From<i32> for ProcessPriority {
    fn from(priority: i32) -> Self {
        match priority {
            1 => ProcessPriority::High,
            _ => ProcessPriority::Normal,
        }
    }
}

impl From<ProcessPriority> for i32 {
    fn from(priority: ProcessPriority) -> Self {
        priority as i32
    }
}

/// Error type for process operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProcessError {
    /// Process limit exceeded
    LimitExceeded,
    /// Process not found
    NotFound,
    /// Invalid process name
    InvalidName,
    /// Invalid pointer or memory error
    InvalidPointer,
    /// Process already terminated
    Terminated,
    /// Other error
    Other(String),
}

impl std::fmt::Display for ProcessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProcessError::LimitExceeded => write!(f, "Process limit exceeded"),
            ProcessError::NotFound => write!(f, "Process not found"),
            ProcessError::InvalidName => write!(f, "Invalid process name"),
            ProcessError::InvalidPointer => write!(f, "Invalid pointer"),
            ProcessError::Terminated => write!(f, "Process already terminated"),
            ProcessError::Other(msg) => write!(f, "Process error: {}", msg),
        }
    }
}

impl std::error::Error for ProcessError {}

pub fn spawn(priority: ProcessPriority) -> Result<ProcessId, ProcessError> {
    let pid_ptr = crate::actor::oats_std_actor_spawn(priority.into());

    if pid_ptr.is_null() {
        return Err(ProcessError::LimitExceeded);
    }

    // SAFETY: pid_ptr is guaranteed to be a valid pointer to a u64
    // allocated by oats_std_actor_spawn. We read it once and immediately
    // free it. The runtime is responsible for correct allocation.
    let pid = unsafe {
        let pid = *(pid_ptr as *const u64);
        libc::free(pid_ptr);
        pid
    };

    Ok(pid)
}

/// Spawn a new process with a name and priority.
///
/// The process can be looked up by name using `whereis`.
pub fn spawn_named(name: &str, priority: ProcessPriority) -> Result<ProcessId, ProcessError> {
    let c_name = match CString::new(name) {
        Ok(s) => s,
        Err(_) => return Err(ProcessError::InvalidName),
    };

    let pid_ptr =
        crate::actor::oats_std_actor_spawn_named(c_name.as_ptr(), priority.into());
    if pid_ptr.is_null() {
        return Err(ProcessError::LimitExceeded);
    }

    // Read the ProcessId from the heap-allocated pointer
    let pid = unsafe { *(pid_ptr as *const u64) };

    // Free the heap-allocated pointer
    unsafe {
        libc::free(pid_ptr);
    }

    Ok(pid)
}

/// Send a message to a process.
///
/// The payload is a type-erased pointer to an ARC-managed heap object.
/// The reference count will be incremented when the message is sent.
///
/// # Safety
///
/// The `payload` must be a valid pointer to an ARC-managed object.
/// This function is marked unsafe because we cannot verify the payload type
/// at compile time. However, it's safer than the raw FFI function because
/// it validates the process IDs.
pub unsafe fn send(
    to: ProcessId,
    from: ProcessId,
    payload: *mut std::ffi::c_void,
    type_id: u64,
) -> Result<(), ProcessError> {
    let to_pid = &to as *const u64;
    let from_pid = &from as *const u64;

    let result = crate::actor::oats_std_actor_send(to_pid, from_pid, payload, type_id);
    if result == 1 {
        Ok(())
    } else {
        Err(ProcessError::NotFound)
    }
}

/// Send a message to a named process.
///
/// # Safety
///
/// The `payload` must be a valid pointer to an ARC-managed object.
pub unsafe fn send_to_name(
    name: &str,
    from: ProcessId,
    payload: *mut std::ffi::c_void,
    type_id: u64,
) -> Result<(), ProcessError> {
    let c_name = match CString::new(name) {
        Ok(s) => s,
        Err(_) => return Err(ProcessError::InvalidName),
    };

    let from_pid = &from as *const u64;

    let result =
        crate::actor::oats_std_actor_send_to_name(c_name.as_ptr(), from_pid, payload, type_id);
    if result == 1 {
        Ok(())
    } else {
        Err(ProcessError::NotFound)
    }
}

/// Receive a message from a specific process.
///
/// Blocks until a message is received from the specified process with the given type ID.
///
/// # Safety
///
/// The returned pointer must be properly managed with reference counting.
pub unsafe fn receive(pid: ProcessId, type_id: u64) -> Option<*mut std::ffi::c_void> {
    let pid_ptr = &pid as *const u64;
    let result = crate::actor::oats_std_actor_receive(pid_ptr, type_id);
    if result.is_null() { None } else { Some(result) }
}

/// Get the current process ID.
pub fn self_pid() -> ProcessId {
    let pid_ptr = crate::actor::oats_std_actor_self();
    if pid_ptr.is_null() {
        0 // Should not happen, but defensive
    } else {
        let pid = unsafe { *(pid_ptr as *const u64) };
        unsafe {
            libc::free(pid_ptr);
        }
        pid
    }
}

/// Exit the current process with a reason.
pub fn exit(reason: &str) -> ! {
    let c_reason = CString::new(reason).unwrap_or_else(|_| CString::new("").unwrap());
    crate::actor::oats_std_actor_exit(&self_pid() as *const ProcessId, 0, c_reason.as_ptr());
    // This should never return, but if it does, we panic
    panic!("Process exit did not terminate");
}

/// Look up a process by name.
///
/// Returns the process ID if found, or `None` if the name is not registered.
pub fn whereis(name: &str) -> Option<ProcessId> {
    let c_name = match CString::new(name) {
        Ok(s) => s,
        Err(_) => return None,
    };

    let pid_ptr = crate::actor::oats_std_actor_whereis(c_name.as_ptr());
    if pid_ptr.is_null() {
        None
    } else {
        let pid = unsafe { *(pid_ptr as *const u64) };
        unsafe {
            libc::free(pid_ptr);
        }
        Some(pid)
    }
}

/// Register a process with a name.
pub fn register(pid: ProcessId, name: &str) -> Result<(), ProcessError> {
    let c_name = match CString::new(name) {
        Ok(s) => s,
        Err(_) => return Err(ProcessError::InvalidName),
    };

    let pid_ptr = &pid as *const u64;
    let result = crate::actor::oats_std_actor_register(pid_ptr, c_name.as_ptr());
    if result == 0 {
        Ok(())
    } else {
        Err(ProcessError::Other("Registration failed".to_string()))
    }
}

/// Unregister a process name.
pub fn unregister(name: &str) -> Result<(), ProcessError> {
    let c_name = match CString::new(name) {
        Ok(s) => s,
        Err(_) => return Err(ProcessError::InvalidName),
    };

    crate::actor::oats_std_actor_unregister(c_name.as_ptr());
    Ok(())
}

/// Link two processes together.
///
/// When one process exits, the other will receive an exit notification.
pub fn link(pid1: ProcessId, pid2: ProcessId) -> Result<(), ProcessError> {
    let pid1_ptr = &pid1 as *const u64;
    let pid2_ptr = &pid2 as *const u64;
    let result = crate::actor::oats_std_actor_link(pid1_ptr, pid2_ptr);
    if result == 0 {
        Ok(())
    } else {
        Err(ProcessError::NotFound)
    }
}

/// Unlink two processes.
pub fn unlink(pid1: ProcessId, pid2: ProcessId) -> Result<(), ProcessError> {
    let pid1_ptr = &pid1 as *const u64;
    let pid2_ptr = &pid2 as *const u64;
    crate::actor::oats_std_actor_unlink(pid1_ptr, pid2_ptr);
    Ok(())
}

/// Monitor a process and get a monitor reference.
pub fn monitor(monitor_pid: ProcessId, target_pid: ProcessId) -> Result<MonitorRef, ProcessError> {
    let monitor_pid_ptr = &monitor_pid as *const u64;
    let target_pid_ptr = &target_pid as *const u64;
    let ref_ptr = crate::actor::oats_std_actor_monitor(monitor_pid_ptr, target_pid_ptr);
    if ref_ptr.is_null() {
        return Err(ProcessError::NotFound);
    }

    let monitor_ref = unsafe { *(ref_ptr as *const u64) };
    unsafe {
        libc::free(ref_ptr);
    }
    Ok(monitor_ref)
}

/// Remove a monitor.
pub fn demonitor(monitor_pid: ProcessId, monitor_ref: MonitorRef) -> Result<(), ProcessError> {
    let monitor_pid_ptr = &monitor_pid as *const u64;
    let monitor_ref_ptr = &monitor_ref as *const u64;
    crate::actor::oats_std_actor_demonitor(monitor_pid_ptr, monitor_ref_ptr);
    Ok(())
}
