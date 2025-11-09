//! Actor process model - Elixir/Erlang-inspired lightweight processes
//!
//! This module provides the standard library interface for the Oats process model,
//! implementing lightweight isolated execution units with message passing.

use libc::{c_char, c_void};

// Re-export runtime process functions
// These are the low-level FFI functions from the runtime

/// Process identifier type (opaque u64)
pub type ProcessId = u64;

/// Monitor reference type (opaque u64)
pub type MonitorRef = u64;

/// Spawn a new process
/// priority: 0=Normal, 1=High
/// Returns ProcessId as a pointer to heap-allocated u64 (caller must free), or null if limit exceeded
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_spawn(priority: i32) -> *mut c_void {
    // Call runtime function
    extern "C" {
        fn process_spawn(priority: i32) -> *mut c_void;
    }
    unsafe { process_spawn(priority) }
}

/// Spawn a new process with a name
/// name: null-terminated C string
/// priority: 0=Normal, 1=High
/// Returns ProcessId as a pointer to heap-allocated u64 (caller must free), or null if limit exceeded
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_spawn_named(name: *const c_char, priority: i32) -> *mut c_void {
    extern "C" {
        fn process_spawn_named(name: *const c_char, priority: i32) -> *mut c_void;
    }
    unsafe { process_spawn_named(name, priority) }
}

/// Send a message to a process
/// to_pid: pointer to u64 containing ProcessId
/// from_pid: pointer to u64 containing sender ProcessId
/// payload: pointer to ARC-managed heap object (will be incremented)
/// type_id: type identifier for runtime type checking
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_send(
    to_pid: *const u64,
    from_pid: *const u64,
    payload: *mut c_void,
    type_id: u64,
) -> i32 {
    extern "C" {
        fn process_send(
            to_pid: *const u64,
            from_pid: *const u64,
            payload: *mut c_void,
            type_id: u64,
        ) -> i32;
    }
    unsafe { process_send(to_pid, from_pid, payload, type_id) }
}

/// Send a message to a named process
/// name: null-terminated C string
/// from_pid: pointer to u64 containing sender ProcessId
/// payload: pointer to ARC-managed heap object
/// type_id: type identifier
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_send_to_name(
    name: *const c_char,
    from_pid: *const u64,
    payload: *mut c_void,
    type_id: u64,
) -> i32 {
    extern "C" {
        fn process_send_to_name(
            name: *const c_char,
            from_pid: *const u64,
            payload: *mut c_void,
            type_id: u64,
        ) -> i32;
    }
    unsafe { process_send_to_name(name, from_pid, payload, type_id) }
}

/// Receive a message from the current process's mailbox
/// pid: pointer to u64 containing ProcessId
/// type_id: optional type identifier (0 means any type)
/// Returns pointer to Message structure, or null if no message
/// Message structure layout: [from: u64][payload: *mut c_void][type_id: u64][timestamp: u64]
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_receive(pid: *const u64, type_id: u64) -> *mut c_void {
    extern "C" {
        fn process_receive(pid: *const u64, type_id: u64) -> *mut c_void;
    }
    unsafe { process_receive(pid, type_id) }
}

/// Get the current process ID (self())
/// Returns pointer to u64 containing ProcessId, or null if not in a process context
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_self() -> *mut c_void {
    extern "C" {
        fn process_self() -> *mut c_void;
    }
    unsafe { process_self() }
}

/// Exit the current process
/// pid: pointer to u64 containing ProcessId
/// reason_type: 0=normal, 1=error, 2=kill, 3=shutdown
/// reason_str: null-terminated C string (for error/kill/shutdown reasons, can be null)
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_exit(
    pid: *const u64,
    reason_type: i32,
    reason_str: *const c_char,
) {
    extern "C" {
        fn process_exit(pid: *const u64, reason_type: i32, reason_str: *const c_char);
    }
    unsafe {
        process_exit(pid, reason_type, reason_str);
    }
}

/// Look up a process by name (whereis)
/// name: null-terminated C string
/// Returns pointer to u64 containing ProcessId, or null if not found
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_whereis(name: *const c_char) -> *mut c_void {
    extern "C" {
        fn process_whereis(name: *const c_char) -> *mut c_void;
    }
    unsafe { process_whereis(name) }
}

/// Register a process with a name
/// pid: pointer to u64 containing ProcessId
/// name: null-terminated C string
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_register(pid: *const u64, name: *const c_char) -> i32 {
    extern "C" {
        fn process_register(pid: *const u64, name: *const c_char) -> i32;
    }
    unsafe { process_register(pid, name) }
}

/// Unregister a process name
/// name: null-terminated C string
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_unregister(name: *const c_char) {
    extern "C" {
        fn process_unregister(name: *const c_char);
    }
    unsafe {
        process_unregister(name);
    }
}

/// Link two processes
/// pid1: pointer to u64 containing first ProcessId
/// pid2: pointer to u64 containing second ProcessId
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_link(pid1: *const u64, pid2: *const u64) -> i32 {
    extern "C" {
        fn process_link(pid1: *const u64, pid2: *const u64) -> i32;
    }
    unsafe { process_link(pid1, pid2) }
}

/// Unlink two processes
/// pid1: pointer to u64 containing first ProcessId
/// pid2: pointer to u64 containing second ProcessId
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_unlink(pid1: *const u64, pid2: *const u64) {
    extern "C" {
        fn process_unlink(pid1: *const u64, pid2: *const u64);
    }
    unsafe {
        process_unlink(pid1, pid2);
    }
}

/// Monitor a process
/// monitor_pid: pointer to u64 containing monitor ProcessId
/// target_pid: pointer to u64 containing target ProcessId
/// Returns pointer to u64 containing MonitorRef, or null on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_monitor(
    monitor_pid: *const u64,
    target_pid: *const u64,
) -> *mut c_void {
    extern "C" {
        fn process_monitor(monitor_pid: *const u64, target_pid: *const u64) -> *mut c_void;
    }
    unsafe { process_monitor(monitor_pid, target_pid) }
}

/// Remove a monitor
/// monitor_pid: pointer to u64 containing monitor ProcessId
/// monitor_ref: pointer to u64 containing MonitorRef
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_demonitor(
    monitor_pid: *const u64,
    monitor_ref: *const u64,
) -> i32 {
    extern "C" {
        fn process_demonitor(monitor_pid: *const u64, monitor_ref: *const u64) -> i32;
    }
    unsafe { process_demonitor(monitor_pid, monitor_ref) }
}

/// Mark a process as waiting for a promise
/// pid: pointer to u64 containing ProcessId
/// promise: pointer to promise object
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_wait_for_promise(pid: *const u64, promise: *mut c_void) {
    extern "C" {
        fn process_wait_for_promise(pid: *const u64, promise: *mut c_void);
    }
    unsafe {
        process_wait_for_promise(pid, promise);
    }
}

/// Mark a process as waiting for a timeout
/// pid: pointer to u64 containing ProcessId
/// timeout_ms: timeout in milliseconds
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_wait_for_timeout(pid: *const u64, timeout_ms: u64) {
    extern "C" {
        fn process_wait_for_timeout(pid: *const u64, timeout_ms: u64);
    }
    unsafe {
        process_wait_for_timeout(pid, timeout_ms);
    }
}

/// Run the process scheduler (execute one process step)
/// Returns 1 if a process was executed, 0 if no runnable processes
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_scheduler_run() -> i32 {
    extern "C" {
        fn process_scheduler_run() -> i32;
    }
    unsafe { process_scheduler_run() }
}

/// Check waiting promises and resume processes whose promises are ready
/// This should be called periodically by the executor
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_check_promises() {
    extern "C" {
        fn process_check_promises();
    }
    unsafe {
        process_check_promises();
    }
}

/// Check expired timeouts and wake up processes
/// This should be called periodically by the scheduler
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_check_timeouts() {
    extern "C" {
        fn process_check_timeouts();
    }
    unsafe {
        process_check_timeouts();
    }
}

// ========== Supervisor Functions ==========

/// Register a supervisor with configuration
/// supervisor_pid: pointer to u64 containing supervisor ProcessId
/// strategy: 0=OneForOne, 1=OneForAll, 2=RestForOne
/// max_restarts: maximum number of restarts in time window
/// max_seconds: time window in seconds
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_register_supervisor(
    supervisor_pid: *const u64,
    strategy: i32,
    max_restarts: u32,
    max_seconds: u64,
) -> i32 {
    extern "C" {
        fn process_register_supervisor(
            supervisor_pid: *const u64,
            strategy: i32,
            max_restarts: u32,
            max_seconds: u64,
        ) -> i32;
    }
    unsafe { process_register_supervisor(supervisor_pid, strategy, max_restarts, max_seconds) }
}

/// Add a child to a supervisor
/// supervisor_pid: pointer to u64 containing supervisor ProcessId
/// child_pid: pointer to u64 containing child ProcessId
/// child_id: null-terminated C string for child ID
/// restart: 0=Permanent, 1=Transient, 2=Temporary
/// shutdown_timeout: shutdown timeout in milliseconds
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_add_supervisor_child(
    supervisor_pid: *const u64,
    child_pid: *const u64,
    child_id: *const c_char,
    restart: i32,
    shutdown_timeout: u64,
) -> i32 {
    extern "C" {
        fn process_add_supervisor_child(
            supervisor_pid: *const u64,
            child_pid: *const u64,
            child_id: *const c_char,
            restart: i32,
            shutdown_timeout: u64,
        ) -> i32;
    }
    unsafe {
        process_add_supervisor_child(supervisor_pid, child_pid, child_id, restart, shutdown_timeout)
    }
}

/// Set maximum number of processes
/// max: maximum number of processes (0 = use default)
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_set_max_processes(max: usize) {
    extern "C" {
        fn process_set_max_processes(max: usize);
    }
    unsafe {
        process_set_max_processes(max);
    }
}

/// Get current process count
/// Returns current number of processes
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_get_count() -> usize {
    extern "C" {
        fn process_get_count() -> usize;
    }
    unsafe { process_get_count() }
}

// ========== Multi-threaded Scheduler Functions ==========

/// Initialize multi-threaded scheduler
/// num_threads: number of worker threads (0 = use CPU count)
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_scheduler_init_multi_threaded(num_threads: usize) -> i32 {
    extern "C" {
        fn process_scheduler_init_multi_threaded(num_threads: usize) -> i32;
    }
    unsafe { process_scheduler_init_multi_threaded(num_threads) }
}

/// Stop multi-threaded scheduler
#[unsafe(no_mangle)]
pub extern "C" fn oats_std_actor_scheduler_stop() {
    extern "C" {
        fn process_scheduler_stop();
    }
    unsafe {
        process_scheduler_stop();
    }
}

