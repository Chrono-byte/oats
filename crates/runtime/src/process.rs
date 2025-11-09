//! Process execution and management
//!
//! This module implements an Elixir/Erlang-inspired process model with:
//! - Lightweight isolated execution contexts
//! - Asynchronous message passing
//! - Process supervision and fault tolerance
//! - Integration with async/await

use libc::c_void;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::thread;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::{promise_poll_into, rc_dec, rc_inc, runtime_free, runtime_malloc};

/// Process identifier - opaque u64 value
pub type ProcessId = u64;

/// Process status enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessStatus {
    Running,    // Process is executing or ready to execute
    Waiting,    // Process is waiting for a message or async operation
    Terminated, // Process has exited
}

/// Process priority levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProcessPriority {
    Normal = 0, // Normal priority
    High = 1,   // High priority (for supervisors)
}

/// Exit reason for a process
#[derive(Debug, Clone)]
pub enum ExitReason {
    Normal,           // Normal termination
    Error(String),    // Error termination with reason
    Kill(String),     // Killed by supervisor
    Shutdown(String), // Shutdown requested
}

/// Message structure for inter-process communication
#[derive(Debug)]
pub struct Message {
    pub from: ProcessId,
    pub payload: usize, // Pointer to ARC-managed heap object (stored as usize for Send safety)
    pub payload_type_id: u64, // Type identifier for runtime type checking
    pub timestamp: u64, // Unix timestamp in milliseconds
}

/// Message queue for a process mailbox
pub struct MessageQueue {
    pub messages: VecDeque<Message>,
}

impl Default for MessageQueue {
    fn default() -> Self {
        Self::new()
    }
}

impl MessageQueue {
    pub fn new() -> Self {
        Self {
            messages: VecDeque::new(),
        }
    }

    pub fn enqueue(&mut self, msg: Message) {
        self.messages.push_back(msg);
    }

    pub fn dequeue(&mut self) -> Option<Message> {
        self.messages.pop_front()
    }

    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }

    pub fn len(&self) -> usize {
        self.messages.len()
    }
}

/// Wait reason for a process
#[derive(Debug, Clone)]
pub enum WaitReason {
    WaitingForMessage,        // Blocked on receive()
    WaitingForPromise(usize), // Blocked on await (promise address as usize)
    WaitingForTimeout(u64),   // Blocked on receive() with timeout
}

/// Timer entry for timeout tracking
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Timer {
    deadline: u64, // Timestamp when timeout expires
    process_id: ProcessId,
}

impl Timer {
    fn new(deadline: u64, process_id: ProcessId) -> Self {
        Self {
            deadline,
            process_id,
        }
    }
}

/// Process structure - internal runtime representation
pub struct Process {
    // Process metadata
    pub id: ProcessId,
    pub name: Option<String>,
    pub status: ProcessStatus,
    pub priority: ProcessPriority,

    // Execution context
    pub mailbox: MessageQueue,
    pub state: Option<usize>, // Process-specific state (type-erased, stored as usize for Send safety)

    // Supervision
    pub supervisor: Option<ProcessId>,
    pub children: Vec<ProcessId>,

    // Linking and monitoring
    pub links: Vec<ProcessId>,    // Processes linked to this one
    pub monitors: Vec<ProcessId>, // Processes monitoring this one
    pub monitor_refs: HashMap<u64, ProcessId>, // MonitorRef -> ProcessId mapping

    // Lifecycle
    pub spawn_time: u64,
    pub last_message_time: u64,

    // Error handling
    pub exit_reason: Option<ExitReason>,

    // Async integration
    pub current_promise: Option<usize>, // Current awaited promise (if any, stored as usize for Send safety)
}

impl Process {
    pub fn new(id: ProcessId) -> Self {
        let now = current_timestamp();
        Self {
            id,
            name: None,
            status: ProcessStatus::Running,
            priority: ProcessPriority::Normal,
            mailbox: MessageQueue::new(),
            state: None,
            supervisor: None,
            children: Vec::new(),
            links: Vec::new(),
            monitors: Vec::new(),
            monitor_refs: HashMap::new(),
            spawn_time: now,
            last_message_time: now,
            exit_reason: None,
            current_promise: None,
        }
    }

    pub fn with_name(id: ProcessId, name: String) -> Self {
        let mut proc = Self::new(id);
        proc.name = Some(name);
        proc
    }

    pub fn with_state(id: ProcessId, state: *mut c_void) -> Self {
        let mut proc = Self::new(id);
        proc.state = Some(state as usize);
        proc
    }
}

/// Restart strategy for supervisors
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RestartStrategy {
    OneForOne,  // Only restart the failed child
    OneForAll,  // Restart all children when one fails
    RestForOne, // Restart the failed child and all children started after it
}

/// Child restart policy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RestartPolicy {
    Permanent, // Always restart
    Transient, // Restart only on abnormal exit
    Temporary, // Never restart
}

/// Child specification for supervisors
#[derive(Debug, Clone)]
pub struct ChildSpec {
    pub id: String,
    pub pid: ProcessId,
    pub restart: RestartPolicy,
    pub shutdown_timeout: u64, // milliseconds
}

/// Supervisor configuration
#[derive(Debug, Clone)]
pub struct SupervisorConfig {
    pub strategy: RestartStrategy,
    pub max_restarts: u32,
    pub max_seconds: u64,
    pub children: Vec<ChildSpec>,
}

/// Process scheduler - manages process execution
pub struct ProcessScheduler {
    runnable_normal: VecDeque<ProcessId>, // Normal priority processes ready to run
    runnable_high: VecDeque<ProcessId>,   // High priority processes ready to run
    waiting: HashMap<ProcessId, WaitReason>, // Processes waiting (message or promise)
    processes: HashMap<ProcessId, Process>, // All processes
    timer_heap: BinaryHeap<Reverse<Timer>>, // Timeouts for receive() with timeout (min-heap via Reverse)
    registry: HashMap<String, ProcessId>,   // Name -> ProcessId mapping
    next_pid: AtomicU64,                    // Next process ID to assign
    process_count: AtomicUsize,             // Current number of processes
    max_processes: AtomicUsize,             // Maximum number of processes (default: 1 million)
    supervisors: HashMap<ProcessId, SupervisorConfig>, // Supervisor configurations
    restart_counts: HashMap<ProcessId, Vec<(u64, u32)>>, // (timestamp, count) for restart tracking
}

// BinaryHeap is a max-heap, but we want a min-heap for timers
// So we use Reverse wrapper
impl PartialOrd for Timer {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Timer {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Normal order: earlier deadlines come first
        self.deadline
            .cmp(&other.deadline)
            .then_with(|| self.process_id.cmp(&other.process_id))
    }
}

impl Default for ProcessScheduler {
    fn default() -> Self {
        Self::new()
    }
}

impl ProcessScheduler {
    pub fn new() -> Self {
        Self {
            runnable_normal: VecDeque::new(),
            runnable_high: VecDeque::new(),
            waiting: HashMap::new(),
            processes: HashMap::new(),
            timer_heap: BinaryHeap::new(), // Will store Reverse<Timer> for min-heap behavior
            registry: HashMap::new(),
            next_pid: AtomicU64::new(1), // Start from 1, 0 is invalid
            process_count: AtomicUsize::new(0),
            max_processes: AtomicUsize::new(1_000_000), // Default: 1 million processes
            supervisors: HashMap::new(),
            restart_counts: HashMap::new(),
        }
    }

    /// Set maximum number of processes
    pub fn set_max_processes(&mut self, max: usize) {
        self.max_processes.store(max, Ordering::Relaxed);
    }

    /// Get current process count
    pub fn process_count(&self) -> usize {
        self.process_count.load(Ordering::Relaxed)
    }

    /// Allocate a new process ID
    pub fn allocate_pid(&self) -> ProcessId {
        self.next_pid.fetch_add(1, Ordering::Relaxed)
    }

    /// Spawn a new process
    pub fn spawn(
        &mut self,
        name: Option<String>,
        state: Option<*mut c_void>,
        priority: ProcessPriority,
    ) -> Option<ProcessId> {
        // Check process limit
        let current = self.process_count.load(Ordering::Relaxed);
        let max = self.max_processes.load(Ordering::Relaxed);
        if current >= max {
            return None; // Process limit exceeded
        }

        let pid = self.allocate_pid();
        let mut process = if let Some(name_str) = name {
            Process::with_name(pid, name_str)
        } else if let Some(state_ptr) = state {
            Process::with_state(pid, state_ptr)
        } else {
            Process::new(pid)
        };
        process.priority = priority;

        // Register name if provided
        if let Some(ref name_str) = process.name {
            self.registry.insert(name_str.clone(), pid);
        }

        // Increment ARC on state if provided
        if let Some(state_ptr) = state {
            unsafe {
                rc_inc(state_ptr);
            }
            process.state = Some(state_ptr as usize);
        }

        self.processes.insert(pid, process);
        self.process_count.fetch_add(1, Ordering::Relaxed);

        // Add to appropriate priority queue
        match priority {
            ProcessPriority::High => self.runnable_high.push_back(pid),
            ProcessPriority::Normal => self.runnable_normal.push_back(pid),
        }

        Some(pid)
    }

    /// Send a message to a process
    pub fn send(
        &mut self,
        to: ProcessId,
        from: ProcessId,
        payload: *mut c_void,
        type_id: u64,
    ) -> bool {
        if let Some(process) = self.processes.get_mut(&to) {
            if process.status == ProcessStatus::Terminated {
                // Process is dead, decrement payload ARC
                unsafe {
                    rc_dec(payload);
                }
                return false;
            }

            // Increment ARC on payload (if not STATIC)
            unsafe {
                rc_inc(payload);
            }

            let msg = Message {
                from,
                payload: payload as usize,
                payload_type_id: type_id,
                timestamp: current_timestamp(),
            };

            process.mailbox.enqueue(msg);
            process.last_message_time = current_timestamp();

            // If process is waiting for a message, wake it up
            if let Some(WaitReason::WaitingForMessage) = self.waiting.get(&to) {
                self.waiting.remove(&to);
                process.status = ProcessStatus::Running;
                // Add to appropriate priority queue
                match process.priority {
                    ProcessPriority::High => self.runnable_high.push_back(to),
                    ProcessPriority::Normal => self.runnable_normal.push_back(to),
                }
            }

            true
        } else {
            // Process doesn't exist, decrement payload ARC
            unsafe {
                rc_dec(payload);
            }
            false
        }
    }

    /// Send a message to a named process
    pub fn send_to_name(
        &mut self,
        name: &str,
        from: ProcessId,
        payload: *mut c_void,
        type_id: u64,
    ) -> bool {
        if let Some(&pid) = self.registry.get(name) {
            self.send(pid, from, payload, type_id)
        } else {
            // Process not found, decrement payload ARC
            unsafe {
                rc_dec(payload);
            }
            false
        }
    }

    /// Receive a message from the process mailbox
    pub fn receive(&mut self, pid: ProcessId, type_id: Option<u64>) -> Option<Message> {
        if let Some(process) = self.processes.get_mut(&pid) {
            // Check if there's a matching message
            if let Some(msg) = process.mailbox.dequeue() {
                // If type_id is specified, check for match
                if let Some(expected_type_id) = type_id {
                    if msg.payload_type_id == expected_type_id {
                        return Some(msg);
                    } else {
                        // Type mismatch, put message back at front
                        process.mailbox.messages.push_front(msg);
                        return None;
                    }
                }
                return Some(msg);
            }
        }
        None
    }

    /// Mark a process as waiting for a message
    pub fn wait_for_message(&mut self, pid: ProcessId) {
        if let Some(process) = self.processes.get_mut(&pid) {
            process.status = ProcessStatus::Waiting;
            self.waiting.insert(pid, WaitReason::WaitingForMessage);
        }
    }

    /// Mark a process as waiting for a promise
    pub fn wait_for_promise(&mut self, pid: ProcessId, promise: *mut c_void) {
        if let Some(process) = self.processes.get_mut(&pid) {
            process.status = ProcessStatus::Waiting;
            process.current_promise = Some(promise as usize);
            self.waiting
                .insert(pid, WaitReason::WaitingForPromise(promise as usize));
        }
    }

    /// Mark a process as waiting for a timeout
    pub fn wait_for_timeout(&mut self, pid: ProcessId, timeout_ms: u64) {
        if let Some(process) = self.processes.get_mut(&pid) {
            process.status = ProcessStatus::Waiting;
            let deadline = current_timestamp() + timeout_ms;
            self.timer_heap.push(Reverse(Timer::new(deadline, pid)));
            self.waiting
                .insert(pid, WaitReason::WaitingForTimeout(deadline));
        }
    }

    /// Resume a process (move from waiting to runnable)
    pub fn resume(&mut self, pid: ProcessId) {
        if let Some(process) = self.processes.get_mut(&pid)
            && process.status == ProcessStatus::Waiting
        {
            process.status = ProcessStatus::Running;
            self.waiting.remove(&pid);
            // Add to appropriate priority queue
            match process.priority {
                ProcessPriority::High => self.runnable_high.push_back(pid),
                ProcessPriority::Normal => self.runnable_normal.push_back(pid),
            }
        }
    }

    /// Link two processes (bidirectional)
    pub fn link(&mut self, pid1: ProcessId, pid2: ProcessId) -> bool {
        if pid1 == pid2 {
            return false; // Can't link to self
        }
        if !self.processes.contains_key(&pid1) || !self.processes.contains_key(&pid2) {
            return false;
        }
        // We need to borrow separately to avoid multiple mutable borrows
        if let Some(p1) = self.processes.get_mut(&pid1)
            && !p1.links.contains(&pid2)
        {
            p1.links.push(pid2);
        }
        if let Some(p2) = self.processes.get_mut(&pid2)
            && !p2.links.contains(&pid1)
        {
            p2.links.push(pid1);
        }
        true
    }

    /// Unlink two processes
    pub fn unlink(&mut self, pid1: ProcessId, pid2: ProcessId) {
        if let Some(p1) = self.processes.get_mut(&pid1) {
            p1.links.retain(|&p| p != pid2);
        }
        if let Some(p2) = self.processes.get_mut(&pid2) {
            p2.links.retain(|&p| p != pid1);
        }
    }

    /// Monitor a process (one-way monitoring)
    /// Returns a monitor reference (u64)
    pub fn monitor(&mut self, monitor_pid: ProcessId, target_pid: ProcessId) -> Option<u64> {
        if monitor_pid == target_pid {
            return None; // Can't monitor self
        }
        if let Some(target) = self.processes.get_mut(&target_pid) {
            if !target.monitors.contains(&monitor_pid) {
                target.monitors.push(monitor_pid);
            }
            // Generate a unique monitor reference
            let monitor_ref = self.next_pid.fetch_add(1, Ordering::Relaxed);
            if let Some(monitor) = self.processes.get_mut(&monitor_pid) {
                monitor.monitor_refs.insert(monitor_ref, target_pid);
            }
            Some(monitor_ref)
        } else {
            None
        }
    }

    /// Remove a monitor
    pub fn demonitor(&mut self, monitor_pid: ProcessId, monitor_ref: u64) -> bool {
        if let Some(monitor) = self.processes.get_mut(&monitor_pid) {
            if let Some(target_pid) = monitor.monitor_refs.remove(&monitor_ref) {
                if let Some(target) = self.processes.get_mut(&target_pid) {
                    target.monitors.retain(|&p| p != monitor_pid);
                }
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Create an exit notification message payload
    /// Returns a pointer to a heap-allocated object representing {down, pid, reason}
    fn create_exit_message(&self, exited_pid: ProcessId, _reason: &ExitReason) -> *mut c_void {
        // Allocate object: [header (8)][meta (8)][pid (8)][reason_str_ptr (8)]
        // For simplicity, we'll create a minimal structure
        // In a real implementation, this would create a proper Oats object with fields
        let size = 8 + 8 + 8 + 8; // header + meta + pid + reason_ptr
        let mem = runtime_malloc(size) as *mut u8;
        if mem.is_null() {
            return std::ptr::null_mut();
        }

        // Initialize header (refcount=1)
        unsafe {
            let header = mem as *mut u64;
            *header = 1; // refcount = 1

            // Store PID at offset 16
            let pid_ptr = mem.add(16) as *mut u64;
            *pid_ptr = exited_pid;

            // For now, we'll use a simple approach - in real implementation,
            // we'd create a proper Oats string object
            // Store null for now - the codegen will handle proper message construction
            let reason_ptr = mem.add(24) as *mut *mut c_void;
            *reason_ptr = std::ptr::null_mut();
        }

        mem as *mut c_void
    }

    /// Notify linked and monitored processes of an exit
    fn notify_exit(&mut self, exited_pid: ProcessId, reason: &ExitReason) {
        // Collect PIDs to notify first to avoid borrowing issues
        let (links, monitors, monitor_refs) = if let Some(exited) = self.processes.get(&exited_pid)
        {
            (
                exited.links.clone(),
                exited.monitors.clone(),
                exited.monitor_refs.clone(),
            )
        } else {
            return;
        };

        // Create exit message payload
        let exit_msg = self.create_exit_message(exited_pid, reason);
        if exit_msg.is_null() {
            return; // Failed to create message
        }

        // Track successful sends to ensure message is properly cleaned up
        let mut successful_sends = 0;

        // Notify linked processes and remove links
        for linked_pid in links {
            // Remove bidirectional link
            if let Some(linked) = self.processes.get_mut(&linked_pid) {
                linked.links.retain(|&p| p != exited_pid);
            }
            // Send exit notification message
            // Type ID 0xDEADBEEF for exit notifications (temporary - codegen will use proper types)
            if self.send(exited_pid, linked_pid, exit_msg, 0xDEADBEEF) {
                successful_sends += 1;
            }
        }

        // Notify monitoring processes with monitor refs
        for monitor_pid in monitors {
            // Find monitor ref for this process (if any)
            let _monitor_ref = monitor_refs
                .iter()
                .find(|(_, target_pid)| **target_pid == exited_pid)
                .map(|(ref_val, _)| *ref_val);

            // Create message with monitor ref (similar structure but with ref field)
            // For now, send the same message - codegen will handle proper structure
            if self.send(exited_pid, monitor_pid, exit_msg, 0xDEADBEEF) {
                successful_sends += 1;
            }
        }

        // If no successful sends, decrement the message reference count
        // (it was created with RC=1, and each successful send increments it)
        // If all receivers are dead/terminated, the message would leak otherwise
        if successful_sends == 0 {
            unsafe {
                rc_dec(exit_msg);
            }
        }
    }

    /// Exit a process
    pub fn exit(&mut self, pid: ProcessId, reason: ExitReason) {
        // Collect supervisor before getting mutable borrow
        let supervisor_pid = if let Some(process) = self.processes.get(&pid) {
            process.supervisor
        } else {
            return;
        };

        // Set status and exit reason
        {
            if let Some(process) = self.processes.get_mut(&pid) {
                process.status = ProcessStatus::Terminated;
                process.exit_reason = Some(reason.clone());
            }
        }

        // Notify supervisor if this is a supervised child
        if let Some(sup_pid) = supervisor_pid {
            // Handle child exit in supervisor
            let _should_restart = self.handle_child_exit(sup_pid, pid, &reason);
            // If restart is needed, supervisor will handle it
            // For now, we just notify - the actual restart happens in supervisor code
        }

        // Notify linked and monitored processes (after releasing mutable borrow)
        self.notify_exit(pid, &reason);

        // Get mutable borrow again for cleanup
        // Process should exist at this point, but handle gracefully if it doesn't
        let Some(process) = self.processes.get_mut(&pid) else {
            // Process was already removed - this shouldn't happen but handle gracefully
            return;
        };

        // Cleanup: decrement ARC on all messages in mailbox
        while let Some(msg) = process.mailbox.dequeue() {
            let payload = msg.payload as *mut c_void;
            unsafe {
                rc_dec(payload);
            }
        }

        // Cleanup: decrement ARC on state
        if let Some(state_addr) = process.state {
            let state = state_addr as *mut c_void;
            unsafe {
                rc_dec(state);
            }
        }

        // Remove from waiting/runnable queues
        self.waiting.remove(&pid);
        self.runnable_normal.retain(|&p| p != pid);
        self.runnable_high.retain(|&p| p != pid);
        self.process_count.fetch_sub(1, Ordering::Relaxed);

        // Remove from registry if named
        if let Some(ref name) = process.name {
            self.registry.remove(name);
        }

        // Clean up links and monitors from other processes
        let links = process.links.clone();
        let monitors = process.monitors.clone();
        for linked_pid in links {
            if let Some(linked) = self.processes.get_mut(&linked_pid) {
                linked.links.retain(|&p| p != pid);
            }
        }
        for monitor_pid in monitors {
            if let Some(monitor) = self.processes.get_mut(&monitor_pid) {
                monitor.monitors.retain(|&p| p != pid);
                // Remove monitor refs pointing to this process
                monitor.monitor_refs.retain(|_, &mut target| target != pid);
            }
        }
    }

    /// Get the current process ID (for self())
    pub fn get_current_pid(&self) -> Option<ProcessId> {
        // This will be set by the runtime when executing process code
        // For now, return None - this needs thread-local storage
        None
    }

    /// Look up a process by name
    pub fn whereis(&self, name: &str) -> Option<ProcessId> {
        self.registry.get(name).copied()
    }

    /// Register a process name
    pub fn register(&mut self, name: String, pid: ProcessId) -> bool {
        if self.registry.contains_key(&name) {
            return false; // Name already registered
        }
        if let Some(process) = self.processes.get_mut(&pid) {
            process.name = Some(name.clone());
            self.registry.insert(name, pid);
            true
        } else {
            false
        }
    }

    /// Unregister a process name
    pub fn unregister(&mut self, name: &str) {
        if let Some(pid) = self.registry.remove(name)
            && let Some(process) = self.processes.get_mut(&pid)
        {
            process.name = None;
        }
    }

    /// Get next runnable process (prioritizes high priority)
    pub fn next_runnable(&mut self) -> Option<ProcessId> {
        // First check high priority queue
        self.runnable_high
            .pop_front()
            .or_else(|| self.runnable_normal.pop_front())
    }

    /// Check and process expired timeouts
    pub fn check_timeouts(&mut self) {
        let now = current_timestamp();
        while let Some(Reverse(timer)) = self.timer_heap.peek() {
            if timer.deadline > now {
                break; // No more expired timers
            }
            // Pop the timer we just peeked - should always succeed after peek
            let Some(Reverse(timer)) = self.timer_heap.pop() else {
                // Heap was modified between peek and pop - break to avoid infinite loop
                break;
            };
            if let Some(WaitReason::WaitingForTimeout(_)) = self.waiting.get(&timer.process_id) {
                // Timeout expired, wake up process
                self.resume(timer.process_id);
            }
        }
    }

    /// Check waiting promises and resume processes whose promises are ready
    /// This is called by the executor when a promise becomes ready
    pub fn check_promises(&mut self) {
        // Collect waiting process IDs and their promises
        // Note: We clone the PIDs and promises to avoid holding references while mutating
        let waiting_pids: Vec<(ProcessId, usize)> = self
            .waiting
            .iter()
            .filter_map(|(pid, reason)| {
                if let WaitReason::WaitingForPromise(promise_addr) = reason {
                    Some((*pid, *promise_addr))
                } else {
                    None
                }
            })
            .collect();

        for (pid, promise_addr) in waiting_pids {
            let promise = promise_addr as *mut c_void;
            let out_mem = runtime_malloc(std::mem::size_of::<*mut c_void>()) as *mut u8;
            if out_mem.is_null() {
                continue;
            }
            let ready = promise_poll_into(promise, out_mem as *mut c_void);
            if ready != 0 {
                // Promise is ready, resume the process
                unsafe {
                    runtime_free(out_mem as *mut c_void);
                }
                if let Some(WaitReason::WaitingForPromise(_)) = self.waiting.get(&pid) {
                    self.resume(pid);
                }
            } else {
                unsafe {
                    runtime_free(out_mem as *mut c_void);
                }
            }
        }
    }

    /// Get a process (mutable)
    pub fn get_process_mut(&mut self, pid: ProcessId) -> Option<&mut Process> {
        self.processes.get_mut(&pid)
    }

    /// Get a process (immutable)
    pub fn get_process(&self, pid: ProcessId) -> Option<&Process> {
        self.processes.get(&pid)
    }

    // ========== Supervisor Methods ==========

    /// Register a supervisor with configuration
    pub fn register_supervisor(
        &mut self,
        supervisor_pid: ProcessId,
        config: SupervisorConfig,
    ) -> bool {
        if !self.processes.contains_key(&supervisor_pid) {
            return false;
        }
        // Set supervisor priority to High
        if let Some(process) = self.processes.get_mut(&supervisor_pid) {
            process.priority = ProcessPriority::High;
            process.supervisor = None; // Supervisor has no supervisor
        }
        self.supervisors.insert(supervisor_pid, config);
        true
    }

    /// Add a child to a supervisor
    pub fn add_supervisor_child(
        &mut self,
        supervisor_pid: ProcessId,
        child_spec: ChildSpec,
    ) -> bool {
        if let Some(config) = self.supervisors.get_mut(&supervisor_pid) {
            // Set child's supervisor
            if let Some(child) = self.processes.get_mut(&child_spec.pid) {
                child.supervisor = Some(supervisor_pid);
            }
            // Add to supervisor's children list
            if let Some(supervisor) = self.processes.get_mut(&supervisor_pid) {
                supervisor.children.push(child_spec.pid);
            }
            config.children.push(child_spec);
            true
        } else {
            false
        }
    }

    /// Handle child process exit (called by supervisor)
    pub fn handle_child_exit(
        &mut self,
        supervisor_pid: ProcessId,
        child_pid: ProcessId,
        reason: &ExitReason,
    ) -> bool {
        if let Some(config) = self.supervisors.get_mut(&supervisor_pid) {
            // Find child spec
            if let Some(child_spec) = config.children.iter_mut().find(|c| c.pid == child_pid) {
                // Check restart policy
                let should_restart = match child_spec.restart {
                    RestartPolicy::Permanent => true,
                    RestartPolicy::Transient => !matches!(reason, ExitReason::Normal),
                    RestartPolicy::Temporary => false,
                };

                if !should_restart {
                    // Remove child from supervisor
                    config.children.retain(|c| c.pid != child_pid);
                    if let Some(supervisor) = self.processes.get_mut(&supervisor_pid) {
                        supervisor.children.retain(|&p| p != child_pid);
                    }
                    return false;
                }

                // Check restart limits
                let now = current_timestamp();
                let restart_window_start = now.saturating_sub(config.max_seconds * 1000);

                // Clean old restart counts
                let counts = self.restart_counts.entry(child_pid).or_default();
                counts.retain(|(ts, _)| *ts > restart_window_start);

                // Count restarts in window
                let restart_count: u32 = counts.iter().map(|(_, count)| count).sum();

                if restart_count >= config.max_restarts {
                    // Too many restarts - shutdown supervisor
                    self.exit(
                        supervisor_pid,
                        ExitReason::Error("max_restarts_exceeded".to_string()),
                    );
                    return false;
                }

                // Record restart
                counts.push((now, 1));

                // Apply restart strategy
                match config.strategy {
                    RestartStrategy::OneForOne => {
                        // Restart only this child
                        // The supervisor will handle the actual restart via spawn_child
                        return true;
                    }
                    RestartStrategy::OneForAll => {
                        // Restart all children
                        let child_pids: Vec<ProcessId> =
                            config.children.iter().map(|c| c.pid).collect();
                        for pid in child_pids {
                            if pid != child_pid {
                                self.exit(pid, ExitReason::Kill("one_for_all_restart".to_string()));
                            }
                        }
                        return true;
                    }
                    RestartStrategy::RestForOne => {
                        // Restart this child and all started after it
                        let mut restart_after = false;
                        let child_pids: Vec<ProcessId> = config
                            .children
                            .iter()
                            .filter_map(|c| {
                                if c.pid == child_pid {
                                    restart_after = true;
                                    None
                                } else if restart_after {
                                    Some(c.pid)
                                } else {
                                    None
                                }
                            })
                            .collect();
                        for pid in child_pids {
                            self.exit(pid, ExitReason::Kill("rest_for_one_restart".to_string()));
                        }
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Restart a child process (creates new PID, supervisor must update child spec)
    pub fn restart_child(
        &mut self,
        supervisor_pid: ProcessId,
        old_child_pid: ProcessId,
        _start_fn: fn() -> ProcessId,
    ) -> Option<ProcessId> {
        // Remove old child from supervisor
        if let Some(config) = self.supervisors.get_mut(&supervisor_pid) {
            config.children.retain(|c| c.pid != old_child_pid);
            if let Some(supervisor) = self.processes.get_mut(&supervisor_pid) {
                supervisor.children.retain(|&p| p != old_child_pid);
            }
        }

        // Spawn new child (this would be called by supervisor code)
        // For now, return None - the actual restart is handled by supervisor code
        None
    }
}

/// Get current timestamp in milliseconds since Unix epoch
fn current_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_millis() as u64
}

/// Global process scheduler instance
static SCHEDULER: OnceLock<Arc<Mutex<ProcessScheduler>>> = OnceLock::new();

fn init_scheduler() -> Arc<Mutex<ProcessScheduler>> {
    SCHEDULER
        .get_or_init(|| Arc::new(Mutex::new(ProcessScheduler::new())))
        .clone()
}

// Thread-local storage for current process ID
thread_local! {
    static CURRENT_PID: std::cell::RefCell<Option<ProcessId>> = const { std::cell::RefCell::new(None) };
}

/// Set the current process ID (called by runtime when executing process code)
pub fn set_current_pid(pid: ProcessId) {
    CURRENT_PID.with(|p| {
        *p.borrow_mut() = Some(pid);
    });
}

/// Get the current process ID
pub fn get_current_pid() -> Option<ProcessId> {
    CURRENT_PID.with(|p| *p.borrow())
}

/// Clear the current process ID
pub fn clear_current_pid() {
    CURRENT_PID.with(|p| {
        *p.borrow_mut() = None;
    });
}

// FFI Functions

/// Spawn a new process
/// priority: 0=Normal, 1=High
/// Returns ProcessId (u64) as a pointer to a heap-allocated u64, or null if limit exceeded
#[unsafe(no_mangle)]
pub extern "C" fn process_spawn(priority: i32) -> *mut c_void {
    let scheduler = init_scheduler();
    let Ok(mut sched) = scheduler.lock() else {
        return std::ptr::null_mut();
    };
    let proc_priority = if priority == 1 {
        ProcessPriority::High
    } else {
        ProcessPriority::Normal
    };
    let pid = match sched.spawn(None, None, proc_priority) {
        Some(pid) => pid,
        None => return std::ptr::null_mut(), // Process limit exceeded
    };

    // Allocate a u64 on the heap to return the PID
    unsafe {
        let size = std::mem::size_of::<u64>();
        let mem = runtime_malloc(size) as *mut u64;
        if mem.is_null() {
            return std::ptr::null_mut();
        }
        *mem = pid;
        mem as *mut c_void
    }
}

/// Spawn a new process with a name
/// name_ptr: pointer to null-terminated C string (caller retains ownership)
/// priority: 0=Normal, 1=High
/// Returns ProcessId (u64) as a pointer to a heap-allocated u64, or null if limit exceeded
#[unsafe(no_mangle)]
pub extern "C" fn process_spawn_named(name_ptr: *const libc::c_char, priority: i32) -> *mut c_void {
    if name_ptr.is_null() {
        return process_spawn(priority);
    }

    let name = unsafe {
        match std::ffi::CStr::from_ptr(name_ptr).to_str() {
            Ok(s) => s.to_string(),
            Err(_) => return std::ptr::null_mut(),
        }
    };

    let scheduler = init_scheduler();
    let Ok(mut sched) = scheduler.lock() else {
        return std::ptr::null_mut();
    };
    let proc_priority = if priority == 1 {
        ProcessPriority::High
    } else {
        ProcessPriority::Normal
    };
    let pid = match sched.spawn(Some(name), None, proc_priority) {
        Some(pid) => pid,
        None => return std::ptr::null_mut(), // Process limit exceeded
    };

    unsafe {
        let size = std::mem::size_of::<u64>();
        let mem = runtime_malloc(size) as *mut u64;
        if mem.is_null() {
            return std::ptr::null_mut();
        }
        *mem = pid;
        mem as *mut c_void
    }
}

/// Send a message to a process
/// to_pid_ptr: pointer to u64 containing ProcessId
/// from_pid_ptr: pointer to u64 containing sender ProcessId
/// payload: pointer to ARC-managed heap object (will be incremented)
/// type_id: type identifier for runtime type checking
#[unsafe(no_mangle)]
pub extern "C" fn process_send(
    to_pid_ptr: *const u64,
    from_pid_ptr: *const u64,
    payload: *mut c_void,
    type_id: u64,
) -> i32 {
    if to_pid_ptr.is_null() || from_pid_ptr.is_null() || payload.is_null() {
        return 0;
    }

    let to_pid = unsafe { *to_pid_ptr };
    let from_pid = unsafe { *from_pid_ptr };

    let scheduler = init_scheduler();
    let Ok(mut sched) = scheduler.lock() else {
        unsafe {
            rc_dec(payload);
        }
        return 0;
    };

    if sched.send(to_pid, from_pid, payload, type_id) {
        1
    } else {
        0
    }
}

/// Send a message to a named process
/// name_ptr: pointer to null-terminated C string
/// from_pid_ptr: pointer to u64 containing sender ProcessId
/// payload: pointer to ARC-managed heap object
/// type_id: type identifier
#[unsafe(no_mangle)]
pub extern "C" fn process_send_to_name(
    name_ptr: *const libc::c_char,
    from_pid_ptr: *const u64,
    payload: *mut c_void,
    type_id: u64,
) -> i32 {
    if name_ptr.is_null() || from_pid_ptr.is_null() || payload.is_null() {
        return 0;
    }

    let name = unsafe {
        match std::ffi::CStr::from_ptr(name_ptr).to_str() {
            Ok(s) => s,
            Err(_) => {
                rc_dec(payload);
                return 0;
            }
        }
    };

    let from_pid = unsafe { *from_pid_ptr };

    let scheduler = init_scheduler();
    let Ok(mut sched) = scheduler.lock() else {
        unsafe {
            rc_dec(payload);
        }
        return 0;
    };

    if sched.send_to_name(name, from_pid, payload, type_id) {
        1
    } else {
        0
    }
}

/// Receive a message from the current process's mailbox
/// pid_ptr: pointer to u64 containing ProcessId
/// type_id: optional type identifier (0 means any type)
/// Returns pointer to Message structure, or null if no message
/// Message structure layout: [from: u64][payload: *mut c_void][type_id: u64][timestamp: u64]
#[unsafe(no_mangle)]
pub extern "C" fn process_receive(pid_ptr: *const u64, type_id: u64) -> *mut c_void {
    if pid_ptr.is_null() {
        return std::ptr::null_mut();
    }

    let pid = unsafe { *pid_ptr };
    let type_id_opt = if type_id == 0 { None } else { Some(type_id) };

    let scheduler = init_scheduler();
    let Ok(mut sched) = scheduler.lock() else {
        return std::ptr::null_mut();
    };

    if let Some(msg) = sched.receive(pid, type_id_opt) {
        // Allocate a Message structure on the heap
        // Layout: [from: u64][payload: *mut c_void][type_id: u64][timestamp: u64]
        unsafe {
            let size = std::mem::size_of::<u64>() * 4 + std::mem::size_of::<*mut c_void>();
            let mem = runtime_malloc(size) as *mut u8;
            if mem.is_null() {
                // Allocation failed - decrement payload reference count since message was dequeued
                // The message payload's reference count must be decremented to prevent leak
                let payload = msg.payload as *mut c_void;
                rc_dec(payload);
                return std::ptr::null_mut();
            }

            let from_ptr = mem as *mut u64;
            *from_ptr = msg.from;

            let payload_ptr = mem.add(8) as *mut *mut c_void;
            *payload_ptr = msg.payload as *mut c_void;

            let type_id_ptr = mem.add(16) as *mut u64;
            *type_id_ptr = msg.payload_type_id;

            let timestamp_ptr = mem.add(24) as *mut u64;
            *timestamp_ptr = msg.timestamp;

            mem as *mut c_void
        }
    } else {
        // No message available, mark process as waiting
        sched.wait_for_message(pid);
        std::ptr::null_mut()
    }
}

/// Get the current process ID (self())
/// Returns pointer to u64 containing ProcessId, or null if not in a process context
#[unsafe(no_mangle)]
pub extern "C" fn process_self() -> *mut c_void {
    if let Some(pid) = get_current_pid() {
        let size = std::mem::size_of::<u64>();
        let mem = runtime_malloc(size) as *mut u64;
        if mem.is_null() {
            return std::ptr::null_mut();
        }
        unsafe {
            *mem = pid;
        }
        mem as *mut c_void
    } else {
        std::ptr::null_mut()
    }
}

/// Exit the current process
/// pid_ptr: pointer to u64 containing ProcessId
/// reason_type: 0=normal, 1=error, 2=kill, 3=shutdown
/// reason_str: pointer to null-terminated C string (for error/kill/shutdown reasons)
#[unsafe(no_mangle)]
pub extern "C" fn process_exit(
    pid_ptr: *const u64,
    reason_type: i32,
    reason_str: *const libc::c_char,
) {
    if pid_ptr.is_null() {
        return;
    }

    let pid = unsafe { *pid_ptr };
    let reason = match reason_type {
        0 => ExitReason::Normal,
        1 => {
            let reason = if reason_str.is_null() {
                "unknown".to_string()
            } else {
                unsafe {
                    std::ffi::CStr::from_ptr(reason_str)
                        .to_string_lossy()
                        .to_string()
                }
            };
            ExitReason::Error(reason)
        }
        2 => {
            let reason = if reason_str.is_null() {
                "unknown".to_string()
            } else {
                unsafe {
                    std::ffi::CStr::from_ptr(reason_str)
                        .to_string_lossy()
                        .to_string()
                }
            };
            ExitReason::Kill(reason)
        }
        3 => {
            let reason = if reason_str.is_null() {
                "unknown".to_string()
            } else {
                unsafe {
                    std::ffi::CStr::from_ptr(reason_str)
                        .to_string_lossy()
                        .to_string()
                }
            };
            ExitReason::Shutdown(reason)
        }
        _ => ExitReason::Normal,
    };

    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        sched.exit(pid, reason);
    }
}

/// Look up a process by name (whereis)
/// name_ptr: pointer to null-terminated C string
/// Returns pointer to u64 containing ProcessId, or null if not found
#[unsafe(no_mangle)]
pub extern "C" fn process_whereis(name_ptr: *const libc::c_char) -> *mut c_void {
    if name_ptr.is_null() {
        return std::ptr::null_mut();
    }

    let name = unsafe {
        match std::ffi::CStr::from_ptr(name_ptr).to_str() {
            Ok(s) => s,
            Err(_) => return std::ptr::null_mut(),
        }
    };

    let scheduler = init_scheduler();
    let Ok(sched) = scheduler.lock() else {
        return std::ptr::null_mut();
    };

    if let Some(pid) = sched.whereis(name) {
        unsafe {
            let size = std::mem::size_of::<u64>();
            let mem = runtime_malloc(size) as *mut u64;
            if mem.is_null() {
                return std::ptr::null_mut();
            }
            *mem = pid;
            mem as *mut c_void
        }
    } else {
        std::ptr::null_mut()
    }
}

/// Register a process with a name
/// pid_ptr: pointer to u64 containing ProcessId
/// name_ptr: pointer to null-terminated C string
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn process_register(pid_ptr: *const u64, name_ptr: *const libc::c_char) -> i32 {
    if pid_ptr.is_null() || name_ptr.is_null() {
        return 0;
    }

    let pid = unsafe { *pid_ptr };
    let name = unsafe {
        match std::ffi::CStr::from_ptr(name_ptr).to_str() {
            Ok(s) => s.to_string(),
            Err(_) => return 0,
        }
    };

    let scheduler = init_scheduler();
    let Ok(mut sched) = scheduler.lock() else {
        return 0;
    };

    if sched.register(name, pid) { 1 } else { 0 }
}

/// Unregister a process name
/// name_ptr: pointer to null-terminated C string
#[unsafe(no_mangle)]
pub extern "C" fn process_unregister(name_ptr: *const libc::c_char) {
    if name_ptr.is_null() {
        return;
    }

    let name = unsafe {
        match std::ffi::CStr::from_ptr(name_ptr).to_str() {
            Ok(s) => s,
            Err(_) => return,
        }
    };

    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        sched.unregister(name);
    }
}

/// Check waiting promises and resume processes whose promises are ready
/// This should be called periodically by the executor
#[unsafe(no_mangle)]
pub extern "C" fn process_check_promises() {
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        sched.check_promises();
    }
}

/// Check expired timeouts and wake up processes
/// This should be called periodically by the scheduler
#[unsafe(no_mangle)]
pub extern "C" fn process_check_timeouts() {
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        sched.check_timeouts();
    }
}

/// Mark a process as waiting for a promise
/// pid_ptr: pointer to u64 containing ProcessId
/// promise: pointer to promise object
#[unsafe(no_mangle)]
pub extern "C" fn process_wait_for_promise(pid_ptr: *const u64, promise: *mut c_void) {
    if pid_ptr.is_null() || promise.is_null() {
        return;
    }

    let pid = unsafe { *pid_ptr };
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        sched.wait_for_promise(pid, promise);
    }
}

/// Mark a process as waiting for a timeout
/// pid_ptr: pointer to u64 containing ProcessId
/// timeout_ms: timeout in milliseconds
#[unsafe(no_mangle)]
pub extern "C" fn process_wait_for_timeout(pid_ptr: *const u64, timeout_ms: u64) {
    if pid_ptr.is_null() {
        return;
    }

    let pid = unsafe { *pid_ptr };
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        sched.wait_for_timeout(pid, timeout_ms);
    }
}

/// Link two processes
/// pid1_ptr: pointer to u64 containing first ProcessId
/// pid2_ptr: pointer to u64 containing second ProcessId
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn process_link(pid1_ptr: *const u64, pid2_ptr: *const u64) -> i32 {
    if pid1_ptr.is_null() || pid2_ptr.is_null() {
        return 0;
    }

    let pid1 = unsafe { *pid1_ptr };
    let pid2 = unsafe { *pid2_ptr };
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        if sched.link(pid1, pid2) { 1 } else { 0 }
    } else {
        0
    }
}

/// Unlink two processes
/// pid1_ptr: pointer to u64 containing first ProcessId
/// pid2_ptr: pointer to u64 containing second ProcessId
#[unsafe(no_mangle)]
pub extern "C" fn process_unlink(pid1_ptr: *const u64, pid2_ptr: *const u64) {
    if pid1_ptr.is_null() || pid2_ptr.is_null() {
        return;
    }

    let pid1 = unsafe { *pid1_ptr };
    let pid2 = unsafe { *pid2_ptr };
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        sched.unlink(pid1, pid2);
    }
}

/// Monitor a process
/// monitor_pid_ptr: pointer to u64 containing monitor ProcessId
/// target_pid_ptr: pointer to u64 containing target ProcessId
/// Returns pointer to u64 containing MonitorRef, or null on failure
#[unsafe(no_mangle)]
pub extern "C" fn process_monitor(
    monitor_pid_ptr: *const u64,
    target_pid_ptr: *const u64,
) -> *mut c_void {
    if monitor_pid_ptr.is_null() || target_pid_ptr.is_null() {
        return std::ptr::null_mut();
    }

    let monitor_pid = unsafe { *monitor_pid_ptr };
    let target_pid = unsafe { *target_pid_ptr };
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        if let Some(monitor_ref) = sched.monitor(monitor_pid, target_pid) {
            let size = std::mem::size_of::<u64>();
            let mem = runtime_malloc(size) as *mut u64;
            if mem.is_null() {
                return std::ptr::null_mut();
            }
            unsafe {
                *mem = monitor_ref;
            }
            mem as *mut c_void
        } else {
            std::ptr::null_mut()
        }
    } else {
        std::ptr::null_mut()
    }
}

/// Remove a monitor
/// monitor_pid_ptr: pointer to u64 containing monitor ProcessId
/// monitor_ref_ptr: pointer to u64 containing MonitorRef
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn process_demonitor(
    monitor_pid_ptr: *const u64,
    monitor_ref_ptr: *const u64,
) -> i32 {
    if monitor_pid_ptr.is_null() || monitor_ref_ptr.is_null() {
        return 0;
    }

    let monitor_pid = unsafe { *monitor_pid_ptr };
    let monitor_ref = unsafe { *monitor_ref_ptr };
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        if sched.demonitor(monitor_pid, monitor_ref) {
            1
        } else {
            0
        }
    } else {
        0
    }
}

/// Run the process scheduler (execute one process step)
/// This should be called in a loop to execute processes
/// Returns 1 if a process was executed, 0 if no runnable processes
#[unsafe(no_mangle)]
pub extern "C" fn process_scheduler_run() -> i32 {
    let scheduler = init_scheduler();
    let Ok(mut sched) = scheduler.lock() else {
        return 0;
    };

    // Check timeouts first
    sched.check_timeouts();

    // Check promises
    sched.check_promises();

    // Get next runnable process
    if let Some(pid) = sched.next_runnable() {
        // Set current PID for this execution context
        set_current_pid(pid);

        // The actual process execution will be handled by the codegen
        // This function just marks the process as runnable
        // The codegen will call back into the runtime when the process
        // needs to yield (receive, await, etc.)

        // For now, we just return that we have a process to run
        // The actual execution happens in the generated code
        clear_current_pid();
        1
    } else {
        0
    }
}

// ========== Supervisor FFI Functions ==========

/// Register a supervisor with configuration
/// supervisor_pid_ptr: pointer to u64 containing supervisor ProcessId
/// strategy: 0=OneForOne, 1=OneForAll, 2=RestForOne
/// max_restarts: maximum number of restarts in time window
/// max_seconds: time window in seconds
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn process_register_supervisor(
    supervisor_pid_ptr: *const u64,
    strategy: i32,
    max_restarts: u32,
    max_seconds: u64,
) -> i32 {
    if supervisor_pid_ptr.is_null() {
        return 0;
    }

    let supervisor_pid = unsafe { *supervisor_pid_ptr };
    let restart_strategy = match strategy {
        0 => RestartStrategy::OneForOne,
        1 => RestartStrategy::OneForAll,
        2 => RestartStrategy::RestForOne,
        _ => return 0,
    };

    let config = SupervisorConfig {
        strategy: restart_strategy,
        max_restarts,
        max_seconds,
        children: Vec::new(),
    };

    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        if sched.register_supervisor(supervisor_pid, config) {
            1
        } else {
            0
        }
    } else {
        0
    }
}

/// Add a child to a supervisor
/// supervisor_pid_ptr: pointer to u64 containing supervisor ProcessId
/// child_pid_ptr: pointer to u64 containing child ProcessId
/// child_id: null-terminated C string for child ID
/// restart: 0=Permanent, 1=Transient, 2=Temporary
/// shutdown_timeout: shutdown timeout in milliseconds
/// Returns 1 on success, 0 on failure
#[unsafe(no_mangle)]
pub extern "C" fn process_add_supervisor_child(
    supervisor_pid_ptr: *const u64,
    child_pid_ptr: *const u64,
    child_id: *const libc::c_char,
    restart: i32,
    shutdown_timeout: u64,
) -> i32 {
    if supervisor_pid_ptr.is_null() || child_pid_ptr.is_null() || child_id.is_null() {
        return 0;
    }

    let supervisor_pid = unsafe { *supervisor_pid_ptr };
    let child_pid = unsafe { *child_pid_ptr };
    let id_str = unsafe {
        match std::ffi::CStr::from_ptr(child_id).to_str() {
            Ok(s) => s.to_string(),
            Err(_) => return 0,
        }
    };

    let restart_policy = match restart {
        0 => RestartPolicy::Permanent,
        1 => RestartPolicy::Transient,
        2 => RestartPolicy::Temporary,
        _ => return 0,
    };

    let child_spec = ChildSpec {
        id: id_str,
        pid: child_pid,
        restart: restart_policy,
        shutdown_timeout,
    };

    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        if sched.add_supervisor_child(supervisor_pid, child_spec) {
            1
        } else {
            0
        }
    } else {
        0
    }
}

/// Set maximum number of processes
/// max: maximum number of processes (0 = use default)
#[unsafe(no_mangle)]
pub extern "C" fn process_set_max_processes(max: usize) {
    let scheduler = init_scheduler();
    if let Ok(mut sched) = scheduler.lock() {
        if max == 0 {
            sched.set_max_processes(1_000_000); // Default
        } else {
            sched.set_max_processes(max);
        }
    }
}

/// Get current process count
/// Returns current number of processes
#[unsafe(no_mangle)]
pub extern "C" fn process_get_count() -> usize {
    let scheduler = init_scheduler();
    if let Ok(sched) = scheduler.lock() {
        sched.process_count()
    } else {
        0
    }
}

// ========== Multi-threaded Scheduler (Phase 3) ==========

/// Work-stealing queue for multi-threaded scheduler
struct WorkStealingQueue {
    queue: Mutex<VecDeque<ProcessId>>,
}

impl WorkStealingQueue {
    fn new() -> Self {
        Self {
            queue: Mutex::new(VecDeque::new()),
        }
    }

    fn pop(&self) -> Option<ProcessId> {
        if let Ok(mut q) = self.queue.lock() {
            q.pop_front()
        } else {
            None
        }
    }

    fn steal(&self) -> Option<ProcessId> {
        if let Ok(mut q) = self.queue.lock() {
            q.pop_back() // Steal from the back
        } else {
            None
        }
    }
}

/// Multi-threaded scheduler with work-stealing
pub struct MultiThreadedScheduler {
    scheduler: Arc<Mutex<ProcessScheduler>>,
    worker_queues: Vec<Arc<WorkStealingQueue>>,
    num_threads: usize,
    running: Arc<AtomicU64>, // Flag to control scheduler threads
}

impl MultiThreadedScheduler {
    pub fn new(num_threads: usize) -> Self {
        let scheduler = Arc::new(Mutex::new(ProcessScheduler::new()));
        let mut worker_queues = Vec::new();
        for _ in 0..num_threads {
            worker_queues.push(Arc::new(WorkStealingQueue::new()));
        }
        Self {
            scheduler,
            worker_queues,
            num_threads,
            running: Arc::new(AtomicU64::new(1)),
        }
    }

    /// Start scheduler threads
    pub fn start(&self) {
        let num_threads = self.num_threads;
        let scheduler = self.scheduler.clone();
        let worker_queues = self.worker_queues.clone();
        let running = self.running.clone();

        for thread_id in 0..num_threads {
            let sched = scheduler.clone();
            let local_queue = worker_queues[thread_id].clone();
            let other_queues = worker_queues.clone();
            let run_flag = running.clone();

            thread::spawn(move || {
                while run_flag.load(Ordering::Relaxed) != 0 {
                    // Try to get work from local queue
                    if let Some(pid) = local_queue.pop() {
                        // Execute process
                        if let Ok(_sched) = sched.lock() {
                            set_current_pid(pid);
                            // Process execution happens here
                            // For now, just mark as runnable again
                            clear_current_pid();
                        }
                        continue;
                    }

                    // Try to steal work from other queues
                    let mut stolen = false;
                    for (idx, queue) in other_queues.iter().enumerate() {
                        if idx != thread_id
                            && let Some(pid) = queue.steal()
                        {
                            if let Ok(_sched) = sched.lock() {
                                set_current_pid(pid);
                                // Process execution happens here
                                clear_current_pid();
                            }
                            stolen = true;
                            break;
                        }
                    }

                    if !stolen {
                        // No work available, yield
                        thread::yield_now();
                    }
                }
            });
        }
    }

    /// Stop scheduler threads
    pub fn stop(&self) {
        self.running.store(0, Ordering::Relaxed);
    }
}

/// Global multi-threaded scheduler instance
static MULTI_SCHEDULER: OnceLock<Arc<MultiThreadedScheduler>> = OnceLock::new();

/// Initialize multi-threaded scheduler
/// num_threads: number of worker threads (0 = use CPU count)
#[unsafe(no_mangle)]
pub extern "C" fn process_scheduler_init_multi_threaded(num_threads: usize) -> i32 {
    let threads = if num_threads == 0 {
        num_cpus::get()
    } else {
        num_threads
    };

    let scheduler = Arc::new(MultiThreadedScheduler::new(threads));
    scheduler.start();

    if MULTI_SCHEDULER.set(scheduler).is_ok() {
        1
    } else {
        0
    }
}

/// Stop multi-threaded scheduler
#[unsafe(no_mangle)]
pub extern "C" fn process_scheduler_stop() {
    if let Some(scheduler) = MULTI_SCHEDULER.get() {
        scheduler.stop();
    }
}
