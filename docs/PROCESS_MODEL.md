# Process Model & Safe Failure Design

**Status:** Design Document (Not Yet Implemented)  
**Last Updated:** November 2025

## Overview

This document outlines the design for an Elixir/Erlang-inspired process model in Oats, providing lightweight isolated execution units with message passing, supervision trees, and a "let it crash" philosophy for fault tolerance.

## Design Goals

1. **Isolation**: Processes are isolated units of execution with no shared mutable state
2. **Message Passing**: Processes communicate exclusively via asynchronous message passing
3. **Fault Tolerance**: Process failures are isolated and can trigger supervised restarts
4. **Lightweight**: Processes should be much lighter than OS threads (target: ~1-2KB per process)
5. **Integration**: Work seamlessly with existing async/await and ARC systems
6. **Deterministic**: Maintain Oats' deterministic memory management guarantees

## Core Concepts

### Processes

A **Process** in Oats is:

- A lightweight isolated execution context (not an OS thread)
- Has its own mailbox for receiving messages
- Has its own stack and local variables
- Cannot directly access another process's memory
- Can spawn child processes and be supervised

### Message Passing

- **Asynchronous**: Sending a message is non-blocking
- **Type-safe**: Messages are typed values (can be any Oats type)
- **Ordered**: Messages are delivered in FIFO order per process
- **Copying**: Messages are copied (via ARC) when sent between processes

### Supervision Trees

- **Supervisor**: A process that monitors and manages child processes
- **Supervised Process**: A process that reports to a supervisor
- **Restart Strategies**: One-for-one, one-for-all, rest-for-one
- **Max Restarts**: Configurable limits to prevent restart loops

### "Let It Crash" Philosophy

- Processes are expected to fail
- Failures are isolated to the failing process
- Supervisors handle recovery, not individual processes
- **Error Handling**: Uncaught exceptions/panics in a process cause it to exit with `{error, reason}`
- **Try-Catch**: Oats supports try-catch, but in process code, uncaught errors bubble to the supervisor rather than crashing the entire system
- **Process Isolation**: A process crash (exception, panic, or explicit exit) only affects that process and its linked/supervised processes

## Type Definitions

### Core Process Types

```typescript
// Process identifier - opaque value type (internally a u64)
type ProcessId = number; // Runtime representation, not directly constructible

// Process status enumeration
enum ProcessStatus {
    Running,    // Process is executing or ready to execute
    Waiting,    // Process is waiting for a message or async operation
    Terminated  // Process has exited
}

// Exit reason union type
type ExitReason = 
    | "normal"                    // Normal termination
    | {error: string}              // Error termination with reason
    | {kill: string}               // Killed by supervisor
    | {shutdown: string};          // Shutdown requested

// Process reference for monitoring
type MonitorRef = number; // Opaque reference returned by monitor()
```

### Message Types

```typescript
// Base message structure (runtime representation)
interface Message {
    from: ProcessId;
    payload: unknown;  // Type-erased at runtime, but type-checked at compile time
    timestamp: number; // Unix timestamp in milliseconds
}

// Typed message wrapper (compile-time concept)
// Messages are sent as any Oats type, wrapped in Message at runtime
```

## Architecture

### Process Runtime Structure

The process structure is managed by the runtime scheduler. From the language perspective, processes are represented by `ProcessId` values. The internal runtime structure (implemented in Rust) is:

```rust
// Runtime implementation (not exposed to Oats code)
struct Process {
    // Process metadata
    id: ProcessId,              // Unique identifier (u64)
    name: Option<String>,        // Optional registered name
    status: ProcessStatus,      // Running, Waiting, Terminated
    
    // Execution context
    stack: Vec<u8>,             // Process stack (grows dynamically)
    mailbox: MessageQueue,      // Incoming message queue
    state: Option<Box<dyn Any>>, // Process-specific state (type-erased)
    
    // Supervision
    supervisor: Option<ProcessId>, // Parent supervisor
    children: Vec<ProcessId>,     // Child processes
    
    // Lifecycle
    spawn_time: u64,            // Timestamp (milliseconds since epoch)
    last_message_time: u64,     // Timestamp
    
    // Error handling
    exit_reason: Option<ExitReason>, // Normal | {error, Reason}
    
    // Async integration
    current_promise: Option<*mut Promise>, // Current awaited promise (if any)
}
```

### Message Queue

```rust
// Runtime implementation
struct MessageQueue {
    messages: VecDeque<Message>,
    waiting_tasks: Vec<Task>, // Tasks waiting for messages
}

struct Message {
    from: ProcessId,
    payload: *mut c_void,      // Pointer to ARC-managed heap object
    payload_type_id: u64,      // Type identifier for runtime type checking
    timestamp: u64,
}
```

**Note**: From the Oats language perspective, messages are sent as typed values. The runtime handles ARC management and type erasure internally.

### Scheduling Model

The scheduler manages process execution and integrates with the existing promise executor:

1. **Cooperative Scheduling**: Processes yield on `receive` or `await`
2. **Fair Scheduling**: Round-robin execution of runnable processes
3. **Priority Levels**: Normal, High (for supervisors)
4. **Integration with Async**: Processes can await promises, yielding to scheduler
5. **Thread Model**:
   - Phase 1: Single-threaded scheduler running on main thread or dedicated thread
   - Phase 3: Multi-threaded scheduler with work-stealing (thread pool size = CPU count)

**Integration with Existing Executor**:

- The process scheduler shares the same promise polling infrastructure as the existing async/await system
- When a process awaits a promise, it registers with the promise executor
- The executor notifies the process scheduler when promises are ready
- This avoids duplication and ensures consistent async behavior

### Memory Model Integration

- **Message Copying**: Messages are ARC-incremented when sent (via `rc_inc`), decremented when received/consumed
- **Process Isolation**: Each process has its own stack, but shares the global heap (ARC ensures safety)
- **Shared Immutable Data**: Static/immortal objects (with `STATIC` flag set) can be shared without copying
- **Weak References**: Processes can hold weak references to other processes (for monitoring) using `Weak<ProcessId>`
- **Process State**: Process-local state uses normal ARC; state is cleaned up on process exit
- **Cycle Collection**: Process reference cycles (e.g., A holds message containing B's ProcessId, B holds message containing A's ProcessId) are handled by the existing cycle collector

## Language Syntax

### Process Spawning

```typescript
// Spawn a new process (returns ProcessId)
let pid: ProcessId = spawn(() => {
    // Process body
    console.log("Hello from process!");
});

// Spawn with initial state
let pid = spawn<StateType>((state: StateType) => {
    // Process with state
    // State persists for the lifetime of the process
}, initialState);

// Spawn a named process
let pid = spawn("my_process", () => {
    // Named process (can be looked up via whereis())
});

// Spawn an async process
let pid = spawn(async () => {
    let result = await someAsyncOperation();
    send(parent, result);
});

// Get current process ID
let myPid: ProcessId = self();
```

### Message Sending

```typescript
// Send a message (non-blocking)
send(pid, message);

// Send to a named process
send("my_process", message);

// Send with type safety
send<MessageType>(pid, typedMessage);
```

### Message Receiving

```typescript
// Receive any message (blocks until message arrives)
// Returns unknown type - caller must handle type checking
let message: unknown = receive();

// Receive with type assertion (compile-time type, runtime checked)
let message = receive<MessageType>(); // Returns MessageType | null if type mismatch

// Receive with timeout (milliseconds)
let message = receive<MessageType>(timeout: number); 
// Returns MessageType | null (null if timeout or type mismatch)

// Selective receive with predicate
let message = receive<MessageType>((msg: MessageType) => {
    return msg.type === "important";
});
// Returns MessageType | null

// Pattern: Receive loop with type guards
while (true) {
    let msg = receive<MessageType>();
    if (msg !== null) {
        // Handle message
        handleMessage(msg);
    }
}
```

### Process Linking and Monitoring

```typescript
// Link to another process (exit propagates)
link(pid);

// Monitor a process (receive exit notifications)
let ref = monitor(pid);
// Later: receive({down, ref, pid, reason})

// Unlink
unlink(pid);

// Unmonitor
demonitor(ref);
```

### Process Registration

```typescript
// Register a process with a name
register("my_process", pid);

// Look up a process by name
let pid = whereis("my_process"); // Returns ProcessId | null

// Unregister
unregister("my_process");
```

### Process Exit

```typescript
// Exit normally (notifies supervisor and linked processes)
exit("normal");

// Exit with error reason
exit({error: "connection_failed"});

// Exit with kill reason (typically from supervisor)
exit({kill: "shutdown"});

// Exit with shutdown reason
exit({shutdown: "graceful"});

// Note: exit() never returns - it terminates the process
// Linked processes receive {down, pid, reason} messages
// Supervisor receives exit notification based on restart strategy
```

## Supervision

### Supervisor Behavior

```typescript
// Define a supervisor
supervisor(function() {
    // Spawn supervised children
    let child1 = spawn_child(worker1);
    let child2 = spawn_child(worker2);
    
    // Supervisor loop
    while (true) {
        let msg = receive();
        // Handle child exit notifications
        if (msg.type === "exit") {
            // Restart strategy determines action
            restart_child(msg.pid);
        }
    }
});
```

### Restart Strategies

1. **One-for-One**: Only restart the failed child
2. **One-for-All**: Restart all children when one fails
3. **Rest-for-One**: Restart the failed child and all children started after it

### Supervisor Configuration

```typescript
interface SupervisorConfig {
    strategy: "one_for_one" | "one_for_all" | "rest_for_one";
    max_restarts: number;      // Max restarts in time window
    max_seconds: number;        // Time window for max_restarts
    children: ChildSpec[];
}

interface ChildSpec {
    id: string;
    start: () => ProcessId;
    restart: "permanent" | "transient" | "temporary";
    shutdown: number; // milliseconds
}
```

## Integration with Existing Systems

### Async/Await Integration

Processes can use async/await naturally:

```typescript
spawn(async function() {
    let result = await someAsyncOperation();
    send(parent, result);
});
```

The scheduler integrates with the existing promise polling system:

- When a process awaits, it yields to the scheduler
- The scheduler polls promises and resumes processes when ready
- This allows efficient I/O without blocking

### ARC Memory Management

- **Message Passing**: Messages are ARC-incremented when sent, decremented when received
- **Process State**: Process-local state uses normal ARC
- **Shared Immutable**: Static strings/arrays can be shared without copying
- **Process References**: ProcessIds are lightweight (no ARC needed)

### Cycle Collection

- Processes can create cycles via message passing (process A holds message containing reference to process B)
- The existing cycle collector can handle process reference cycles
- Process exit breaks cycles automatically

## Runtime Implementation

### Process Scheduler

The scheduler manages process execution and integrates with the promise executor:

```rust
struct ProcessScheduler {
    runnable: VecDeque<ProcessId>,           // Processes ready to run
    waiting: HashMap<ProcessId, WaitReason>, // Processes waiting (message or promise)
    processes: HashMap<ProcessId, Process>,   // All processes
    timer_heap: BinaryHeap<Timer>,           // Timeouts for receive() with timeout
    registry: HashMap<String, ProcessId>,    // Name -> ProcessId mapping
    executor: Arc<Exec>,                     // Shared promise executor
}

enum WaitReason {
    WaitingForMessage,        // Blocked on receive()
    WaitingForPromise(*mut Promise), // Blocked on await
    WaitingForTimeout(u64),   // Blocked on receive() with timeout
}
```

**Thread Model**:

- **Phase 1**: Single scheduler thread (can be main thread or dedicated thread)
- **Phase 3**: Multi-threaded with work-stealing queues (one per CPU core)
- Processes are not bound to specific threads (can migrate between scheduler threads in Phase 3)

### Message Delivery

1. **Send**: Sender calls `rc_inc` on message payload (if not STATIC)
2. **Enqueue**: Message enqueued in receiver's mailbox (FIFO order)
3. **Wake**: If receiver is waiting on `receive()`, mark it as runnable
4. **Receive**: When receiver calls `receive()`, message is dequeued
5. **Cleanup**: Receiver's reference to message payload is dropped (calls `rc_dec` when out of scope)

**Type Safety**:

- Compile-time: `send<T>(pid, value)` ensures `value` is of type `T`
- Runtime: Type identifier stored with message for runtime type checking in `receive<T>()`
- If type mismatch in `receive<T>()`, message remains in queue and function returns `null`

### Process Lifecycle

1. **Spawn**:
   - Allocate process structure on heap
   - Initialize stack (starts small, ~1KB, grows as needed)
   - Initialize mailbox (empty message queue)
   - Add to scheduler's runnable queue
   - Return ProcessId to caller

2. **Run**:
   - Scheduler selects process from runnable queue
   - Execute process code until yield point:
     - `receive()` - waiting for message
     - `await` - waiting for promise
     - Explicit `yield()` (future feature)
   - Process state saved (stack, locals, current promise if any)

3. **Wait**:
   - Process moved to waiting queue
   - Scheduler moves to next runnable process
   - Process remains in waiting until condition met

4. **Resume**:
   - Condition met (message arrived, promise ready, timeout expired)
   - Process moved back to runnable queue
   - Execution continues from yield point

5. **Exit**:
   - Process terminates (normal exit, error, or kill)
   - Cleanup resources:
     - Decrement ARC on process state
     - Decrement ARC on remaining messages in mailbox
     - Remove from scheduler
   - Notify supervisor and linked processes
   - Supervisor decides on restart based on strategy

### Error Handling

When a process crashes (uncaught exception, panic, or explicit exit):

1. **Capture**: Runtime captures error information (exception type, message, stack trace if available)
2. **Set Exit Reason**: Set `exit_reason` to `{error, reason}` (or provided reason)
3. **Notify Links**: Send `{down, pid, reason}` messages to all linked processes
4. **Notify Supervisor**: Send exit notification to supervisor (if any)
5. **Cleanup**:
   - Decrement ARC on process state
   - Decrement ARC on all messages in mailbox
   - Remove from scheduler and registry
6. **Supervisor Action**: Supervisor receives notification and decides on restart based on:
   - Restart strategy (one-for-one, one-for-all, rest-for-one)
   - Max restarts configuration
   - Child restart policy (permanent, transient, temporary)

## Performance Considerations

### Lightweight Processes

- **Stack Size**: Start small (~1KB), grow dynamically as needed (up to configurable max, e.g., 8MB)
- **Allocation**: Process structure allocated on heap, but compact (~200-300 bytes base)
- **Context Switching**: Much cheaper than OS threads:
  - No kernel call required
  - Just save/restore stack pointer and registers
  - Estimated overhead: <100ns per context switch
- **Stack Growth**: Stack grows in pages; if max size reached, process exits with `{error, "stack_overflow"}`

### Message Passing Optimization

- **Zero-Copy for Immutable**: Static/immortal objects (with STATIC flag) are shared without ARC increment
- **ARC Sharing**: Mutable objects are ARC-incremented (shared ownership), not copied
- **Copy-on-Write**: Consider COW for large messages (future optimization)
- **Batching**: Batch small messages when possible (future optimization)
- **Message Size**: No hard limit, but very large messages (>1MB) may impact scheduler performance

### Scheduler Efficiency

- **Work Stealing**: Consider work-stealing queue for multi-core
- **Priority Queues**: High-priority processes (supervisors) scheduled first
- **Timer Optimization**: Efficient timer heap for timeouts

## Safety Guarantees

### Isolation Guarantees

1. **No Shared Mutable State**: Processes cannot directly access another process's memory
2. **Message Copying**: All mutable data is copied when sent (via ARC)
3. **Type Safety**: Message types are checked at compile time (via generics)

### Failure Isolation

1. **Process Crashes Don't Affect Others**: Only linked/supervised processes are notified
2. **Supervisor Isolation**: Supervisor crashes don't cascade (unless supervised by another supervisor)
3. **Resource Cleanup**: Process exit always cleans up resources (ARC ensures this)

### Memory Safety

1. **ARC for Messages**: Messages are properly reference counted
2. **Process State**: Process-local state uses normal ARC
3. **Cycle Collection**: Existing cycle collector handles process cycles

## Migration Path

### Phase 1: Core Process Model

- Basic process spawning
- Message sending/receiving
- Process scheduler (single-threaded initially)
- Integration with async/await

### Phase 2: Supervision

- Supervisor behavior
- Restart strategies
- Process linking/monitoring

### Phase 3: Advanced Features

- Process registration (names)
- Multi-threaded scheduler
- Performance optimizations
- Hot code reloading (future)

### Phase 4: Standard Library

- Generic supervisor implementations
- Common process patterns (gen_server, gen_event equivalents)
- Process pools and load balancing

## Example: Echo Server

```typescript
// Echo server process
spawn("echo_server", function() {
    while (true) {
        let msg = receive<{from: ProcessId, text: string}>();
        send(msg.from, {echo: msg.text});
    }
});

// Client
let server = whereis("echo_server");
send(server, {from: self(), text: "Hello"});
let response = receive<{echo: string}>();
console.log(response.echo); // "Hello"
```

## Example: Supervised Worker Pool

```typescript
supervisor(function() {
    let config: SupervisorConfig = {
        strategy: "one_for_one",
        max_restarts: 5,
        max_seconds: 60,
        children: []
    };
    
    // Spawn worker processes
    for (let i = 0; i < 10; i++) {
        let worker = spawn_child(function() {
            // Worker logic
            while (true) {
                let task = receive<Task>();
                processTask(task);
            }
        });
        config.children.push({id: `worker_${i}`, start: () => worker, ...});
    }
    
    // Supervisor loop
    while (true) {
        let msg = receive();
        if (msg.type === "exit") {
            // Restart failed worker
            restart_child(msg.pid);
        }
    }
});
```

## Open Questions

1. **Process Migration**:
   - **Decision**: Yes, processes should be able to migrate between scheduler threads in Phase 3
   - **Rationale**: Enables better load balancing and work-stealing
   - **Implementation**: Process state is already thread-independent (stack, mailbox, state)

2. **Distributed Processes**:
   - **Status**: Future work, not in initial phases
   - **Considerations**: Network transparency, message serialization, node failure handling
   - **Reference**: Erlang/OTP distributed architecture

3. **Process Limits**:
   - **Decision**: Configurable maximum (default: 1 million processes)
   - **Rationale**: Prevents resource exhaustion while allowing high concurrency
   - **Implementation**: Check limit in `spawn()`, return error if exceeded

4. **Priority Levels**:
   - **Decision**: Two levels initially (Normal, High)
   - **Rationale**: Supervisors need higher priority for timely restarts
   - **Future**: May add more levels if needed (Low, Critical)

5. **Hot Code Reloading**:
   - **Status**: Future work, not in initial phases
   - **Considerations**: Process state migration, version compatibility, graceful upgrades
   - **Reference**: Erlang/OTP hot code loading

6. **Process Groups**:
   - **Question**: Should processes be organized into groups for coordinated operations?
   - **Status**: Deferred to Phase 4 (standard library patterns)

7. **Process Tracing and Debugging**:
   - **Question**: How to support debugging and tracing of process execution?
   - **Status**: Future work, consider integration with language server

## References

- [Erlang/OTP Design Principles](https://www.erlang.org/doc/design_principles/users_guide.html)
- [Elixir GenServer Documentation](https://hexdocs.pm/elixir/GenServer.html)
- [The Actor Model](https://en.wikipedia.org/wiki/Actor_model)
- [Let It Crash Philosophy](https://learnyousomeerlang.com/buckets-of-sockets#the-erlang-way)
