# Async/Await Implementation Plan

## Overview

Supporting `async`/`await` is a significant but achievable goal for the Oats compiler. It requires deep integration between the compiler-generated code and the runtime system. This document outlines a comprehensive plan for implementing async/await functionality.

## Architecture: State Machines + Runtime Scheduler

Async/await cannot be implemented with compiler changes alone. The generated code needs a runtime system that knows how to pause and resume functions.

---

## Part 1: Compiler-Side Transformation (The State Machine)

The compiler must fundamentally rewrite `async` functions. An `async` function is not compiled as a single, linear block of code. Instead, it's transformed into a structure that can be paused at each `await` point and resumed later.

### How State Machine Transformation Works

#### 1. Create a State Struct

For each `async` function, the compiler creates a struct that holds:
- **All local variables** of the function (ensuring they persist when the function is paused)
- A **state field** (integer) to track which part of the function to execute next
- **Saved intermediate values** from expressions that span `await` points

Example transformation:
```typescript
// Source code:
async function fetchUser(id: number): Promise<User> {
    let response = await fetch(`/api/users/${id}`);
    let user = await response.json();
    return user;
}

// Transformed into state struct (conceptual):
struct FetchUserState {
    state: i32,           // Current execution point
    id: f64,              // Parameter
    response: ptr,        // Local variable
    user: ptr,            // Local variable
    temp_result: ptr,     // Temporary for await results
}
```

#### 2. Rewrite the Function Body

The body of the `async` function becomes a single, non-`async` function that:
- Takes a pointer to its state struct
- Contains a large `switch` statement based on the "state" field
- Each `case` corresponds to a block of code between `await` expressions

```rust
// Pseudo-IR for the transformed function:
fn fetchUser_resume(state_ptr: *mut FetchUserState) -> TaskStatus {
    match state_ptr.state {
        0 => {
            // Initial state: call fetch()
            let url = string_concat("/api/users/", state_ptr.id);
            let fetch_promise = fetch(url);
            state_ptr.temp_result = fetch_promise;
            state_ptr.state = 1;
            return TaskStatus::Pending;
        }
        1 => {
            // Resume after fetch completes
            state_ptr.response = state_ptr.temp_result;
            let json_promise = response_json(state_ptr.response);
            state_ptr.temp_result = json_promise;
            state_ptr.state = 2;
            return TaskStatus::Pending;
        }
        2 => {
            // Resume after json() completes
            state_ptr.user = state_ptr.temp_result;
            return TaskStatus::Complete(state_ptr.user);
        }
    }
}
```

#### 3. Lower `await` Expressions

An `await` expression is transformed from a simple call into a sequence:
1. Execute the expression being awaited (e.g., another `async` function call)
2. Save the current position by setting the "state" field in the state struct
3. Return a special "pending" status to the runtime (pausing execution)

### Compiler Implementation Details

**Files to Modify:**

- **`crates/oats/src/types.rs`**:
  - Add `OatsType::Promise(Box<OatsType>)` variant
  - Implement `Promise<T>` representation in the type system
  - Add helper methods: `is_promise()`, `unwrap_promise_inner()`

- **`crates/oats/src/codegen/mod.rs`**:
  - Detect `async` functions in `gen_function_ir`:
    ```rust
    if func.is_async {
        return self.gen_async_function_ir(name, func, params, ret);
    }
    ```
  - Add new method `gen_async_function_ir()`:
    - Create state struct type
    - Allocate state struct in heap
    - Generate resume function with switch-based dispatch
    - Return Promise object wrapping the state

- **`crates/oats/src/codegen/stmt.rs`** (or in mod.rs):
  - Add handling for `await` expressions in `lower_expr`:
    ```rust
    ast::Expr::Await(await_expr) => {
        // Lower the awaited expression
        let promise_val = self.lower_expr(&await_expr.arg, ...)?;
        
        // Save state and suspend
        self.emit_state_save(current_state_id + 1);
        self.emit_suspend_task(promise_val);
        
        // Create continuation point
        let resume_bb = self.context.append_basic_block(function, "await.resume");
        self.builder.position_at_end(resume_bb);
        
        // Load result from state struct
        let result = self.emit_load_temp_result();
        Ok(result)
    }
    ```

**New Helpers to Add:**

```rust
impl<'a> CodeGen<'a> {
    /// Generate IR for an async function (state machine transformation)
    fn gen_async_function_ir(
        &self,
        name: &str,
        func: &ast::Function,
        params: &[OatsType],
        ret: &OatsType,
    ) -> Result<FunctionValue<'a>, Diagnostic> {
        // 1. Create state struct type with fields for:
        //    - state: i32
        //    - params: ...
        //    - locals: ...
        //    - temp_result: ptr
        
        // 2. Create wrapper function that allocates state and schedules task
        
        // 3. Create resume function with switch-based state dispatch
        
        // 4. Return Promise<T> object
    }
    
    /// Emit state save before await
    fn emit_state_save(&self, next_state: u32) {
        // store i32 next_state, ptr %state_ptr.state_field
    }
    
    /// Emit task suspension (return pending to runtime)
    fn emit_suspend_task(&self, promise: BasicValueEnum<'a>) {
        // call @runtime_suspend_on_promise(ptr %state_ptr, ptr %promise)
        // ret i32 TASK_PENDING
    }
    
    /// Analyze function to find all await points
    fn find_await_points(&self, body: &[ast::Stmt]) -> Vec<AwaitInfo> {
        // Walk AST and collect all await expressions with metadata
    }
}
```

---

## Part 2: Runtime-Side Execution (The Scheduler)

The existing runtime in `crates/runtime/src/lib.rs` is purely synchronous. A new component—an **event loop** or **task scheduler**—is needed to manage and run the state machines generated by the compiler.

### How the Scheduler Works

#### 1. Task Queue

The scheduler maintains a queue of tasks ready to be executed. Each "task" is a pointer to a state machine struct.

```c
// Runtime task representation
typedef struct Task {
    void* state_ptr;           // Pointer to state machine struct
    TaskResumeFn resume_fn;    // Function to resume execution
    struct Task* next;         // Next task in queue
} Task;

typedef enum {
    TASK_PENDING = 0,
    TASK_COMPLETE = 1,
    TASK_ERROR = 2,
} TaskStatus;

typedef TaskStatus (*TaskResumeFn)(void* state_ptr);
```

#### 2. The Event Loop

The core of the scheduler is a loop that:
1. Pulls a task from the ready queue
2. Calls the task's state machine resume function
3. If the function returns "complete," destroy the task
4. If the function returns "pending," re-schedule it (or wait for its dependency)

```c
void runtime_run_event_loop() {
    while (has_pending_tasks()) {
        Task* task = dequeue_ready_task();
        
        TaskStatus status = task->resume_fn(task->state_ptr);
        
        if (status == TASK_COMPLETE) {
            // Task finished, clean up
            free_task(task);
        } else if (status == TASK_PENDING) {
            // Task is waiting, will be rescheduled by promise completion
            // (do nothing - task remains in waiting set)
        }
    }
}
```

#### 3. Promise Object Layout

Promises link tasks together and enable the scheduler to know when to resume waiting tasks.

```c
typedef struct Promise {
    int32_t refcount;          // For memory management
    uint8_t status;            // Pending/Resolved/Rejected
    void* result;              // Result value (when resolved)
    Task** waiting_tasks;      // Array of tasks waiting on this promise
    size_t waiting_count;      // Number of waiting tasks
} Promise;
```

#### 4. Integrating with I/O

For `async`/`await` to be useful for network requests or file access, the scheduler must integrate with the OS's non-blocking I/O:
- **Linux**: `epoll`
- **macOS**: `kqueue`
- **Windows**: `IOCP` (I/O Completion Ports)

### Runtime Implementation Details

**Files to Modify:**

- **`crates/runtime/src/lib.rs`**:

```rust
// Add to runtime (C implementation):

// Task management
#[no_mangle]
pub extern "C" fn runtime_schedule_task(state_ptr: *mut u8, resume_fn: TaskResumeFn);

#[no_mangle]
pub extern "C" fn runtime_run_event_loop();

// Promise operations
#[no_mangle]
pub extern "C" fn promise_create() -> *mut Promise;

#[no_mangle]
pub extern "C" fn promise_resolve(promise: *mut Promise, result: *mut u8);

#[no_mangle]
pub extern "C" fn promise_add_waiter(promise: *mut Promise, task: *mut Task);

// Suspend current task until promise completes
#[no_mangle]
pub extern "C" fn runtime_suspend_on_promise(state_ptr: *mut u8, promise: *mut Promise);

// I/O integration (Phase 4)
#[no_mangle]
pub extern "C" fn runtime_register_io_handler(fd: i32, handler: IOCallback);
```

**Data Structures:**

```c
// In crates/runtime/src/lib.rs (C code):

typedef struct TaskQueue {
    Task* head;
    Task* tail;
    size_t count;
} TaskQueue;

typedef struct Runtime {
    TaskQueue ready_queue;      // Tasks ready to run
    TaskQueue waiting_set;      // Tasks waiting on promises
    int epoll_fd;               // For Linux async I/O
    bool is_running;
} Runtime;

static Runtime global_runtime = {0};
```

---

## Incremental Adoption Plan

Adding full `async`/`await` support is a huge task, but it can be approached incrementally.

### Phase 0: Foundation (Design & Types)

**Goal**: Define the Promise object layout and add placeholder types.

**Tasks**:
- [ ] Create this design document ✅
- [ ] Add `Promise` object layout specification to `docs/object_heap_design.md`
- [ ] Add `OatsType::Promise(Box<OatsType>)` to `crates/oats/src/types.rs`
- [ ] Write unit tests for Promise type representation

**Estimated Effort**: 2-4 hours

---

### Phase 1: State Machine Transformation (Minimal)

**Goal**: Implement state machine transformation for the simplest case—awaiting a value that's already resolved.

**Tasks**:
- [ ] Add `is_async` detection in function parsing
- [ ] Implement `gen_async_function_ir()` for simple case (no loops, single await)
- [ ] Transform single `await` expression into state save + suspend
- [ ] Generate switch-based resume function
- [ ] Test: compile async function to IR and verify state struct + switch structure

**Test Case**:
```typescript
async function simpleAsync(): Promise<number> {
    let x = await resolved(42);
    return x;
}

function resolved(val: number): Promise<number> {
    // Stub: returns already-resolved promise
}
```

**Expected IR**:
- State struct with `state`, `x` fields
- Resume function with switch on state field
- Proper RC semantics on state struct fields

**Estimated Effort**: 1-2 weeks

---

### Phase 2: Minimal Scheduler (No Real Async)

**Goal**: Implement a basic single-threaded scheduler that runs tasks until completion, without handling real non-blocking I/O.

**Tasks**:
- [ ] Implement task queue data structure in runtime
- [ ] Add `runtime_schedule_task()` function
- [ ] Add `runtime_run_event_loop()` function
- [ ] Implement Promise object with status/result fields
- [ ] Add `promise_resolve()` to mark promises complete and reschedule waiters
- [ ] Test: manually schedule tasks and verify they run to completion

**Test Case**:
```typescript
async function test(): Promise<number> {
    return 42;
}

function main(): number {
    let promise = test();
    runtime_run_event_loop();
    return promise.result;
}
```

**Estimated Effort**: 1-2 weeks

---

### Phase 3: Compiler + Runtime Integration

**Goal**: Connect the compiler and runtime so generated code properly calls runtime functions.

**Tasks**:
- [ ] Codegen for `await` calls `runtime_suspend_on_promise()`
- [ ] Async function wrapper allocates state and calls `runtime_schedule_task()`
- [ ] Main function modified to call `runtime_run_event_loop()` before exit
- [ ] Add runtime function declarations in codegen initialization
- [ ] Test: end-to-end compilation and execution of async function

**Test Case**:
```typescript
async function compute(): Promise<number> {
    let a = await resolved(10);
    let b = await resolved(20);
    return a + b;
}

export function main(): number {
    let result = compute();
    runtime_run_event_loop();
    return result.unwrap(); // Should return 30
}
```

**Estimated Effort**: 1-2 weeks

---

### Phase 4: Real Async I/O

**Goal**: Extend the scheduler to handle timers and non-blocking I/O operations.

**Tasks**:
- [ ] Integrate with `epoll` (Linux) / `kqueue` (macOS) / `IOCP` (Windows)
- [ ] Implement `setTimeout()` using timer wheel or heap-based scheduler
- [ ] Implement async file I/O (`fs.readFile`, `fs.writeFile`)
- [ ] Implement async network I/O (sockets, HTTP client)
- [ ] Add I/O callback handling that resolves promises
- [ ] Test: real async operations (HTTP fetch, file read)

**Test Case**:
```typescript
async function fetchAndProcess(): Promise<string> {
    let response = await fetch("https://api.example.com/data");
    let json = await response.json();
    return json.message;
}

export function main(): number {
    let promise = fetchAndProcess();
    runtime_run_event_loop();
    print_str(promise.result);
    return 0;
}
```

**Estimated Effort**: 3-4 weeks

---

## Technical Challenges & Solutions

### Challenge 1: Nested Async Calls

**Problem**: When an async function calls another async function, the caller must wait for the callee's promise.

**Solution**: The callee's promise is stored in the caller's state struct. The caller's task is added to the promise's waiters list. When the callee completes, it resolves its promise, which triggers rescheduling of all waiting tasks.

### Challenge 2: Exception Handling in Async Functions

**Problem**: If an `await` expression throws an error (promise rejected), the error must propagate through the call stack.

**Solution**: 
- Add `TaskStatus::Error` variant
- Store error value in Promise struct
- Resume function checks promise status before loading result
- If error, propagate by returning `TaskStatus::Error`

### Challenge 3: Async Functions in Loops

**Problem**: Awaiting inside a loop requires special handling—each iteration needs its own state.

**Solution**:
- Loop body becomes its own sub-state machine
- State field encodes both "which block" and "which loop iteration"
- Alternative: Flatten loops into explicit continuation-passing style

### Challenge 4: Memory Management of State Structs

**Problem**: State structs live on the heap and must be freed when tasks complete.

**Solution**:
- Reference count state structs (like other heap objects)
- Promise holds reference to waiting tasks' state structs
- When task completes, decrement refcount and free if zero

---

## Performance Considerations

### Stack vs. Heap Allocation

**Tradeoff**: Sync functions use stack allocation (fast). Async functions need heap allocation (slower but necessary for persistence).

**Mitigation**: 
- Pool/reuse state struct allocations
- Use arena allocator for short-lived async operations

### Scheduler Overhead

**Tradeoff**: Context switching between tasks has overhead.

**Mitigation**:
- Batch multiple ready tasks before yielding to OS
- Use inline caching for task dispatch

### Promise Chain Depth

**Tradeoff**: Long chains of promises create many heap allocations.

**Mitigation**:
- Promise chaining optimization: collapse chains at runtime
- Tail-call optimization for async functions that just await and return

---

## Testing Strategy

### Unit Tests

- **State machine generation**: Verify IR contains correct switch structure
- **Promise operations**: Test resolve/reject/await primitives
- **Task scheduling**: Test queue operations (enqueue, dequeue, reschedule)

### Integration Tests

- **Simple async function**: Single await, verify completion
- **Chained awaits**: Multiple awaits in sequence
- **Concurrent tasks**: Multiple async functions running simultaneously
- **Error propagation**: Rejected promise causes caller to error

### End-to-End Tests

- **Timer example**: `setTimeout()` resolves after delay
- **File I/O**: Read file asynchronously
- **HTTP request**: Fetch data from network (requires HTTP client runtime)

---

## Future Enhancements

### After Initial Implementation

1. **Async Iterators**: Support `for await (x of asyncIterable)`
2. **Promise Combinators**: `Promise.all()`, `Promise.race()`
3. **Async Generators**: `async function* generator()`
4. **Work Stealing**: Multi-threaded scheduler for CPU-bound tasks
5. **Structured Concurrency**: Task groups and cancellation

---

## References & Resources

### Academic Papers
- "Lightweight Asynchronous Snapshots for Distributed Dataflows" (Naiad paper)
- "Async/Await in Programming Languages" (Rust RFC 2394)

### Implementation Examples
- **Tokio** (Rust): https://github.com/tokio-rs/tokio
- **Async/Await in C++20**: Coroutines implementation
- **JavaScript V8**: Turbofan's async lowering
- **Python asyncio**: Event loop and task scheduling

### Existing Oats Documentation
- `docs/object_heap_design.md`: Object layout and RC semantics
- `docs/CLASS_IMPLEMENTATION.md`: State struct patterns (similar approach)
- `.github/copilot-instructions.md`: Current roadmap and status

---

## Summary

Implementing `async`/`await` is the most ambitious feature on the Oats roadmap, but it's achievable through incremental development:

1. **Phase 0** (Foundation): Add Promise type (~2-4 hours)
2. **Phase 1** (State Machines): Transform simple async functions (~1-2 weeks)
3. **Phase 2** (Scheduler): Build synchronous task runner (~1-2 weeks)
4. **Phase 3** (Integration): Connect compiler and runtime (~1-2 weeks)
5. **Phase 4** (Real I/O): Add non-blocking I/O (~3-4 weeks)

**Total Estimated Effort**: 2-3 months of focused development

This feature would move Oats from a toy compiler to a serious systems programming language capable of handling real-world concurrent workloads.
