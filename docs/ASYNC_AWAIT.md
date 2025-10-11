# Async/Await Design for Oats

**Status:** Phase 0 Complete, Phase 1 In Progress  
**Last Updated:** October 10, 2025

## Table of Contents

1. [Overview](#overview)
2. [Current Implementation Status](#current-implementation-status)
3. [Design Goals](#design-goals)
4. [High-Level Architecture](#high-level-architecture)
5. [Implementation Phases](#implementation-phases)
6. [Technical Details](#technical-details)
7. [Examples](#examples)
8. [Future Work](#future-work)

## Overview

This document describes the design and implementation of native async/await support for Oats. The implementation transforms `async` functions into state machines that can suspend and resume execution, enabling non-blocking asynchronous operations.

### Key Concepts

- **`async function`** - Function that returns a `Promise<T>` and can contain `await` expressions
- **`await` expression** - Suspends execution until a Promise completes
- **`Promise<T>`** - Object representing eventual completion of an async operation
- **State Machine** - Compilation target for async functions, with explicit suspend/resume points
- **Executor** - Runtime component that schedules and runs async tasks

## Current Implementation Status

### ✅ Phase 0: Basic Infrastructure (Complete)

**Type System Support:**
- `Promise<T>` type fully integrated
- Type inference for Promise-returning functions
- Promise type checking and validation
- Generic Promise types (`Promise<number>`, `Promise<string>`, etc.)

**Runtime Primitives:**
```c
// Create an immediately-resolved Promise
promise_resolve(ptr: *mut c_void) -> *mut c_void

// Poll a Promise for completion
promise_poll_into(promise: *mut c_void, out: *mut c_void, task: *mut c_void) -> i32

// Create Promise from state machine
promise_new_from_state(state: *mut c_void) -> *mut c_void
```

**Codegen:**
- `async function` declarations recognized and parsed
- `await` expressions lower to `promise_poll_into` calls
- Promise values properly typed and passed through IR
- Basic placeholder state machine allocation

**Testing:**
- 8 Promise type tests passing
- Basic async/await example compiles and runs
- Runtime helpers unit tested
- Example: `examples/proper_tests/async_await.oats`

### ⏳ Phase 1: State Machines (40% Complete)

**Completed:**
- Async function signature tracking
- Await point identification
- Resume index counter

**In Progress:**
- State struct generation from local variables
- Poll function with state switch
- Local variable lifetime analysis
- Closure capture in async context

**Pending:**
- Complete state machine transformation
- Cooperative executor implementation
- Waker registration and task queue
- Error propagation through async boundaries

## Design Goals

### Core Requirements

1. **Compatibility:** Match TypeScript/JavaScript async/await semantics
2. **Performance:** Zero-cost when not using async features
3. **Safety:** Memory-safe with proper RC handling
4. **Debugging:** Good error messages and stack traces

### Non-Goals (For Now)

- Multi-threaded executor (Phase 2+)
- Async iterators and generators
- Top-level await
- Structured concurrency primitives

## High-Level Architecture

### Compilation Strategy

```
async function foo() {        State Machine:
    println("Start");         struct FooState {
    let x = await bar();  →       state: i32,
    println(x);                   x: string,  // captured local
    return x + "!";               temp: *Promise
}                             }
                              
                              poll(state, waker, out) {
                                  switch state.state {
                                      0: { /* Start */ }
                                      1: { /* After await */ }
                                      ...
                                  }
                              }
```

### Runtime Architecture

```
┌──────────────────────────────────────┐
│         User Async Code              │
│  async fn foo() { await bar(); }     │
└──────────────┬───────────────────────┘
               │ (compiler transform)
               ↓
┌──────────────────────────────────────┐
│       State Machine + Poll Fn        │
│  FooState { state, locals }          │
│  foo_poll(state, waker, out) → i32   │
└──────────────┬───────────────────────┘
               │ (wrapped in)
               ↓
┌──────────────────────────────────────┐
│          Promise<T> Object           │
│  [header][meta][state_ptr][result]   │
└──────────────┬───────────────────────┘
               │ (scheduled by)
               ↓
┌──────────────────────────────────────┐
│         Executor + Waker             │
│  TaskQueue, run_until_complete()     │
└──────────────────────────────────────┘
```

## Implementation Phases

### Phase 0: MVP Infrastructure ✅

**Duration:** Complete  
**Goal:** Enable basic async/await syntax with immediate resolution

**Deliverables:**
- [x] `Promise<T>` type in type system
- [x] Parse `async` function declarations
- [x] Parse `await` expressions
- [x] Runtime: `promise_resolve`, `promise_poll_into`
- [x] Codegen: lower `await` to runtime calls
- [x] Tests: type system and basic codegen

**Result:** Can write and compile async/await code, but everything resolves immediately.

### Phase 1: State Machine Transform ⏳

**Duration:** 2-3 weeks (40% complete)  
**Goal:** True suspend/resume with state preservation

**Work Items:**
1. **State Struct Generation** (Week 1)
   - Analyze function body for local variables
   - Determine which locals live across `await` points
   - Generate state struct layout
   - Allocate state on heap

2. **Body Transformation** (Week 1-2)
   - Split function at each `await` point
   - Generate state machine with switch
   - Store continuation state before suspend
   - Restore locals after resume

3. **Poll Function** (Week 2)
   - Generate `foo_poll(state, waker, out)` function
   - Implement state switch logic
   - Handle inner Promise polling
   - Return Pending or Ready status

4. **RC Handling** (Week 2)
   - Increment RC when storing into state
   - Decrement RC on state machine drop
   - Handle RC across suspend points
   - Test with complex object graphs

5. **Testing** (Week 2-3)
   - Multiple await points
   - Nested async calls
   - Async methods on classes
   - Error handling

**Acceptance Criteria:**
- Async function with 2+ awaits suspends and resumes correctly
- Local variables preserved across await points
- RC handled correctly (no leaks or use-after-free)
- Integration test: async example runs to completion

### Phase 2: Cooperative Executor (Future)

**Duration:** 1-2 weeks  
**Goal:** Task queue and waker-based scheduling

**Work Items:**
- Task queue implementation
- Waker objects and registration
- `executor_run()` main loop
- Timer integration (`sleep_async`)
- Tests for concurrent async tasks

### Phase 3: Production Features (Future)

**Duration:** 2-3 weeks  
**Goal:** Complete Promise API and error handling

**Work Items:**
- `Promise.resolve()`, `Promise.reject()`
- `.then()`, `.catch()`, `.finally()` methods
- Promise chaining
- Error propagation
- `Promise.all()`, `Promise.race()`
- Timeout and cancellation

## Technical Details

### State Machine Layout

**State Struct (Heap Allocated):**
```
Offset 0:   u64 header (RC + flags)
Offset 8:   u64 meta_slot (state machine metadata)
Offset 16:  i32 state (current state tag)
Offset 20:  [padding]
Offset 24+: Captured locals (8 bytes each)
```

**Example:**
```typescript
async function process(x: number): Promise<string> {
    let y = x * 2;
    let result = await fetchData(y);
    return result + "!";
}
```

**Generates:**
```
struct ProcessState {
    header: u64,
    meta: u64,
    state: i32,        // 0 = start, 1 = after_fetch
    x: f64,            // parameter
    y: f64,            // local
    result: *string,   // local (pointer)
    temp_promise: *Promise,
}
```

### Poll Function ABI

```c
// Returns: 0 = Pending, 1 = Ready
// out_ptr: where to write result when Ready
int32_t foo_poll(void* state_ptr, void* waker_ptr, void* out_ptr);
```

**State Machine Logic:**
```rust
fn foo_poll(state: *State, waker: *Waker, out: *Out) -> i32 {
    match state.state {
        0 => {  // Initial state
            // ... do work until first await ...
            let promise = some_async_call();
            state.temp_promise = promise;
            state.state = 1;  // Next state
            
            // Try polling the promise
            if poll(promise, waker, &state.result) == READY {
                // Continue to next state immediately
                state.state = 1;
                goto state_1;
            } else {
                return PENDING;  // Suspend
            }
        }
        1 => {  // After first await
            // ... continue with state.result ...
            *out = compute_final(state.result);
            return READY;
        }
    }
}
```

### Promise Object Layout

```
Offset 0:  u64 header (RC + flags)
Offset 8:  u64 meta_slot
Offset 16: *void state_ptr (NULL if resolved)
Offset 24: void* result_slot (value when resolved)
Offset 32: *void waker_ptr (registered waker)
```

**States:**
- **Pending:** `state_ptr != NULL`, poll via state machine
- **Resolved:** `state_ptr == NULL`, `result_slot` contains value

### Memory Management

**Reference Counting Rules:**
1. **State struct:** RC=1 when created, owned by Promise
2. **Captured locals:** RC incremented when stored in state
3. **Promise return:** RC=1, caller responsible for cleanup
4. **Await expression:** RC managed by poll function
5. **State machine drop:** Decrement RC of all captured pointers

**Example RC Flow:**
```typescript
async function example() {
    let obj = new MyClass();  // RC = 1
    await something();         // RC incremented to 2 (stored in state)
    obj.method();              // RC still 2
    return obj;                // RC stays 2 (returned, not dropped)
}
// After function completes, state drops, RC -> 1
```

## Examples

### Example 1: Simple Async Function

**Source:**
```typescript
async function greet(name: string): Promise<string> {
    return Promise.resolve("Hello, " + name);
}

export function main(): void {
    let p: Promise<string> = greet("World");
    // In Phase 1+, would await or execute via executor
}
```

**Current Behavior (Phase 0):**
- Compiles successfully
- Returns immediately-resolved Promise
- `promise_poll_into` returns Ready on first poll

### Example 2: Multiple Awaits

**Source:**
```typescript
async function process(): Promise<void> {
    println("Start");
    let x = await fetchNumber();
    println("Got: " + x);
    let y = await fetchNumber();
    println("Got: " + y);
    println("Done");
}
```

**State Machine (Phase 1):**
```
State 0: Print "Start", call fetchNumber(), await → state 1
State 1: Load x, print, call fetchNumber(), await → state 2
State 2: Load y, print, print "Done", return Ready
```

### Example 3: Async Method

**Source:**
```typescript
class DataFetcher {
    url: string;
    
    async fetch(): Promise<string> {
        let data = await httpGet(this.url);
        return data;
    }
}
```

**State Captures:**
- `this` pointer (RC incremented)
- `data` local variable

## Future Work

### Phase 2+: Advanced Features

**Multi-threaded Executor:**
- Work-stealing task queue
- Thread-safe waker implementation
- Atomic state transitions

**Async I/O:**
- File operations
- Network sockets
- Integration with epoll/kqueue

**Debugging:**
- Async stack traces
- State machine visualization
- Performance profiling

**Optimization:**
- Inline small async functions
- Eliminate state machine for await-free paths
- Stack pinning for short-lived async

### Research Topics

- Zero-copy async (borrowing across await)
- Structured concurrency (task groups, cancellation)
- Async iterators and streams
- Integration with JavaScript event loop (for WASM)

## References

**Design Inspiration:**
- Rust's Future trait and async/await
- TypeScript/JavaScript async/await semantics
- C++20 coroutines
- Python asyncio

**Related Documents:**
- `ASYNC_PHASE1.md` - Detailed Phase 1 implementation plan
- `ASYNC_PRIMITIVES.md` - Complete runtime primitive specification
- `ARCHITECTURE.md` - Memory layout and RC rules
- `ROADMAP.md` - Overall project timeline

---

**Questions or suggestions?** Open an issue on GitHub!

**Want to contribute?** See Phase 1 work items in ROADMAP.md
