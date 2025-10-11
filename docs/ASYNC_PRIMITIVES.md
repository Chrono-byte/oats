# Oats Async Primitives — Complete Specification

**Status:** Detailed Design Document - Partially Implemented  
**Last Updated:** October 10, 2025  
**Implementation Status:** Phase 0 primitives complete, Phase 1 primitives pending

## Purpose
This document specifies a complete set of runtime primitives, ABIs, object
layouts, and codegen contracts needed to implement native async/await in
Oats. It focuses on Phase 1: producing correct cooperative async semantics
with a single-threaded executor, waker model, and state-machine lowering.

Goals
-----
- Define stable ABIs and object layouts for Promise and state-machine objects.
- Provide a minimal but functional runtime API (promise creation, polling,
  wakers, executor queue, timer helper) with well-defined semantics.
- Specify codegen contracts for lowering `async fn`/`await` and generating
  state machines.
- Document RC (reference-counting) rules and lifetime invariants.

ABI and Object Layouts
----------------------
All heap objects follow the project's unified header layout (see
`docs/ARCHITECTURE.md`). Here we define the memory layout for Promise and
StateMachine objects (offsets in bytes):

Common: header (8 bytes) | meta-slot (8 bytes)

Promise object (base ptr returned by runtime):
- offset 0: header (rc/flags) 8 bytes
- offset 8: meta-slot (reserved) 8 bytes
- offset 16: state_ptr (i8*) — pointer to state machine or NULL for resolved promises
- offset 24: result_slot (i8* or f64 stored according to Promise<T>)
- offset 32: waker_ptr (i8*) — opaque waker or NULL
- total: at least 40 bytes + padding for alignment

StateMachine object (base ptr returned by ctor):
- offset 0: header
- offset 8: meta-slot
- offset 16: vtable/meta pointer (optional)
- offset 24: i32 state_tag (4 bytes) + padding
- offset 32: fields... (captured locals that live across await points)

Poll function ABI
-----------------
Each state machine implements a poll function with the signature (C ABI):

    // poll(state_ptr: i8*, waker_ptr: i8*, out_ptr: i8*) -> i32
    // Returns: 0 = Pending, 1 = Ready
    int64_t oat_poll(i8* state_ptr, i8* waker_ptr, i8* out_ptr)

Notes:
- `out_ptr` is an i8** pointer (pointer-sized slot) where the poll function
  writes the resolved value (pointer or boxed value) when returning Ready.
- For numeric-only Promise<T> (T is number), `out_ptr` points to an f64 slot
  (or agreed-upon ABI) — we will use pointer slot for uniformity (box numbers
  in Phase 1 if needed) to simplify ABI.

Runtime primitives (Phase 1)
---------------------------
(Declared with `#[no_mangle] pub extern "C" fn ...`)

1) promise_new_from_state(state_ptr: *mut c_void) -> *mut c_void
   - Create a Promise object wrapping the provided state machine.
   - The returned pointer is a fully-formed Promise with rc=1.
   - The promise.state_ptr points to the provided state_ptr.

2) promise_resolve(ptr: *mut c_void) -> *mut c_void
   - Create an already-resolved Promise whose result_slot holds `ptr`.
   - Useful for `Promise.resolve(...)` and tests.

3) promise_poll_into(promise_ptr: *mut c_void, out_slot: *mut c_void, waker_ptr: *mut c_void) -> i32
   - If the promise is resolved, write the result into out_slot and return 1.
   - If it wraps a state machine, call the state's poll function (via
     stored vtable/known symbol) with the supplied waker_ptr and out_slot.
   - If the poll returns Ready, write out and return 1; else return 0.

4) waker_create_for_task(promise_ptr: *mut c_void) -> *mut c_void
   - Create an opaque waker value that, when invoked, enqueues the
     `promise_ptr` into the runtime executor.

5) waker_wake(waker_ptr: *mut c_void)
   - When external events are ready they call this to re-enqueue the
     associated task.

6) executor_enqueue(promise_ptr: *mut c_void)
   - Add promise to the executor's ready queue. Increments rc while queued.

7) executor_run()
   - Run the single-threaded executor loop until the ready queue is empty.
   - For each queued promise, call `promise_poll_into(p, out, NULL)`.
   - If poll returns Pending, leave it alone; if Ready, finalize and drop.

8) sleep_async(ms: f64) -> *mut c_void (optional)
   - Create a Promise that completes after `ms` milliseconds. For phase 1
     this can spawn a background thread that sleeps and then calls
     `waker_wake` — acceptable for a prototype.

Codegen contracts and lowering
------------------------------
- `async fn foo(...) -> T` codegen emits two symbols:
  1) `<foo>_async_ctor` : allocates state machine, initializes captured
     fields, sets state_tag=0, wraps in a Promise via `promise_new_from_state`.
     Returns `i8*` (promise pointer).
  2) `<foo>_poll` : function implementing the poll-switch logic with signature
     `i64 (<i8*> state_ptr, i8* waker_ptr, i8* out_ptr)`.

- `await expr` lowering inside a poll function:
  1) Evaluate `expr` to `promise_ptr`.
  2) Create an out_slot (i8* alloca) to receive result.
  3) Create or obtain current waker (waker_create_for_task(current_promise)).
  4) Call `promise_poll_into(promise_ptr, out_slot, waker_ptr)`.
  5) If returns Ready (1): load out_slot and continue.
     If returns Pending (0): store current `state_tag`, store any live locals
+    into the state machine fields, return Pending (0) from current poll.

Reference counting rules
------------------------
- When storing pointers into state machine fields, call `rc_inc` on the
  pointer and `rc_dec` on overwritten values where applicable.
- When a Promise is enqueued in the executor, `executor_enqueue` increments
  the RC; executor removes its ref when task completes.
- Dropping a state machine should `rc_dec` all its pointer fields appropriately.

Testing & Validation plan
-------------------------
- Unit tests in `crates/runtime/tests/` for promise primitives and executor.
- Integration test: Lower `runAsyncCode` example to state machine and run
  `executor_run()` to ensure the async flow completes.

Implementation steps (detailed)
-------------------------------
1) Runtime primitives (lib.rs)
   - Implement data structures (VecDeque) for executor queue.
   - Implement promise object creation helpers and `promise_poll_into`
   - Implement waker objects as small boxed structs that contain a pointer
     to the promise/task and call `executor_enqueue` when woken.
2) Codegen runtime getters
   - Add `get_promise_new_from_state`, `get_promise_poll_into`, `get_executor_enqueue`, `get_executor_run`, `get_waker_create`, `get_waker_wake` getters in `CodeGen`.
3) Codegen lowering
   - Emit `_async_ctor` and `_poll` functions for small async functions.
   - Lower `await` to poll logic and state save/restore pattern.
4) Tests & iterate
   - Add tests, run harness, debug.

Open questions
--------------
- Exception semantics: should rejections be modeled separately from panics?
  For Phase 1 we can ignore rejection or map it to panic until we implement
  a richer `Result`/`Reject` handling.
- Numeric Promise<T> ABI: boxing numbers vs separate numeric out slot. For MVP
  we'll box numbers into heap objects to use a uniform pointer-based out_slot.

Next action
-----------
If you approve, I'll start implementing the runtime primitives (executor,
promise_poll_into, waker) in `crates/runtime/src/lib.rs` and add codegen
getters in `crates/oats/src/codegen/mod.rs`. Then we'll prototype a
state-machine for `runAsyncCode`.
