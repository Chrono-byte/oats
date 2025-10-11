Oats Async Phase 1 — State Machine & Executor Design

Goal
----
Design and implement Phase 1: compile-time transformation of `async fn` into
heap-allocated state machines and a minimal single-threaded executor/waker
model that supports `await` suspension/resume for simple cases.

Overview
--------
Phase 1 turns the Phase 0 plumbing into real async behavior with these
components:

1) Compiler transformation
- Each `async fn foo(params) -> T` becomes two artifacts:
  a) `foo_async_ctor(params) -> i8*` — allocates and returns a Promise
     wrapping the state machine (unstarted state).
  b) `foo_poll(state_ptr: i8*, task_ptr: i8*) -> i32` — the poll function that
     advances the state machine and returns status (0=Pending,1=Ready).
- The state machine layout is a heap object with header+meta and fields for
  local variables that must live across await points.
- The function body is lowered into a switch over `state` where each case
  corresponds to code between suspension points. `await` becomes:
  - spawn the awaited promise's poll; if pending, store continuation state
    and return Pending; if ready, load result and continue.

2) Promise representation
- A Promise object wraps:
  [header (rc/flags) u64][meta slot 8][state_machine_ptr i8*][result_slot i8*][waker_ptr i8*]
- Promise API (runtime):
  - `promise_new(state_ptr) -> promise_ptr`
  - `promise_poll_into(promise_ptr, out_ptr, task_ptr) -> i32`
  - `promise_set_waker(promise_ptr, waker_ptr)`

3) Executor & Waker model (MVP)
- Single-threaded executor with a global/heap-backed task queue.
- Tasks are Promises whose state machines are polled via `foo_poll`.
- Waker is an opaque pointer that, when invoked, enqueues the associated
  task back onto the executor queue.
- For now, IO helpers can synchronously call the waker when ready.

4) Call/await lowering
- `await expr` lowered to:
  - evaluate `expr` -> `promise_ptr`.
  - call `promise_poll_into(promise_ptr, out_slot, current_task)`.
  - if returns Ready (1), load out-slot and continue.
  - if returns Pending (0), store continuation state and return Pending from
    the current poll function.

Implementation Plan: incremental steps
-------------------------------------
Step A — Runtime & ABI
- Define concrete ABI for poll functions: `i32 poll(i8* state_ptr, i8* task_ptr, i8* out_ptr)` returning 0/1.
- Implement runtime primitives in `crates/runtime/src/lib.rs`:
  - `promise_new_from_state(state_ptr)`
  - `promise_poll_into(promise_ptr, out_ptr, task_ptr)` — calls state machine poll fi
  - `executor_enqueue(promise_ptr)` and `executor_run()` (single-threaded loop)
  - `waker_create(task_ptr)` and `waker_wake(waker_ptr)`

Step B — Codegen scaffolding
- Add getters for the runtime functions in `crates/oats/src/codegen/mod.rs`.
- Implement lowering/prototype: when encountering an `async fn` declaration,
  emit `foo_async_ctor` that allocates a state object (with minimal fields) and returns a Promise via `promise_new_from_state`.
- Lower `await` to call `promise_poll_into(..., current_task)` and branch on result.

Step C — State machine generation
- Implement the basic transform to split function into states at each `await`.
- Allocate fields into the state machine for variables that live across awaits.
- Implement `foo_poll` as a function with a state-switch and continuation points.

Step D — Executor & integration tests
- Implement a simple executor that polls tasks until completion.
- Add tests: simple `async` functions, `sleep_ms_async` that enqueues a timer, and validate basic scheduling.

Edge cases & considerations
- Captured closures and nested `async` functions — scope-splitting required.
- RC rules: ensure state machine holds references properly (rc_inc on store, rc_dec on drop).
- `this` handling in methods — `async` methods must capture `this` in state.
- Exception/throw propagation: decide semantics for reject vs panic.

Milestones & acceptance criteria
- M1: `async fn` that `await`s an immediately-ready promise returns and executor completes.
- M2: `async fn` with two await points suspends/resumes correctly.
- M3: Integration of `sleep_ms_async` as an example: awaiting a timer resumes after expiration.

Next action
-----------
If you want, I'll start Step A and implement the runtime primitives (`promise_new_from_state`, `promise_poll_into` that dispatches to poll functions, a very small executor) and wire the CodeGen declarations. Then in Step B I'll emit a prototype `foo_async_ctor` for the `runAsyncCode` example so the harness executes the async code via the executor.

Tell me to proceed and I'll start Step A now.