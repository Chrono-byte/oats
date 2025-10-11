Async/Await design for Oats

Goal
----
Design and implement native async/await support for Oats. This includes:
- Compiler transform: lower `async fn` into a heap-allocated state machine implementing a poll interface.
- Runtime: a Promise/Future object representation, an executor/task queue, and waker registration.
- Interop: `await` lowered to poll a Promise; `async` functions return `Promise<T>`.

High-level design
-----------------
1) State machine transformation
- Each `async fn foo(...) -> T` becomes:
  - A `FooStateMachine` nominal (heap object) containing:
    - state: i32 (or enum tag)
    - fields for locals that must live across `await`
    - boxed futures/promises held as i8* pointers
    - a vtable/metadata slot for the poll entry function
  - A `foo_async_ctor(...) -> i8*` helper that allocates the state machine and returns a `Promise<T>` object wrapping it.
  - A `foo_poll(state_ptr: i8*, task_waker: i8*) -> i64` (or similar) function that advances the state machine and returns a status (pending/ready) and optionally writes the output into a provided out-param or returns via register.

2) Promise representation
- Promise object layout:
  [ header (rc/flags) u64 ][ i64 meta? ][ state_machine_ptr i8* ][ result slot i8* or f64 ]
- `Promise<T>` holds a strong reference to the state machine. When polled, the runtime calls the poll function on the state machine.

3) Executor / Waker
- A runtime executor maintains a task queue of ready-to-poll Promises.
- When `await` polls a Promise and it returns Pending, the state machine registers a waker (opaque pointer) with the resource and returns control.
- On wake, runtime re-enqueues the task.

4) Await lowering (compiler)
- Lower `let x = await expr;` to:
  - Evaluate `expr` to a `Promise<T>` object pointer
  - Call runtime `promise_poll_into(promise_ptr, &out_slot)` (or call state_machine poll with current task/waker)
  - If poll returns Ready, use `out_slot` value; else suspend. In the initial MVP (single-threaded), implement a cooperative/synchronous runtime where `await` polls and, if Pending, returns immediately (or error) — later expand to full suspension/resume.

Phased plan (MVP -> Full)
--------------------------
Phase 0 (MVP - small step)
- Implement runtime helpers: `promise_resolve(ptr) -> promise` (wrap an immediate value), `promise_poll_into(promise, out_ptr) -> i32` (0=pending,1=ready), simple `sleep_ms_async(ms) -> promise` stub that becomes ready after sleeping (blocking implementation acceptable for MVP).
- Compiler: lower `await` to `promise_poll_into` and for `async fn` keep returning an immediately-resolved `Promise` (no state machine). This lets existing examples run and sets the ABI.

Phase 1 (cooperative executor)
- Implement heap-allocated state machine layout and poll function generation.
- Implement `async fn` lowering to create state machine and return Promise.
- Implement a simple executor and waker that runs tasks until completion.

Phase 2 (robust runtime)
- Non-blocking IO integration, multi-threaded executor, proper wakers, and performance tweaks.

API/runtime helpers (initial set)
- `sleep_ms(ms: f64)` (already exists, blocking) — keep for tests.
- `promise_resolve(ptr_or_f64) -> i8*` (create resolved Promise)
- `promise_poll_into(promise_ptr, out_ptr: i8*) -> i32` (0 pending, 1 ready)
- `promise_new_from_state(state_ptr) -> i8*` (wrap state machine)
- `promise_wake(promise_ptr)` / `promise_set_waker(...)` (for phase 1+)

Notes & risks
- Transforming closures and captured variables into the state machine is the most complex part — handling lifetimes and rc_inc/rc_dec is required.
- FFI and external libraries require waker integration.

Next tasks
- Add `docs/ASYNC_AWAIT.md` (this document) — done.
- Add runtime helper stubs for `promise_resolve` and `promise_poll_into` as no-op or minimal implementations so codegen can reference them.
- Add CodeGen declarations for the runtime helpers.
- Prototype lowering of `await` to call `promise_poll_into` (and for now, make `await` return inner expression if the promise is immediately resolved).

If you want, I can implement the Phase 0 helpers and small compiler wiring now.
