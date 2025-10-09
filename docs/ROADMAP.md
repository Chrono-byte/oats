## Oats — Roadmap (refreshed)

This document is the canonical roadmap for the Oats ahead-of-time TypeScript
to native compiler. It replaces older planning notes with a concise, actionable
plan: priorities, milestones, acceptance criteria, QA gates, and immediate
next steps tied to the repository layout (see `crates/oats`, `crates/runtime`,
`examples/`).

Summary goal
------------
Translate a well-scoped subset of TypeScript into efficient, debuggable native
executables using LLVM, while providing deterministic memory semantics via a
reference-counting runtime. Prioritize correctness, tests, diagnostics, and
reproducible builds before large-scale feature expansion.

High-level principles
---------------------
- Small, verifiable steps: every feature change must include tests.
- Keep compiler and runtime separable (`crates/oats` vs `crates/runtime`).
- Prefer correctness over premature optimization; add debug-only runtime
  checks to detect RC violations early.
- User-facing diagnostics should be clear and actionable (see
  `crates/oats/src/diagnostics.rs`).

Phases and timeline
-------------------

Phase 1 — Stabilize & Test (0–3 months)
- Focus: make the toolchain reliable and reproducible.
- Work items:
  - CI that builds the workspace, runs unit tests and snapshots, and runs a
    small set of end-to-end examples.
  - Improve and expand snapshot tests for codegen IR (insta snapshots under
    `crates/oats/tests`), and end-to-end tests that compile and run
    representative examples from `examples/`.
  - Add unit tests for runtime header/RC helpers under `crates/runtime/tests`.
  - Harden diagnostics and ensure parser rules are enforced consistently.
- Success criteria:
  - `cargo test --workspace` passes on CI and locally.
  - A set of 5+ representative examples compile, link and run without leaks or
    crashes in debug mode.

Phase 2 — Language Features & Module Model (3–9 months)
- Focus: add module resolution, interfaces/type-aliases, captured closures,
  and stabilize object/array/string semantics.
- Work items:
  - Module resolution and multi-file compilation (Node-like relative imports
    first, then optional package resolution).
  - Interfaces and type aliases mapped into `OatsType` (see
    `crates/oats/src/types.rs`).
  - Complete closure capture support with correct RC semantics and escape
    analysis or conservative heap allocation fallback.
  - Expand the standard runtime helpers and stdlib surface incrementally.
- Success criteria:
  - Cross-file examples compile and run.
  - Captured closures behave correctly in unit and integration tests.

Phase 3 — Advanced Features & Production Polish (9–24 months)
- Focus: generics (monomorphization), try/catch, better optimization, and
  production-grade runtime and packaging.
- Work items:
  - Monomorphization pipeline for generics or validated type-erasure tradeoff.
  - Exception/try-catch support or an ergonomic Result-like alternative.
  - Release process, ABI stability guidance, and performance baselines.

Component map (where to look)
-----------------------------
- Compiler: `crates/oats/src`
  - `parser.rs` — parsing, semicolon rules, AST hygiene
  - `types.rs` — `OatsType` mapping and helpers
  - `codegen/` — IR emission (emit.rs, expr.rs, stmt.rs, helpers.rs)
  - `diagnostics.rs` — user-facing errors
- Runtime: `crates/runtime/src`
  - RC helpers, header layout, object/string/array layouts, and tests
- Examples & tests: `examples/`, `crates/oats/tests/`, `crates/runtime/tests/`

Priority work items (ordered)
----------------------------
1. CI + reproducible build (high)
   - Ensure CI runs `cargo build` and `cargo test --workspace` and can run a
     handful of compiled examples safely (or at least produce build artifacts).

2. Runtime correctness tests (high)
   - Add tests asserting header layout, `rc_inc`/`rc_dec` semantics,
     weak-upgrade/weak-dec behavior, and destructor invocation order.

3. Snapshot coverage for codegen (high)
   - Extend `insta` snapshots where missing and adopt a policy for snapshot
     updates: PRs that update snapshots must include a human explanation.

4. Module resolver & multi-file compilation (medium)
   - Implement relative import resolution and a module graph. Start simple;
     avoid node_modules resolution in the first iteration.

5. Captured closures (medium)
   - Finish escape analysis, closure env lowering, and calling ABI. Conservative
     heap-allocation fallback is acceptable initially.

6. Generics strategy (long)
   - Decide monomorphization vs type erasure and prototype the chosen approach.

Quality gates and CI
--------------------
- Build: workspace builds for debug and release targets. Command: `cargo
  build --workspace --all-targets`.
- Tests: `cargo test --workspace` runs unit, integration, and snapshot tests.
- Clippy: `cargo clippy --all-targets` should be run in PRs; warnings must be
  addressed for non-experimental code.
- Snapshots: `insta` snapshot updates must include explanations in PRs.
- Smoke: CI should produce at least one native executable from `examples/`
  and either run it in a sandbox or keep the artifact for manual inspection.

Testing strategy (practical)
---------------------------
- Unit tests: small, fast tests for parser/type mapping/runtime primitives.
- Snapshot tests: capture emitted LLVM IR for representative AST nodes and
  language constructs.
- Integration tests: compile + link + run examples that exercise the runtime.
- Regression tests: convert bug reproducer examples into snapshot or
  integration tests to prevent regressions.

Acceptance criteria for PRs
--------------------------
- New behavior must include tests.
- Breaking changes must be documented and justified.
- Snapshot updates must explain why the change is correct or desired.
- PRs that touch RC or memory layout must include runtime tests demonstrating
  correctness (usually under `crates/runtime/tests/`).

Developer workflow & checklist
-----------------------------
Before merging:
- Run `source ./scripts/setup_env.sh` (if using local LLVM) and `cargo test
  --workspace`.
- Run `cargo clippy --all-targets -- -D warnings` for stricter PRs.
- If changing codegen, include `insta` snapshot or explain why snapshot
  updates are necessary.

Security & sandboxing
---------------------
- Avoid running untrusted compiled programs in CI without sandboxing. If a
  CI job needs to run generated native code, run it in a container with strict
  resource limits or run only in an allowlist environment.

Risks and mitigations
---------------------
- RC invariants broken by codegen changes → memory corruption.
  - Mitigation: add a debug runtime mode with header invariants checks and
    leak detection; add unit tests that exercise destructor ordering.

- Snapshot churn hides regressions.
  - Mitigation: require human explanation for snapshot updates and add
    regression tests that assert user-visible behaviors.

Immediate next steps (choose one and I will implement it)
-------------------------------------------------------
Pick one of the items below and I will implement it next in this repo:

1) Add CI workflow: Create a GitHub Actions workflow that runs `cargo test
   --workspace`, `cargo clippy`, and produces artifacts for a small set of
   examples.
2) Add runtime unit tests: Add focused tests under `crates/runtime/tests` for
   header layout, `rc_inc`/`rc_dec` and weak-upgrade semantics.
3) Add CONTRIBUTING.md: Document the PR checklist, how to run tests locally,
   and how snapshot updates should be handled.

If you'd like me to proceed, tell me which of the three (CI / runtime tests /
CONTRIBUTING) to implement and I'll make the change, run tests, and report
back with results.

Appendix A — Runtime header quick reference
-----------------------------------------
- Header (8 bytes) layout summary used across the runtime and documented here
  for quick reference:
  - Bits 0–31: strong reference count (u32)
  - Bit 32: static bit (1 = immortal/static)
  - Bits 33–48: weak reference count (16 bits)
  - Bits 49–63: type tag & flags

Appendix B — Useful commands
----------------------------
source ./scripts/setup_env.sh    # set up LLVM environment (when required)
cargo test --workspace           # run all tests
cargo run -p oats --bin aot_run -- examples/cycle_reclaim.oats

Appendix C — Where to find things
---------------------------------
- Compiler source: `crates/oats/src`
- Runtime source: `crates/runtime/src`
- Tests and examples: `crates/oats/tests`, `crates/runtime/tests`,
  `examples/`

---

Short closing note
------------------
This refreshed roadmap focuses the next cycle on stability, CI, tests, and a
clear path to adding language features. Tell me which immediate task you'd
like me to implement (CI, runtime tests, or CONTRIBUTING.md) and I'll do it
and validate the result.

**Effort:** 3-4 weeks | **Complexity:** Medium

```typescript
// Target support:
const { x, y } = point;
const [first, second] = array;
function fn({ name, age }: User) {}
```

#### 11. Tuples 🟢

**Effort:** 1-2 weeks | **Complexity:** Low

```typescript
// Target support:
const pair: [string, number] = ["age", 42];
```

#### 12. Spread & Rest Operators 🟡

**Effort:** 2-3 weeks | **Complexity:** Medium

```typescript
// Target support:
const arr = [1, 2, ...otherArr];
const obj = { x: 1, ...otherObj };
function sum(...nums: number[]) {}
```

### Expected Phase 2 Outcome

- **Compatibility:** ~40-50% of typical applications
- **Can Compile:** Medium frameworks, React applications
- **Examples:** Full Express servers, complex React apps

---

## Long-term Vision (Phase 3)

**Goal:** Production-ready, npm-compatible, ecosystem integration\
**Target Compatibility:** 80%+ of TypeScript code\
**Timeline:** 12+ months, 3-5 developers

### Advanced Type System

- [ ] Higher-kinded types
- [ ] Conditional types: `T extends U ? X : Y`
- [ ] Mapped types: `{ [K in keyof T]: ... }`
- [ ] Template literal types
- [ ] Intersection types: `A & B`
- [ ] Discriminated unions
- [ ] Index signatures: `[key: string]: any`

### Module System (Advanced)

- [ ] `node_modules` resolution
- [ ] Package.json parsing
- [ ] `@types/*` definitions
- [ ] CommonJS interop: `require()`
- [ ] ESM: `import.meta`, dynamic `import()`

### Standard Library (Complete)

- [ ] Full `Array` prototype methods
- [ ] Full `String` prototype methods
- [ ] `Map`, `Set`, `WeakMap`, `WeakSet`
- [ ] `RegExp`, `Date`, `JSON`
- [ ] `Promise`, async iterators
- [ ] `Proxy`, `Reflect`

### Ecosystem Integration

- [ ] `tsconfig.json` support
- [ ] Source maps and debugging
- [ ] Incremental compilation
- [ ] Module bundling
- [ ] FFI & native interop

### Performance & Optimization

- [ ] Inline functions
- [ ] Dead code elimination
- [ ] Loop optimizations
- [ ] Escape analysis (stack allocation)

---

## Async/Await Implementation Plan

### Overview

Async/await is one of the most complex features to implement, requiring deep
integration between compiler and runtime. This section provides a comprehensive
implementation plan.

### Architecture: State Machines + Runtime Scheduler

#### Compiler-Side Transformation

**1. Create State Struct** For each async function, create a struct containing:

- All local variables (persist when function pauses)
- State field (integer) to track execution point
- Saved intermediate values from expressions

```typescript
// Source:
async function fetchUser(id: number): Promise<User> {
    let response = await fetch(`/api/users/${id}`);
    let user = await response.json();
    return user;
}

// Transformed to state struct:
struct FetchUserState {
    state: i32,           // Current execution point
    id: f64,              // Parameter
    response: ptr,        // Local variable
    user: ptr,            // Local variable
    temp_result: ptr,     // Temporary for await results
}
```

**2. Rewrite Function Body** Transform the async function into a resume function
with switch-based dispatch:

```rust
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
            // Continue to next await...
        }
        // ... more states
    }
}
```

**3. Lower `await` Expressions** Each `await` becomes:

1. Execute the awaited expression
2. Save current state
3. Return "pending" status to runtime
4. Create continuation point for resume

#### Runtime-Side Execution

**1. Task Queue** Runtime maintains queue of ready-to-run tasks:

```c
typedef struct Task {
    void* state_ptr;           // Pointer to state machine struct
    TaskResumeFn resume_fn;    // Function to resume execution
    struct Task* next;         // Next task in queue
} Task;
```

**2. Event Loop** Core scheduler loop:

```c
void runtime_run_event_loop() {
    while (has_pending_tasks()) {
        Task* task = dequeue_ready_task();
        TaskStatus status = task->resume_fn(task->state_ptr);
        
        if (status == TASK_COMPLETE) {
            free_task(task);
        } else if (status == TASK_PENDING) {
            // Will be rescheduled by promise completion
        }
    }
}
```

**3. Promise Object** Link tasks together and enable scheduler to resume waiting
tasks:

```c
typedef struct Promise {
    i64 header;               // Standard RC header
    uint8_t status;           // Pending/Resolved/Rejected
    void* result;             // Result value (when resolved)
    Task** waiting_tasks;     // Array of tasks waiting on this promise
    size_t waiting_count;     // Number of waiting tasks
} Promise;
```

### Implementation Phases

#### Phase 0: Foundation (2-4 hours)

- [ ] Add `Promise` object layout to heap design
- [ ] Add `OatsType::Promise(Box<OatsType>)` to type system
- [ ] Write unit tests for Promise representation

#### Phase 1: State Machine Transformation (1-2 weeks)

- [ ] Add `is_async` detection in function parsing
- [ ] Implement `gen_async_function_ir()` for simple case
- [ ] Transform single `await` expression
- [ ] Generate switch-based resume function
- [ ] Test: compile async function to IR

#### Phase 2: Minimal Scheduler (1-2 weeks)

- [ ] Implement task queue in runtime
- [ ] Add `runtime_schedule_task()` function
- [ ] Add `runtime_run_event_loop()` function
- [ ] Implement Promise with status/result fields
- [ ] Test: manually schedule tasks

#### Phase 3: Integration (1-2 weeks)

- [ ] Codegen for `await` calls runtime functions
- [ ] Async function wrapper schedules tasks
- [ ] Main function calls event loop
- [ ] Test: end-to-end async function execution

#### Phase 4: Real I/O (3-4 weeks)

- [ ] Integrate with OS event systems (epoll/kqueue/IOCP)
- [ ] Implement `setTimeout()` using timer wheel
- [ ] Implement async file I/O
- [ ] Implement async network I/O
- [ ] Test: real async operations

**Total Estimated Effort:** 2-3 months focused development

---

## Consolidated Priorities

### High-level Goals

- **Remove panic sites** in compiler by migrating to `Result<_, Diagnostic>`
- **Modularize** large `codegen/mod.rs` into smaller modules
- **Harden runtime** with bounds checks and error handling
- **Add CI** that ensures correctness across platforms

### Short-term (1-3 days)

1. **Finish error handling migration** (incremental)
   - Strategy: Add `lower_*_result` adapters, convert callers gradually
   - Safety: Small commits, run `cargo build -p oats` after each
   - Tests: Add tests that ensure diagnostics instead of panics

2. **Member-write lowering** (0.5-1 day)
   - Implement `obj.field = expr` with RC semantics
   - Add test asserting correct IR (getelementptr + store + rc_inc/rc_dec)

3. **Add CI for LLVM-18** (1 day)
   - Use `scripts/setup_env.sh` as reference
   - Install `llvm-18` on Ubuntu runners and set env variables

### Medium-term (weeks)

1. **Modularize CodeGen** (batched approach)
   - Batch A: Create `expr.rs` and `stmt.rs`, move small helpers
   - Batch B: Move function-level emission to `gen.rs`
   - Batch C: Common utilities to `helpers.rs`, tidy imports

2. **Core language features**
   - Arrow functions (non-capturing)
   - Object literals
   - Module resolution (relative imports first)
   - Standard library basics

### Long-term (months)

- **Closures** with boxed environments and capture semantics
- **Memory management** improvements (cycle detection or tracing GC)
- **Async/await** lowering and runtime scheduler

---

## Timeline & Effort Estimates

### Phase 1 Breakdown (6-12 months, 1-2 developers)

| Feature                       | Effort          | Complexity | Priority |
| ----------------------------- | --------------- | ---------- | -------- |
| Arrow functions (non-closing) | 1-2 weeks       | Low        | P0       |
| Interfaces & type aliases     | 2-3 weeks       | Medium     | P0       |
| Union types (basic)           | 2-3 weeks       | Medium     | P1       |
| Object literals               | 1-2 weeks       | Low-Medium | P1       |
| Template literals             | 1 week          | Low        | P2       |
| Module resolution             | 4-6 weeks       | High       | P0       |
| Standard library basics       | 2-4 weeks       | Medium     | P1       |
| **Total Phase 1**             | **14-22 weeks** |            |          |

### Phase 2 Breakdown (6-12 months, 2-3 developers)

| Feature                 | Effort          | Complexity | Priority |
| ----------------------- | --------------- | ---------- | -------- |
| Closures                | 4-6 weeks       | High       | P0       |
| Generics                | 6-8 weeks       | Very High  | P0       |
| Async/await             | 8-12 weeks      | Very High  | P0       |
| Destructuring           | 3-4 weeks       | Medium     | P2       |
| Optional/default params | 1-2 weeks       | Low        | P2       |
| Try/catch               | 4-6 weeks       | High       | P1       |
| Tuples                  | 1-2 weeks       | Low        | P2       |
| Spread/rest             | 2-3 weeks       | Medium     | P1       |
| **Total Phase 2**       | **30-48 weeks** |            |          |

### Phase 3 Breakdown (12+ months, 3-5 developers)

- **Advanced language features:** 6-12 months
- **Ecosystem integration:** 6-12 months
- **Production hardening:** 6-12 months

### Success Metrics

#### Compatibility Milestones

- [ ] **10% milestone:** Simple utility libraries (lodash basics)
- [ ] **25% milestone:** Small frameworks (basic Express routes)
- [ ] **50% milestone:** Medium applications (React components)
- [ ] **75% milestone:** Large frameworks (full Express, React)
- [ ] **90% milestone:** Most npm packages

#### Performance Targets

- [ ] Compilation speed: <100ms per 1000 LOC
- [ ] Generated code: Within 2x of V8/JIT performance
- [ ] Memory overhead: <10MB baseline

#### Quality Targets

- [ ] Test coverage: >80%
- [ ] Zero crashes on valid TypeScript
- [ ] Graceful errors on invalid code
- [ ] Documentation for all public APIs

---

## Next Steps

### Immediate (This Sprint)

1. ✅ **Arrow functions (non-closing)** - Quick win, high impact
2. ✅ **Object literals** - Unblocks many patterns
3. ✅ **Template literals** - Quality-of-life improvement

**Expected Outcome:** Jump from <1% to ~10% compatibility

### Next Quarter (3 months)

4. ✅ **Interfaces & type aliases** - Critical for any real code
5. ✅ **Union types (basic)** - Nullable types, option types
6. ✅ **Module resolution** - Multi-file compilation

**Expected Outcome:** ~15-20% compatibility, can compile small libraries

### Next 6 Months

7. ✅ **Standard library basics** - Console, Math, Array methods
8. ✅ **Closures** - Unlocks functional programming
9. ✅ **Try/catch** - Error handling

**Expected Outcome:** ~30-40% compatibility, can compile real applications

---

## Contributing

### Good First Issues

- Arrow functions (non-closing)
- Template literals
- Tuples
- Object literals

### Harder Issues (Need Mentoring)

- Module resolution
- Closures
- Generics
- Async/await

### How to Help

1. **Pick a feature** from Phase 1, check effort estimate
2. **Open an issue:** "Implement [feature name]"
3. **Get feedback** on approach
4. **Submit incremental PRs** (small, tested changes)
5. **Document your work**

---

This roadmap represents a living document that evolves based on user feedback,
technical discoveries, and resource availability. The focus remains on building
a production-quality TypeScript compiler that can handle real-world codebases
while maintaining the performance benefits of AOT compilation.
