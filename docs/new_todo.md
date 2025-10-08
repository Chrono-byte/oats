Oats: Prioritized Project Roadmap (Security & Performance Focus)

This document is a compact, actionable roadmap with the most critical security and performance items at the top. The checklist below is updated to reflect recent safety-focused changes.

1. High-Priority: Security & Performance Hardening

These tasks are critical for making the compiler and the code it generates more robust, secure, and performant.

    [Security] Harden Runtime with Bounds Checking and Error Handling

        Status: Done (basic)

        What I changed: added explicit bounds checks to array helpers in `crates/runtime/src/lib.rs` (array_get_f64, array_get_ptr, array_get_ptr_borrow, array_set_f64, array_set_ptr). On out-of-bounds access the runtime prints a short diagnostic to stderr and aborts, avoiding undefined memory reads/writes.

        Notes: The abort path is intentionally minimal and dependency-free. A future enhancement could return error codes or raise a runtime trap value visible to generated code.

    [Security/Correctness] Eliminate panics in compiler
        Status: In progress (incremental migration underway)

        Recent work: several panic-prone `.unwrap()` / `.expect()` calls were replaced with guarded checks and safe fallbacks. A small, safe migration was started in `crates/oats/src/codegen/mod.rs`: an adapter `lower_expr_result(...) -> Result<_, Diagnostic>` was added and many call-sites were converted one-by-one to `lower_expr_result(...).ok()` so callers keep receiving an Option while diagnostic capture is centralized.

        Safety/process notes: edits were intentionally tiny (one-line or a few lines), committed and build-verified after every change to avoid introducing syntax errors in the large lowering file. The active work branch used for these edits is `restore-93fb7b8` and `cargo build -p oats` was run after each commit during the migration.

        Remaining: finish converting lowering helpers to return `Result<_, Diagnostic>` (so callers can use `?`), sweep remaining `.unwrap()`/`.expect()` in `crates/oats/src/codegen/helpers.rs` and `crates/oats/src/bin/aot_run.rs`, and add integration tests that verify diagnostics are emitted instead of panics on lowering failures.

    [Performance/Security] Implement Memory Cycle Detection

        Status: Not started (design required)

        Suggested approach: prototype a small tracing pass that runs on-demand (e.g., when allocation fails) to collect cycles. Alternatively, implement a deferred cycle-breaker that detects unreachable cycles via a mark-and-sweep of objects known to the runtime.

    [Performance] Compiler and Runtime Optimizations

        Status: Not started

        Suggested first steps: add lightweight profiling (benchmarks, flamegraphs) around the front-end parse/typecheck/codegen boundaries; experiment with LLVM optimization levels and selectively inlining tiny runtime helpers in the generated IR.

    [Security] Secure the CI and Build Process

        Status: Not started

        Suggested first steps: pin Cargo dependencies (Cargo.lock is present but consider adding explicit version constraints in Cargo.toml where appropriate), add `cargo deny` or `cargo audit` to CI, and document required environment variables (e.g., LLVM prefix) in CI workflows.

2. Medium-Priority: Feature Implementation & Correctness

    Implement Dot-Member Access and Class Lowering

        Status: Partially implemented in lowering (for constructors and field storage) but full class support (methods, inheritance, method dispatch) remains pending.

    Expand the Type System

        Status: Not started (arrays are represented and work; unions/tuples/generics require design)

    Improve for-of Loop Lowering

        Status: Basic for-of lowering implemented (checked)

    [Maintenance] Modularize CodeGen (split large mod.rs)

        Status: Not started

        Problem: `crates/oats/src/codegen/mod.rs` is extremely large (~3000+ lines) and contains many responsibilities (expression lowering, statement lowering, function emission, helpers). This makes reviews, incremental edits, and safe refactors (like the Result-based migration) error-prone.

        Goal: Break `codegen/mod.rs` into smaller, focused modules so each area is easier to read, test, and change. Keep the `CodeGen` struct in a small top-level `mod.rs` and move lowering responsibilities into submodules that implement `impl<'a> CodeGen<'a>` methods across files.

        Recommended small-step plan:

        - Audit & plan (1): scan `mod.rs` and produce a short map of logical sections (expr lowering, stmt lowering, function IR emission, helpers, runtime decls). Create a migration checklist mapping methods -> target files.
        - Batch A (low risk, quick): Create `crates/oats/src/codegen/expr.rs` and `crates/oats/src/codegen/stmt.rs`. Move purely-local lowering functions (expression-only helpers and statement visitors) into those files as `impl<'a> CodeGen<'a>` methods. Leave `CodeGen` type and small public helpers in `mod.rs` that `pub mod expr; pub mod stmt;` can reference.
        - Batch B (medium): Move function-level emission (`gen_function_ir`, `emit_host_main`) into `crates/oats/src/codegen/gen.rs` and create `helpers.rs` for smaller utilities currently in `mod.rs` that are reused across modules.
        - Batch C (cleanup): Update `use`/visibility, run `cargo build -p oats` and `cargo test`, and tidy imports. Replace any `super::` cycles with explicit `pub(crate)` helpers where needed.

        Safety rules while refactoring:

        - Make only a few small file moves per commit (1–3 small edits). Run `cargo build -p oats` after each commit.
        - Preserve public APIs (function symbol names used by runtime) and avoid changing semantics in the same commit as a move.
        - Add a smoke-test (run `./target/debug/aot_run ./examples/class_field.oats`) after each major batch to ensure emitted IR still links and runs.

        Quality gates per batch:

        - Build: `cargo build -p oats` (must pass)
        - Format: `cargo fmt` (apply before committing)
        - Tests: `cargo test -p oats` (run quick unit/integration tests if present)
        - Smoke: run the aot runner on one example to ensure no regressions.

        Estimated effort: Batch A (1–3 hours), Batch B (1–2 hours), Batch C (0.5–1 hour) depending on cross-file dependencies and visibility fixes.

        Why this helps now: modularizing the codegen makes the ongoing migration to `Result<_, Diagnostic>` and the removal of `.unwrap()` safer and easier because edits will be smaller, localized, and easier to compile-verify.

3. Low-Priority: Long-Term & Ecosystem

    Implement Closures and Advanced Function Features — Not started

    Implement Async/Await — Not started

    Develop a Standard Library — Not started

Quick Checklist (Status)

    [x] [Security] Runtime bounds checking (implemented in `crates/runtime/src/lib.rs`)

    [~] [Security] Eliminate panics in compiler (partial — multiple unwrap/expect occurrences replaced; full Result-driven refactor recommended)

    [ ] [Performance] Memory cycle detection (design required)

    [ ] [Performance] Compiler and runtime profiling/optimization

    [ ] [Security] Secure CI and build process (pin deps + security scans)

    [x] Basic for-of lowering implemented

    [x] RC helpers (rc_inc/rc_dec) implemented

    [ ] Full class lowering

    [ ] Type-system expansion (unions, generics)

Changes made in this pass

    - `crates/runtime/src/lib.rs`
      - Added bounds checks to array helpers and a small abort diagnostic.

    - `crates/oats/src/codegen/mod.rs`
      - Replaced several panic-prone `.unwrap()` usages with guarded checks and safe fallbacks; preferred returning `None` from lowering where appropriate rather than panicking.

    - `crates/oats/src/bin/aot_run.rs`
      - Replaced a few `.unwrap()`/`.expect()` uses with safe checks when extracting call results or parameters.

Verification performed

    - Built `crates/runtime` (cargo build -p runtime) — success.
    - Built `crates/oats` (cargo build -p oats) — success.
    - Built the workspace (cargo build) — success.

Next actionable items (pick one)

    1. Finish removing remaining `.unwrap()` / `.expect()` by converting lowering APIs to `Result<..., Diagnostic>` and adding a top-level diagnostic emitter (recommended).
    2. Add unit/integration tests that verify the runtime aborts on out-of-bounds array accesses (runtime tests and an integration test that compiles example code and checks for abort behavior).
    3. Prototype a simple cycle collector that runs when allocation fails (or periodically) to mitigate refcount cycles.
    4. Harden CI: pin dependencies and add `cargo audit`/`cargo deny` in a GitHub Actions workflow.

If you'd like, I can start with item 1 (structured error propagation) or item 2 (tests for runtime bounds checks). Tell me which and I'll implement it next.

2. Medium-Priority: Feature Implementation & Correctness

These tasks are important for improving the language's capabilities and ensuring correct behavior.

    Implement Dot-Member Access and Class Lowering:

        Task: Implement lowering for obj.field access and full class support, including constructors and methods.

        Justification: This is a fundamental feature for object-oriented programming and is a prerequisite for writing more complex applications.

    Expand the Type System:

        Task: Add support for unions, tuples, and a basic form of generics (e.g., via monomorphization or type erasure).

        Justification: A more powerful type system allows for writing safer and more expressive code.

    Improve for-of Loop Lowering:

        Task: The current implementation of for-of loops can be optimized. Explore more efficient ways to iterate over arrays and other iterable objects.

        Justification: This is a common language feature, and optimizing it will have a broad impact on performance.

3. Low-Priority: Long-Term & Ecosystem

These are large-scale features that will be important for the long-term success of the language but are less critical than the items listed above.

    Implement Closures and Advanced Function Features:

        Task: Add support for closures, arrow functions, and other advanced function features. This will require significant work on environment capturing.

        Justification: Closures are a powerful and expected feature in modern programming languages.

    Implement Async/Await:

        Task: Add support for asynchronous programming with async/await. This will require a runtime with an event loop and a state machine transformation for async functions.

        Justification: Essential for I/O-bound applications and concurrent programming.

    Develop a Standard Library:

        Task: Create a standard library with common data structures, utilities, and I/O functions.

        Justification: A rich standard library is crucial for making a language productive and useful for a wide range of tasks.


Quick Checklist (Status)

    [x] [Security] Runtime bounds checking (implemented in crates/runtime/src/lib.rs)

    [~] [Security] Eliminate panics in compiler (many .unwrap()/.expect() occurrences replaced with safe checks; work remaining)

    [ ] [Performance] Memory cycle detection

    [ ] [Performance] Compiler and runtime profiling/optimization

    [ ] [Security] Secure CI and build process

    [x] Basic for-of lowering implemented

    [x] RC helpers (rc_inc/rc_dec) implemented

    [ ] Full class lowering

    [ ] Type-system expansion (unions, generics)