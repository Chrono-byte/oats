Oats: Prioritized Project Roadmap (Security & Performance Focus)

This document is a compact, actionable roadmap with the most critical security and performance items at the top. The checklist below is updated to reflect recent safety-focused changes.

1. High-Priority: Security & Performance Hardening

These tasks are critical for making the compiler and the code it generates more robust, secure, and performant.

    [Security] Harden Runtime with Bounds Checking and Error Handling

        Status: Done (basic)

        What I changed: added explicit bounds checks to array helpers in `crates/runtime/src/lib.rs` (array_get_f64, array_get_ptr, array_get_ptr_borrow, array_set_f64, array_set_ptr). On out-of-bounds access the runtime prints a short diagnostic to stderr and aborts, avoiding undefined memory reads/writes.

        Notes: The abort path is intentionally minimal and dependency-free. A future enhancement could return error codes or raise a runtime trap value visible to generated code.

    [Security/Correctness] Eliminate panics in compiler

        Status: In progress

        What I changed: replaced several high-risk `.unwrap()` / `.expect()` usages with guarded checks to avoid panics in these hot spots (notably in `crates/oats/src/codegen/mod.rs` and `crates/oats/src/bin/aot_run.rs`). These changes make the compiler more robust against malformed AST inputs or missing IR values.

        Remaining: There are additional `.unwrap()` / `.expect()` uses across the codebase (especially short-lived builder operations in `helpers.rs`) that should be either converted to structured `Result`-based returns or guarded similarly. A proper long-term fix is to convert lowering functions to `Result<_, Diagnostic>` and propagate errors to a single emission/diagnostic site.

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