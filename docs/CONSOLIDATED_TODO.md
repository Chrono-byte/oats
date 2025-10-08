```markdown
# Oats — Consolidated TODO & Roadmap

This file consolidates the actionable TODOs from the repository docs into a single, prioritized roadmap. It focuses on short, medium, and long-term tasks with clear next steps and safety guidelines for refactors.

## High-level goals

- Remove panic sites in the compiler by migrating lowering functions to return `Result<_, Diagnostic>` and centralize diagnostic emission.
- Modularize the large `crates/oats/src/codegen/mod.rs` into smaller modules (`expr.rs`, `stmt.rs`, `gen.rs`, `helpers.rs`) to make refactors safer.
- Harden runtime with bounds checks and error handling, and add tests + CI that ensure correctness across platforms (LLVM-18 required).

## Short-term (1–3 days)

1) Finish converting lowering helpers to `Result<_, Diagnostic>` (incremental)
   - Strategy: adapter-first. Add `lower_*_result` adapters that return `Result` and convert callers one-by-one using `.ok()` temporarily. Then progressively change callers to `?`.
   - Safety: make very small commits (one logical change per commit). Run `cargo build -p oats` after each commit.
   - Tests: Add targeted tests that ensure diagnostics are emitted instead of panics when lowering fails.

2) Member-write lowering + focused test (0.5–1 day)
   - Implement lowering for member stores (`obj.field = expr`), ensure RC semantics on stores, and add a test under `crates/oats/tests/` asserting correct IR (getelementptr + store + rc_inc/rc_dec usage).

3) Add CI job for LLVM-18 + `cargo test -p oats` (1 day)
   - Use `scripts/setup_env.sh` as a reference for setting `LLVM_SYS_181_PREFIX`.
   - Add steps to install `llvm-18` on Ubuntu runners and set env variables.

## Medium-term (weeks)

1) Modularize CodeGen (Batched)
   - Batch A (low risk): Create `crates/oats/src/codegen/expr.rs` and `stmt.rs`. Move small, self-contained expression and statement lowering helpers into them.
   - Batch B (medium): Move function-level emission (`gen_function_ir`, `emit_host_main`) into `gen.rs` and common utilities into `helpers.rs`.
   - Batch C (cleanup): Tidy visibility, imports, run `cargo fmt`, `cargo build -p oats`, and `cargo test -p oats`.
   - Safety rules: 1–3 small edits per commit; run builds after each commit.

2) Dot-member access & class lowering
   - Implement direct `obj.field` lowering with simple nominal struct layout, `getelementptr` offsets, and field load/store tests. Then implement `ClassDecl` lowering with constructor/emitted methods.

3) Expand Type System
   - Add unions/tuples support and plan for generics (monomorphization vs erasure). Update `types.rs`, `map_ts_type`, and coercion rules.

## Long-term (months)

- Closures with boxed environments and capture semantics.
- Memory management improvements (cycle detection or tracing GC).
- Async/await lowering and runtime scheduler.

## Security & Performance Checklist

- Runtime bounds checks: Done (see `crates/runtime/src/lib.rs`). Add integration tests verifying abort on OOB.
- Remove panics across compiler: In progress. Track remaining `.unwrap()` / `.expect()` calls and migrate.
- Add `cargo audit`/`cargo deny` to CI and pin critical deps where necessary.

## Next immediate actionable item (choose one)

1. Finish the Result-migration for a single lowering function family (e.g., `lower_expr`) end-to-end: adapter -> caller conversions -> tests.
2. Implement member-write lowering + test (recommended short win).
3. Add a GitHub Actions workflow to install LLVM-18 and run `cargo test -p oats`.

When you pick one I will implement it using the small-commit, build-verify discipline and run tests locally.

## Notes

- Keep the `CodeGen` struct in `crates/oats/src/codegen/mod.rs` and implement `impl<'a> CodeGen<'a>` methods in submodules to maintain a single coherent type across files.
- Use byte offsets for diagnostic spans (`diagnostics::report_error_span`).
- Before changing runtime helper names or signatures, update both `crates/oats/src/codegen/*` declarations and `crates/runtime/src/lib.rs` implementations.

``` 
