# Project Roadmap (summary)

**Last Updated:** October 16, 2025

This document tracks short- and mid-term priorities and records recent progress
so the team and contributors can see what to work on next.

## Recent progress (highlights)

- **Implemented union return type inference for arrow functions**: Arrow functions with block bodies now properly infer union types when multiple different return types are present, rather than defaulting to `Number`.
- Stabilized `crates/oats/src/codegen/stmt.rs`: fixed malformed edits that
  caused build failures and ensured initializer lowering stores the lowered
  value into pre-allocated locals (with `rc_inc` for pointers).
- Fixed monomorphization for generics: call-site inferred param types are now
  used when creating specialization keys so `getFirstElement(numbers)` and
  `getFirstElement(strings)` get distinct specializations.
- Replaced incorrect `strlen`-based truthiness lowering with header-length loads
  for object/array/string truthiness checks.
- Fixed array-literal lowering and runtime ABI mismatch: pass pointer-to-
  pointer (alloca address) into `array_set_ptr` so runtime can reallocate and
  update caller pointer.
- Implemented several RC fixes: template-literals/number->string lowering now
  emits `rc_dec` for temporaries, binary `+` concat and `println` codepaths
  materialize and decrement temporaries as appropriate.
- Added an IR-level test for the generics example and found/fixed regressions
  discovered by end-to-end tests (including cycle-reclaim).

These changes brought the workspace to a green state for the critical examples
and tests exercised during this session.

## Short-term priorities (next 2–4 weeks)

### 1.1 RC Memory Management Audit

- **Audit all lowering sites that allocate heap temporaries**: template literals,
  string concatenation, `number_to_string`, `union_box_*`,
  `array`/`tuple`/object literal lowering, and `println`/printing helpers.
- **Emit missing `rc_dec` where temporaries are created and ownership is not
  transferred**.
- **Add focused tests (IR string-contains or insta snapshots) for each pattern
  to prevent regressions**.

### 1.2 Code Quality and Warnings

- Clean up any remaining compiler warnings (unused assignments) found in `stmt.rs` and other files.
- Run `cargo clippy --workspace` and fix critical lints.
- ~~Remove dead code warnings (unused RTA functions: `collect_function_asts`, `run_worklist`, `find_method_calls`, etc.).~~ ✅ **DONE**: Added `#[allow(dead_code)]` attributes to unused RTA functions.

### 1.3 Testing and Integration

- Run `./scripts/run_fuzzing.sh` for fuzz targets and `./scripts/run_all_proper_tests.sh` to compile all proper_tests examples and catch regressions.
- Implement performance benchmarks for codegen speed.
- Expand snapshot tests for edge cases.

## Mid-term priorities (1–3 months)

### 2.1 Developer Experience & Tooling

- Create a language server for IDE support.
- Add build scripts for easier setup.
- Generate API docs and usage guides.
- Improve error diagnostics with source spans and suggestions.
- Add debugging/logging features for memory tracking.

### 2.2 Fix Critical Bugs

- **Fix control flow error reporting**: Currently `continue` outside of loop is ignored; should emit diagnostic (see TODO in `stmt.rs`).

### 2.3 Hardening and optimization of ARC & cycle collector

- Investigate redundant RC elimination and escape analysis opportunities.
- Improve the cycle collector's coverage and add stress tests.

### 2.4 Monomorphization & type-inference hardening

- Add tests to ensure caching/specialization keys are robust across different
  call-site inference patterns and local variables.

### 2.5 Documentation and developer ergonomics

- Update `docs/DEVELOPMENT.md` and `docs/ARCHITECTURE.md` with the
  ownership/RC rules, object layout invariants, and the most important runtime
  ABI contracts (e.g., `array_set_ptr` signature, header layout).
- Add comprehensive documentation and examples.
- Create more examples and tutorials.

## Long-term priorities (3–6 months)

### 3.1 Implement Missing Features

- Extend generics support (e.g., constraints, multiple type parameters).
- Add full interface implementation checking.

### 3.2 Expand Language Support

- Add more TypeScript features (e.g., decorators, namespaces, modules).
- Support for concurrency primitives beyond async/await.
- Add support for generators (`function*` / `yield`).
- Add support for JSX/TSX.
- Implement declaration merging and module augmentation.
- Add support for advanced type-level features (conditional types, mapped types, etc.).

### 3.3 Performance Optimization

- Profile and optimize LLVM IR generation (e.g., reduce redundant allocations).
- Implement generational GC with nursery and mature object spaces.

### 3.4 Rapid Type Analysis (RTA) Implementation

#### Phase 1: Analysis - Building the Foundation (Weeks 1-5)

- **Milestone 1.2: Call Graph Construction and Worklist Algorithm (Weeks 4-5)**
  - Implement call graph data structure to represent method calls.
  - Implement iterative RTA worklist algorithm starting from main, processing method calls to find potential targets.

#### Phase 2: Optimization - Applying the Analysis (Weeks 6-11)

- **Milestone 2.1: Devirtualization of Method Calls (Weeks 6-9)**
  - Modify codegen to query RTA results for devirtualization of method calls.
  - Implement logic to emit direct static calls when only one possible method target.
  - Create micro-benchmarks to measure virtual vs direct call performance.

#### Phase 3: Validation and Refinement (Weeks 12-14)

- **Milestone 3.1: Integration Testing (Weeks 12-13)**
  - Apply RTA to existing test suite including deno_tests to ensure no regressions.
  - Create larger test application with classes, inheritance, methods to validate optimizer.
- **Milestone 3.2: Final Report and Documentation (Week 14)**
  - Perform final performance and code size measurements on benchmarks.
  - Write internal documentation for RTA module, design, integration, diagnostics.

### 3.5 Code Maintenance and Security

- Modularize large files (e.g., split `expr.rs` into submodules).
- Update dependencies (e.g., bump `inkwell` for newer LLVM versions).
- Audit for buffer overflows or unsafe operations.
- Harden recursion limits and add more safety checks.

### 3.6 Runtime Enhancements

- Add more built-in functions (e.g., advanced string/array ops).

### 3.7 Community and Ecosystem

- Add package management for dependencies.
- Integrate with existing TS tooling (e.g., via plugins).

## Release candidate and stabilization

- Once the rc_dec audit, snapshot tests, and lints are green, prepare a
  release-candidate branch that includes:
  - A summary of ownership/RC guarantees and codegen invariants
  - Updated tests and any new snapshots
  - Short smoke-test log showing proper_tests/examples pass

## Completed Features (Historical Record)

### Core Language Features ✅

- ~~Basic enum support implemented with type checking and code generation.~~ ✅ **DONE**
- ~~Generics implemented with monomorphization at call sites.~~ ✅ **DONE**
- ~~Interface and type alias support implemented.~~ ✅ **DONE**
- ~~Labeled statement support with break/continue resolution.~~ ✅ **DONE**
- ~~Union return type inference for arrow functions with multiple return types.~~ ✅ **DONE**

### Memory Management ✅

- ~~Automatic Reference Counting (ARC) with escape analysis and cycle collection.~~ ✅ **DONE**
- ~~Bacon's concurrent cycle collection algorithm as the default.~~ ✅ **DONE**
- ~~Inter-procedural escape analysis with closure capture detection.~~ ✅ **DONE**
- ~~RC protocol violations fixed in tuple initialization, variable declarations, and for-of loops.~~ ✅ **DONE**

### Testing and Quality ✅

- ~~Fuzzing targets for parser testing.~~ ✅ **DONE**
- ~~Test package names and configurations updated.~~ ✅ **DONE**
- ~~Basic RTA with class hierarchy analysis, instantiation tracking, and dead code elimination.~~ ✅ **DONE**

### Design Decisions (Intentionally Restricted/Rejected)

- ~~`const` declarations limited by design for AOT compilation requirements.~~ **INTENTIONALLY RESTRICTED**
- ~~`var` declarations rejected by design; only `let` (and `let mut`) supported.~~ **INTENTIONALLY REJECTED**
