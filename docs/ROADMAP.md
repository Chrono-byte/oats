# Oats — Roadmap

This document provides a clear, actionable roadmap for the Oats ahead-of-time TypeScript-to-native compiler. It reflects the current state of the project and outlines future development phases.

## Current Status (October 2025)

### ✅ Implemented Features

**Core Language:**
- ✅ Type system: `number`, `boolean`, `string`, `void`, typed arrays
- ✅ Classes: constructors, fields, methods, inheritance, `this` binding
- ✅ Control flow: `if`/`else`, `for`, `while`, `do-while`, `for-of` loops
- ✅ Labeled break/continue statements
- ✅ Operators: Full binary and unary operator support (including bitwise, logical)
- ✅ Template literals and string operations
- ✅ Arrow functions with closure capture
- ✅ Type inference for local variables

**Memory Management:**
- ✅ Automatic reference counting (RC) for all heap objects
- ✅ Deterministic destruction and cleanup
- ✅ Weak reference support (`Weak<T>`)
- ✅ Proper RC handling in closures and nested structures
- ⚠️ Cycle detection: Design complete, implementation planned

**Async/Await (Phase 0 - MVP):**
- ✅ `Promise<T>` type support
- ✅ `async function` declarations
- ✅ `await` expression lowering
- ✅ Runtime helpers: `promise_resolve`, `promise_poll_into`
- ⏳ State machine transformation (partial)
- ⏳ Full cooperative executor (planned Phase 1)

**Tooling & Infrastructure:**
- ✅ Complete build pipeline (TypeScript → LLVM IR → native binary)
- ✅ Diagnostic system with source locations
- ✅ Snapshot testing with `insta`
- ✅ 39+ passing tests
- ✅ End-to-end integration tests

## Goal

Ship a reliable, well-tested AOT TypeScript-to-native compiler that provides deterministic memory semantics (RC) and supports modern language features including async/await.

## Principles

- **Small, reviewable changes** with comprehensive tests
- **Clear separation** between compiler (`crates/oats`) and runtime (`crates/runtime`)
- **Correctness first:** Prioritize diagnostics and reproducible behavior over feature velocity
- **Documentation:** Keep architecture docs synchronized with implementation

## Development Phases

### Phase 1: Stability & Testing (CURRENT - 80% Complete)

**Status:** Near completion, focusing on remaining edge cases and test coverage.

**Goals:**
- [x] Robust error handling (Result-based, no panics in codegen)
- [x] Comprehensive test suite (unit, integration, snapshot)
- [ ] CI/CD pipeline (GitHub Actions)
- [x] Runtime header and RC correctness tests
- [ ] Performance baselines and benchmarking

**Remaining Work:**
1. **CI Infrastructure** (High Priority - 1-2 days)
   - Add GitHub Actions workflow for `cargo build`, `cargo test`, `cargo clippy`
   - Automated snapshot review policy
   - Test coverage reporting

2. **Additional Runtime Tests** (Medium Priority - 1 day)
   - Edge cases for RC increment/decrement
   - Static bit behavior validation
   - Header corruption detection tests

3. **Documentation Completion** (Medium Priority - 1 day)
   - CONTRIBUTING.md with PR guidelines
   - Example gallery with explanations
   - Performance tuning guide

### Phase 2: Async/Await Completion (NEXT - 2-3 weeks)

**Status:** Foundation laid, core transformation work remaining.

**Goals:**
- [ ] Complete state machine transformation for `async fn`
- [ ] Implement cooperative single-threaded executor
- [ ] Add waker registration and task queue
- [ ] Support `Promise.resolve()`, `Promise.reject()`, `.then()`, `.catch()`
- [ ] Working examples: timers, simple I/O operations

**Work Items:**
1. **State Machine Generation** (Week 1)
   - Split function body at `await` points
   - Allocate state struct with local variables
   - Generate poll function with state switch
   - Handle captured variables and closures

2. **Runtime Executor** (Week 1-2)
   - Task queue implementation
   - Waker objects and registration
   - Cooperative scheduling
   - Integration with event loop (basic)

3. **Promise API** (Week 2)
   - `Promise.resolve()` / `Promise.reject()`
   - `.then()` / `.catch()` / `.finally()` methods
   - Error propagation through promise chains

4. **Testing & Examples** (Week 2-3)
   - Async function tests (multiple awaits, nesting)
   - Timer-based examples (`sleep_async`)
   - Error handling in async contexts
   - Integration with existing examples

See `docs/ASYNC_AWAIT.md`, `docs/ASYNC_PHASE1.md`, and `docs/ASYNC_PRIMITIVES.md` for detailed design.

### Phase 3: Language Features (4-6 weeks)

**Status:** Design phase, prioritization needed.

**Goals:**
- [ ] Module system (imports/exports)
- [ ] Object literals with type inference
- [ ] Interfaces and type aliases
- [ ] Union types (discriminated unions)
- [ ] Generics (functions, classes)
- [ ] Enum support

**Priority Order:**
1. **Module System** (Highest - enables code organization)
   - Relative imports (`./foo`, `../bar`)
   - Named exports and imports
   - Default exports
   - Circular dependency detection

2. **Object Literals** (High - common pattern)
   - Inline object creation
   - Type inference from literal
   - Property access lowering

3. **Interfaces** (High - type safety)
   - Interface declarations
   - Structural typing
   - Method signatures

4. **Union Types** (Medium - type system completeness)
   - Tagged unions / discriminated unions
   - Type narrowing with guards
   - Runtime type tag generation

5. **Generics** (Medium - code reuse)
   - Generic functions with type parameters
   - Generic classes
   - Monomorphization strategy

### Phase 4: Cycle Collection (2 weeks)

**Status:** Design complete (`docs/CCRC.md`), implementation planned.

**Goals:**
- [ ] Implement trial-deletion cycle collector
- [ ] Per-class field map metadata generation
- [ ] Root list maintenance during RC operations
- [ ] Configurable collection triggers and thresholds
- [ ] Cycle collector stress tests

**Implementation Steps:**
1. Emit `field_map` metadata in constructors
2. Populate meta-slot (offset +8) correctly
3. Implement root list tracking
4. Add trial-deletion algorithm
5. Test with complex cyclic structures

### Phase 5: Production Hardening (Ongoing)

**Goals:**
- [ ] Performance optimization (escape analysis, stack allocation)
- [ ] Better error messages and diagnostics
- [ ] Debugging support (DWARF info, source maps)
- [ ] Standard library expansion
- [ ] Documentation and tutorials
- [ ] Community engagement and feedback

## Acceptance Criteria & Quality Gates

**Before Merging Any PR:**
- ✅ `cargo test --workspace` passes
- ✅ `cargo clippy --workspace` has no warnings
- ✅ No new `.unwrap()` or `.expect()` in codegen paths
- ✅ Snapshot changes explained in PR description
- ✅ Memory layout changes include runtime tests

**Phase Completion Criteria:**
- All tests passing (including new phase-specific tests)
- Documentation updated
- Examples demonstrating new features
- Performance regression checks passed

## Current Work Items (Pick One to Implement)

## Current Work Items (Pick One to Implement)

**High Priority:**
1. **Add CI Workflow** (1-2 hours)
   - GitHub Actions for build, test, clippy
   - Snapshot validation
   - Linux-only initially, expand later

2. **Complete Async State Machine** (3-4 days)
   - Generate poll functions from async fn bodies
   - Implement state struct allocation
   - Add state switch generation for resume points

3. **Add Runtime Tests** (2-3 hours)
   - Header layout validation
   - RC inc/dec edge cases
   - Weak reference lifecycle tests

**Medium Priority:**
4. **Object Literals** (2-3 days)
   - Parse object literal expressions
   - Lower to heap allocation + field stores
   - Type inference from literal shape

5. **Module System Bootstrap** (3-5 days)
   - Relative import resolution
   - Named export/import lowering
   - Symbol table per-module

**Low Priority:**
6. **Performance Baseline** (1-2 days)
   - Add benchmark suite
   - Profile compilation time
   - Document optimization opportunities

## Timeline Estimates

| Phase | Duration | Dependencies | Risk |
|-------|----------|--------------|------|
| Phase 1 Completion | 1 week | None | Low |
| Phase 2 (Async) | 2-3 weeks | Phase 1 | Medium |
| Phase 3 (Language) | 4-6 weeks | Phase 2 | Medium |
| Phase 4 (Cycles) | 2 weeks | Phase 3 | Low |

**Total to Feature Completeness:** ~10-12 weeks

## Quick Start for Contributors

### New to the project?
```bash
# Setup
source ./scripts/setup_env.sh
cargo build --workspace
cargo test --workspace

# Run an example
cargo run -p oats --bin aot_run -- examples/add.oats
./aot_out/add
```

### Want to contribute?
1. Read `docs/ARCHITECTURE.md` for system overview
2. Read `docs/DEVELOPMENT.md` for contribution guidelines
3. Pick a task from "Current Work Items" above
4. Ask questions in issues or discussions

### Testing your changes:
```bash
cargo test -p oats                    # Run compiler tests
cargo test -p runtime                 # Run runtime tests
./scripts/run_all_proper_tests.sh    # Full integration test
cargo insta review                    # Review snapshot changes
```

## Where to Look in the Repository

**Compiler Core:**
- `crates/oats/src/parser.rs` - AST parsing with deno_ast
- `crates/oats/src/types.rs` - Type system (OatsType enum)
- `crates/oats/src/codegen/` - LLVM IR generation
  - `mod.rs` - CodeGen struct, runtime function declarations
  - `expr.rs` - Expression lowering
  - `stmt.rs` - Statement lowering
  - `emit.rs` - Function/constructor emission

**Runtime:**
- `crates/runtime/src/lib.rs` - RC helpers, allocators, string/array ops
- `crates/runtime/tests/` - Runtime unit tests

**Tests:**
- `crates/oats/tests/codegen.rs` - IR generation tests
- `crates/oats/tests/end_to_end.rs` - Full compilation tests
- `crates/oats/tests/snapshots/` - Insta snapshot files

**Examples:**
- `examples/*.oats` - Small test cases
- `examples/proper_tests/` - Comprehensive feature tests

## Decision Log

**Memory Management:** Chose RC over GC for determinism and predictability. Cycle collector added as safety net.

**Async Model:** State machine transformation with cooperative executor. Matches Rust/TypeScript mental models.

**Type System:** Structural typing with nominal classes. Union types for flexibility.

**Module System:** ES6-style imports/exports, resolved at compile time. No dynamic loading.

**IR Generation:** Direct LLVM lowering (no intermediate MIR). Simplifies pipeline, enough for current scale.

## Future Considerations (Beyond MVP)

- Multi-threaded executor for async (Phase 6+)
- JIT compilation mode for development
- Source-level debugging with LLDB/GDB
- Package manager integration (npm compatibility layer)
- WASM backend for browser targets
- Incremental compilation for large codebases

## Get Involved

- **Report Issues:** Found a bug? Open an issue with minimal reproduction
- **Request Features:** Have an idea? Start a discussion
- **Contribute Code:** Pick an item from "Current Work Items" or good first issues
- **Improve Docs:** Spot an error or gap? Submit a PR

---

**Last Updated:** October 10, 2025  
**Next Review:** End of Phase 1 Completion
