# Development Guide

This is the concise development guide for contributors. It replaces the older,
more fragmented development docs and points to deeper references when needed.

## Setup

- Install Rust (rustup) and LLVM 18
- Source the environment helper: `source ./scripts/setup_env.sh`
- Build: `cargo build --workspace`
- Tests: `cargo test --workspace`

## Coding Standards

- All codegen functions should return `Result<T, Diagnostic>` and avoid panics
  in library code.
- Prefer explicit error propagation with `Diagnostic::simple`.

## Common Tasks

- Adding a runtime helper: implement in `crates/runtime/src/lib.rs` and declare
  in `crates/oats/src/codegen/mod.rs`.
- Adding codegen tests: use `crates/oats/tests/` helpers and insta snapshots
  where applicable.

## Quality Gates

- Build: `cargo build --workspace` (must pass)
- Tests: `cargo test --workspace` (must pass)
- Lint: `cargo clippy --workspace` (fix warnings where possible)

````
# Oats — Development Guide

This document provides practical, actionable guidance for contributors working
on the Oats compiler. It covers development workflows, coding standards, testing
strategies, and common patterns.

## Table of Contents

1. [Getting Started](#getting-started)
2. [Architecture Quick Reference](#architecture-quick-reference)
3. [Coding Standards](#coding-standards)
4. [Testing Strategy](#testing-strategy)
5. [Common Development Tasks](#common-development-tasks)
6. [Debugging Guide](#debugging-guide)
7. [Contributing Workflow](#contributing-workflow)

## Getting Started

### Prerequisites

**Required:**

- Rust toolchain (1.70+): Install via [rustup](https://rustup.rs/)
- LLVM 18: Development headers and libraries
- Basic understanding of compilers and LLVM IR

**Optional:**

- `llvm-config` for debugging
- `valgrind` for memory leak detection
- `perf` for performance profiling

### Environment Setup

```bash
# Clone the repository
git clone https://github.com/Chrono-byte/oats.git
cd oats

# Setup LLVM environment (required before building)
source ./scripts/setup_env.sh

# Build the project
cargo build --workspace

# Run tests
cargo test --workspace

# Try an example
cargo run -p oats --bin toasty -- examples/add.oats
./aot_out/add
````

### First-Time Build Tips

- **LLVM errors:** Ensure `LLVM_SYS_181_PREFIX` points to your LLVM 18
  installation
- **Link errors:** You may need to install `clang-18` and `lld-18`
- **Slow builds:** Use `cargo build -j4` to limit parallelism if
  memory-constrained

## Architecture Quick Reference

### Project Structure

```
oats/
├── crates/
│   ├── oats/          # Compiler (parser, typechecker, codegen)
│   │   ├── src/
│   │   │   ├── parser.rs       # TypeScript AST parsing
│   │   │   ├── types.rs        # Type system (OatsType)
│   │   │   ├── diagnostics.rs  # Error reporting
│   │   │   ├── codegen/        # LLVM IR generation
│   │   │   │   ├── mod.rs      # CodeGen struct, function declarations
│   │   │   │   ├── expr.rs     # Expression lowering
│   │   │   │   ├── stmt.rs     # Statement lowering
│   │   │   │   └── emit.rs     # Top-level emission
│   │   │   └── bin/
│   │   │       └── toasty.rs  # CLI driver
│   │   └── tests/              # Compiler tests
│   └── runtime/       # C-callable runtime library
│       ├── src/lib.rs          # RC, allocators, builtins
│       └── tests/              # Runtime tests
├── examples/          # Test programs
└── docs/             # Documentation
```

### Compilation Pipeline

```
TypeScript Source (.oats)
    ↓ [parser.rs - deno_ast]
Abstract Syntax Tree (AST)
    ↓ [types.rs - type inference/checking]
Typed AST
    ↓ [codegen/*.rs - inkwell]
LLVM IR (.ll)
    ↓ [LLVM optimization passes]
Object File (.o)
    ↓ [linker with runtime]
Native Executable
```

### Key Abstractions

**`OatsType` (types.rs):**

- Represents all type information
- Variants: Number, Boolean, String, Array, NominalStruct, Union, Weak, Promise,
  etc.
- Used for type checking and LLVM type mapping

**`CodeGen` (codegen/mod.rs):**

- Main IR generation context
- Holds LLVM context, module, builder
- Caches runtime function declarations
- Thread-local state for current function compilation

**`Diagnostic` (diagnostics.rs):**

- Rustc-style error reporting
- Includes span information for source location
- Used throughout for error propagation

## Coding Standards

### Error Handling

**CRITICAL: All codegen functions must return `Result<T, Diagnostic>`**

```rust
// ✅ CORRECT
pub fn lower_expr(...) -> Result<BasicValueEnum<'a>, Diagnostic> {
    let val = some_operation()
        .map_err(|_| Diagnostic::simple("Operation failed"))?;
    Ok(val)
}

// ❌ WRONG - Never use these in codegen
pub fn lower_expr(...) -> BasicValueEnum<'a> {
    let val = some_operation().unwrap();  // Panic!
    val
}
```

**Pattern for LLVM operations:**

```rust
let call_site = self.builder.build_call(fn_val, &args, "name")
    .map_err(|_| Diagnostic::simple("Failed to build call"))?;
```

**Diagnostic creation:**

```rust
// Simple message
Diagnostic::simple("Type mismatch")

// With source location
Diagnostic::simple_with_span("Type mismatch", expr.span.lo.0 as usize)

// Multi-line with notes
Diagnostic {
    message: "Cannot call non-function type".to_string(),
    span_start: Some(span_start),
    notes: vec!["Expected function type".to_string()],
}
```

### Memory Management Contracts

**Batch 6 (cleanup):** Remove remaining `.expect()`/`.unwrap()` calls **Batch 7
(tests & CI):** Add tests for diagnostic output

### Conversion Patterns

#### Caller Converted to Return Result:

```rust
let val = self.lower_expr_result(expr, function, param_map, locals)?;
```

#### Caller That Must Remain Option (emit & fallback):

```rust
match self.lower_expr_result(expr, function, param_map, locals) {
    Ok(v) => { /* use v */ }
    Err(d) => { 
        crate::diagnostics::emit_diagnostic(&d, Some(self.source)); 
        return None; 
    }
}
```

#### Replace `.expect("build_call failed")`:

```rust
let call_site = match self.builder.build_call(fn_val, args, "name") {
    Ok(cs) => cs,
    Err(_) => return Err(Diagnostic::simple("runtime call failed")),
};
```

### Testing Strategy

**Unit Tests:** Helper error cases and boundary conditions **Integration
Tests:** Run `toasty` on malformed examples, assert diagnostic output **CI
Steps:** Verify `cargo build --all`, `cargo test --all`, diagnostic smoke tests

### Timeline Estimate

- **Prep/adapters:** ~0.5 day
- **Batches 1-3:** ~1-2 days
- **Helpers & propagation:** ~2-3 days
- **Tests + CI:** ~0.5-1 day
- **Total:** ~4-7 days

---

## Constructor Parameter Properties

### Problem Description

TypeScript allows shorthand syntax where constructor parameters with
accessibility modifiers automatically create and initialize class fields:

```typescript
export class Person {
    constructor(public name: string) {}
    // Equivalent to:
    // name: string;
    // constructor(name: string) {
    //     this.name = name;
    // }
}
```

### Previous Issue

The compiler was correctly collecting parameter properties as fields, but
constructor codegen was NOT initializing them - it only stored parameters into
local variables without writing to object field memory.

**Symptoms:**

- Class constructed successfully
- Methods could access `this` pointer
- But field values were uninitialized (garbage or zero)
- Example: `person.printName()` would print nothing or crash

### Solution Implementation

Added auto-assignment logic in `gen_constructor_ir()` after parameter locals are
created:

```rust
// Auto-assign constructor parameters to matching fields
// (TypeScript shorthand: constructor(public name: string) creates and assigns a field)
for (field_idx, (field_name, _field_type)) in fields.iter().enumerate() {
    if let Some(param_idx) = param_names.iter().position(|pn| pn == field_name) {
        // This field matches a constructor parameter - auto-assign it
        let param_val = f.get_nth_param(param_idx as u32).expect("param missing");
        let field_offset = header_size + (field_idx as u64 * 8);
        
        // Calculate field address and store the value
        let field_ptr = self.builder.build_struct_gep(
            self.context.i8_type(),
            obj_ptr,
            field_offset as u32,
            "field_ptr"
        )?;
        
        let _ = self.builder.build_store(field_ptr_cast, param_val);
        
        // If pointer type, increment RC for the stored reference
        if param_val.get_type().is_pointer_type() {
            let rc_inc_fn = self.get_rc_inc();
            let _ = self.builder.build_call(rc_inc_fn, &[param_val.into()], "rc_inc_field");
        }
    }
}
```

### Memory Layout

For `class Person { constructor(public name: string) {} }`:

```
Offset 0:  i64 header (RC=1, flags)
Offset 8:  ptr name (field 0)
```

Constructor receives `name` parameter, stores it at offset 8, and increments its
RC.

### Testing

- `examples/test_class_simple.oats` - Prints "Alice" correctly
- `examples/comprehensive_test.oats` - Full integration test with classes
- `cargo test -p oats` - All tests passing

### Files Modified

- `crates/oats/src/codegen/mod.rs` - Added field initialization in
  `gen_constructor_ir()`

---

## Development Workflow

### Recommended Implementation Order

**Week 1 - Quick Wins:**

1. Consolidate type mapping (15 min)
2. Formalize TODO tracking (30 min)
3. Add labeled break/continue (2 hours)

**Week 2 - Robustness:** 4. Eliminate panics - helpers.rs (1 hour) 5. Eliminate
panics - expr.rs (1 hour) 6. Eliminate panics - mod.rs (2 hours)

**Week 3 - Verification:**\
7. Run full test suite 8. Add regression tests for error handling 9. Update
documentation

**Total Estimated Time:** 7-8 hours

### Risk Mitigation

**Small Patches:** Keep changes focused (1-3 files), build after each commit
**Revert Strategy:** If patch causes large compile churn, revert and split
smaller **Test Coverage:** Verify existing tests still pass after each change
**Documentation:** Update docs and examples for any user-facing changes

### Success Metrics

After completing improvement plan:

- ✅ Zero `.unwrap()` calls in codegen paths
- ✅ Zero duplicate functions
- ✅ Zero TODO comments (all tracked in issues)
- ✅ Full loop control flow support
- ✅ Better error messages for users
- ✅ More maintainable codebase

---

## Test Suite Improvements

I consolidated the test setup and added stronger integration tests to make the
suite easier to maintain.

- Shared utilities: `crates/oats/tests/common/mod.rs` now provides
  `gen_ir_for_source(src: &str) -> anyhow::Result<String>` which centralizes
  parser + CodeGen setup used by many tests.

- Snapshot testing: `insta` was added as a dev-dependency and a snapshot test
  was added at `crates/oats/tests/codegen_snapshot.rs`. Snapshots are stored in
  `crates/oats/tests/snapshots/`.

  - To create/update snapshots interactively:

    ```bash
    cargo insta test
    ```

  - To auto-accept snapshots (useful in CI or initial run):

    ```bash
    INSTA_UPDATE=auto cargo test -p oats --test codegen_snapshot
    ```

* End-to-end testing: `crates/oats/tests/end_to_end.rs` runs the `toasty`
* runner to compile `examples/add.oats` into a temporary directory, runs the
* produced binary, and asserts the numeric output. This test builds `runtime`
* and `toasty` as needed, and may take a few seconds on first run.

---

**Last Updated:** October 11, 2025\
**For PR guidelines, see:** `CONTRIBUTING.md` at repository root
