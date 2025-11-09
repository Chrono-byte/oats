# Development Guide

## Current Status

Oats is an active AOT compiler for the Oats language with good language support. We have tests, snapshots, and fuzzing. Parser maintains ~90% syntax compatibility with TypeScript for migration purposes.

## Setup

- Get Rust (rustup) and LLVM 18
- Run `source ./scripts/setup_env.sh`
- Build: `cargo build --workspace`
- Tests: `cargo test --workspace`

## Coding Standards

- Codegen functions return `Result<T, Diagnostic>`, no panics.
- Use `Diagnostic::simple` for errors.

## Common Tasks

- Add runtime helper: in `crates/runtime/src/lib.rs`, declare in `crates/oats/src/codegen/mod.rs`
- Add codegen test: use `crates/oats/tests/` with insta snapshots.

## Quality Gates

- Build: `cargo build --workspace` (must pass)
- Tests: `cargo test --workspace` (must pass)
- Lint: `cargo clippy --workspace` (fix warnings)

## Architecture Quick Reference

### Project Structure

```md
├── crates/
│   ├── oats/          # Compiler (parser, types, codegen)
│   ├── runtime/       # Runtime library
│   ├── oats_ast/      # wip home built AST library
│   ├── oats_parser/   # wip home built parser
│   ├── toasty/        # Test runner
│   └── std/           # Standard library
├── examples/          # Test programs
└── docs/              # Docs
```

### Compilation Pipeline

Oats source → AST → Typed AST → LLVM IR → Object File → Executable

### Key Abstractions

- `OatsType`: Represents types
- `CodeGen`: IR generation context
- `Diagnostic`: Error reporting

## Error Handling

All codegen returns `Result<T, Diagnostic>`.

Example:

```rust
pub fn lower_expr(...) -> Result<BasicValueEnum<'a>, Diagnostic> {
    let val = some_operation()
        .map_err(|_| Diagnostic::simple("Failed"))?;
    Ok(val)
}
```

For LLVM calls:

```rust
let call_site = self.builder.build_call(fn_val, &args, "name")
    .map_err(|_| Diagnostic::simple("Call failed"))?;
```

Diagnostics:

```rust
Diagnostic::simple("Error message")
```

## Testing

- Unit tests for helpers
- Integration tests with `toasty`
- Snapshots with `insta`

### Test Coverage

**Current Status:**

- 98+ test functions found across 37 test files
- Tests cover parsing, codegen, end-to-end scenarios
- Snapshot testing with `insta` for IR verification
- Fuzz testing for parser

**Test Organization:**

- `tests/parsing/` - Parser tests
- `tests/codegen/` - Code generation tests
- `tests/end_to_end/` - Integration tests
- `examples/proper_tests/` - Comprehensive test suite

**Recommendations:**

- Add more edge case tests for type checking
- Add tests for error handling paths
- Add property-based tests for codegen correctness
- Consider adding benchmarks for performance regression testing

## Code Quality

### Error Handling

**Standards:**

- All codegen functions return `Result<T, Diagnostic>`, no panics
- Use `Diagnostic::simple` for errors
- `.unwrap()`/`.expect()` are forbidden within `crates/oats/src` (enforced by ongoing audits)

**Current Issues:**

- Some functions in `builder.rs` use `anyhow::bail!` directly instead of returning `Diagnostic`
- Inconsistent error propagation in some codegen paths
- Some `.unwrap()` calls may exist (need audit)

**Recommendation:**

- Complete the audit to remove all `.unwrap()`/`.expect()` calls
- Standardize on `Diagnostic` for all compiler errors
- Consider using `thiserror` for structured error types

### Code Organization

**Strengths:**

- Clear module boundaries
- Good use of Rust idioms
- Appropriate use of `RefCell` and `Cell` for interior mutability

**Issues:**

- `builder.rs` is too large (1370+ lines) - should be split
- Some codegen files are quite large (e.g., `expr/mod.rs`)
- Some helper functions could be better organized

**Recommendation:**

- Split `builder.rs` into:
  - `module_resolution.rs` - Module discovery and loading
  - `compilation.rs` - Main compilation orchestration
  - `linking.rs` - Object file linking logic
- Consider splitting large codegen modules further

## Known Issues & TODOs

### ✅ Completed Implementations

#### 1. Call Graph Construction in RTA (`crates/oatsc/src/rta.rs:61`)

**Status:** ✅ **COMPLETED**

- Implemented `build_call_graph()` function that analyzes all functions and methods
- Enhanced `find_method_calls_in_stmt()` to handle more statement types (if, while, for, return, block)
- Enhanced `find_method_calls_in_expr()` to handle more expression types (binary, unary, member, assign, new)
- Removed `#[allow(dead_code)]` attributes from helper functions
- The call graph now maps caller function names to lists of callee function names

#### 2. VarDecl Handling in For Loop Init (`crates/oatsc/src/codegen/stmt/control_flow.rs:107`)

**Status:** ✅ **COMPLETED**

- Fixed the TODO by calling `lower_decl_stmt()` directly for `ForInit::VarDecl` cases
- Removed the error return and properly handle variable declarations in for loop initialization
- This enables code like `for (let i = 0; i < 10; i++)` to work correctly

### ❌ Cannot Implement (Requires AST Changes)

#### 3. Arrow Function Parameter Type Extraction (`crates/oatsc/src/codegen/expr/arrow_expr.rs:150`)

**Status:** ❌ **BLOCKED - Requires AST Changes**

- **Issue:** `ArrowExpr.params` is `Vec<Pat>`, not `Vec<Param>`
- **Problem:** `Pat` (Pattern) doesn't have a `ty` field, so type annotations cannot be extracted
- **Solution:** Requires updating `oats_ast` to change `ArrowExpr.params` from `Vec<Pat>` to `Vec<Param>`
- **Current Workaround:** Defaults all arrow parameters to `Number` type
- **Related:** See `crates/oatsc/src/builder.rs:734` for similar TODO

### 📋 Remaining TODOs by Category

#### High Priority - Can Implement

##### 1. External Function Signature from Metadata (`crates/oatsc/src/builder.rs:246`)

**Location:** `crates/oatsc/src/builder.rs:246`
**Feasibility:** ⚠️ **MEDIUM** - Requires understanding metadata format

- Currently defaults to `(no args, returns f64)` for all external functions
- Need to parse `.oats.meta` files to extract actual function signatures
- Would improve type safety and correctness

##### 2. Validate extern_oats Entries (`crates/oatsc/src/builder.rs:134`)

**Location:** `crates/oatsc/src/builder.rs:134`
**Feasibility:** ✅ **EASY** - Simple file validation

- Add file existence checks for `extern_oats` entries
- Return clear error messages if files don't exist
- Low risk, high value

##### 3. Type-Directed Lowering (`crates/oatsc/src/codegen/expr/calls.rs:586`)

**Location:** `crates/oatsc/src/codegen/expr/calls.rs:586`
**Feasibility:** ⚠️ **MEDIUM** - Requires type inference improvements

- Currently uses default types (f64) for call arguments
- Should use actual inferred types from the expression
- Would improve code generation quality

##### 4. TsType to FunctionSig Conversion (`crates/oatsc/src/parser.rs:290`)

**Location:** `crates/oatsc/src/parser.rs:290`
**Feasibility:** ⚠️ **MEDIUM** - Requires type system work

- Currently creates placeholder FunctionSig
- Need to properly convert TypeScript function types to Oats FunctionSig
- Important for type checking and code generation

#### Medium Priority - Feature Gaps

##### 5. Object Literal Types (`crates/oatsc/src/types.rs:415`)

**Status:** ✅ **COMPLETED** (January 2025)

- ✅ `TsTypeLit` added to AST
- ✅ Parser implementation complete (`ts_type_lit_parser`)
- ✅ Type mapping implemented in `map_ts_type()` and `map_ts_type_with_subst()`
- ✅ Supports property signatures, method signatures, and index signatures

##### 6. Package Compilation (`crates/oatsc/src/main.rs:107`)

**Location:** `crates/oatsc/src/main.rs:107`
**Feasibility:** ⚠️ **COMPLEX** - Multi-step feature

- Requires:
  1. Loading `Oats.toml` manifest files
  2. Resolving entry points
  3. Compiling with extern packages
  4. Generating `.oats.meta` files
  5. Writing object files
- This is a larger feature that should be planned separately

#### Low Priority - Infrastructure

##### 7. Pass Options Through Call Stack (`crates/oatsc/src/builder.rs:34`)

**Location:** `crates/oatsc/src/builder.rs:34`
**Feasibility:** ✅ **EASY** - Refactoring task

- Currently uses environment variables for options
- Should pass `CompileOptions` through function parameters
- Improves code quality and testability

##### 8. Import Statement Support (`crates/toasty/src/project.rs:416`, `crates/toasty/src/module_resolution.rs:147`)

**Status:** ✅ **AST AND PARSER COMPLETE** (January 2025)

- ✅ `ImportStmt` added to AST with full specifier support
- ✅ Parser implementation complete (`import_stmt_parser`)
- ⚠️ **Remaining:** Module resolution code in `toasty` needs to be updated to use AST import statements
- **Note:** AST and parser are ready; module resolution is a separate task

##### 9. Package Metadata (`crates/toasty/src/commands.rs:236`)

**Location:** `crates/toasty/src/commands.rs:236`
**Feasibility:** ⚠️ **MEDIUM** - Depends on package system

- Currently hardcodes exported symbols for known modules
- Should use actual package metadata from `.oats.meta` files
- Related to package compilation TODO above

### Recommendations

#### Immediate Next Steps (Easy Wins)

1. ✅ **Validate extern_oats entries** - Simple file validation
2. ✅ **Refactor options passing** - Improve code quality

#### Short Term (Medium Effort)

1. ⚠️ **External function signatures** - Requires metadata parsing
2. ⚠️ **Type-directed lowering** - Improve code generation
3. ⚠️ **TsType to FunctionSig** - Better type checking

#### Long Term (Complex Features)

1. ⚠️ **Package compilation** - Major feature, needs planning
2. ⚠️ **Module resolution** - AST/parser ready, needs implementation in toasty

### ✅ Recently Completed (January 2025)

#### Language Feature Implementation

**Status:** ✅ **ALL CORE LANGUAGE FEATURES COMPLETE**

The following features have been fully implemented in AST and parser:

1. ✅ **Type Aliases** - `type Name<T> = Type;` with type parameters, constraints, and defaults
2. ✅ **Interfaces** - Full support with properties, methods, index signatures, extends, and type parameters
3. ✅ **Enums** - `enum Name { Variant = value? }` with string and number values
4. ✅ **Namespaces** - `namespace Name { body }`
5. ✅ **Optional Chaining** - `obj?.prop`, `arr?.[0]` expressions
6. ✅ **Generators** - `function*` and `async function*` with `is_generator` field
7. ✅ **Yield Expressions** - `yield expr` and `yield* expr`
8. ✅ **Super Expressions** - `super`, `super.prop`, `super.method()`, `super(...)`
9. ✅ **Type Literal Types** - `{ prop: type, method(): type }` in type annotations
10. ✅ **Async Function Parsing** - Parser now correctly sets `is_async: true`
11. ✅ **Object Destructuring** - Full AST and parser support (was already in AST, parser confirmed)

**Impact:**

- All core TypeScript language features are now parseable
- AST is complete for all major language constructs
- Parser maintains ~95%+ syntax compatibility with TypeScript
- Ready for codegen implementation of these features

#### Blocked (Requires AST Changes)

- Arrow function parameter types (still uses `Vec<Pat>` instead of `Vec<Param>`)
