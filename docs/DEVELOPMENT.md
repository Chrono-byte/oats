# Oats Compiler Development Guide

This document consolidates all development-focused information including current status, technical debt, improvement plans, and contributor guidelines.

## Table of Contents

1. [Current Status](#current-status)
2. [Technical Debt & Improvement Plan](#technical-debt--improvement-plan)
3. [Error Handling Migration](#error-handling-migration)
4. [Constructor Parameter Properties](#constructor-parameter-properties)
5. [Development Workflow](#development-workflow)
6. [Contributing Guidelines](#contributing-guidelines)

---

## Current Status

### Completed Features ✅

**Core Language Support:**
- ✅ While/do-while loops with proper basic block structure
- ✅ Break/continue statements with label support
- ✅ All unary operators (!, -, +, ~, ++, --)  
- ✅ Classes with constructors, methods, field access
- ✅ For-loops, for-of loops, if/else statements
- ✅ Arrays and array literals with runtime support
- ✅ String operations and template literals
- ✅ 51 passing tests covering all features

**Memory Management:**
- ✅ Unified heap object system with reference counting
- ✅ Static string literals are immortal (static bit prevents RC modifications)
- ✅ Heap strings, arrays, and classes properly initialized with RC=1
- ✅ Zero memory corruption, all RC operations tested and verified
- ✅ 40+ tests passing, zero memory safety issues

**Infrastructure:**
- ✅ LLVM IR generation and AOT compilation
- ✅ Runtime integration with `crates/runtime`
- ✅ Comprehensive test suite with integration tests
- ✅ Diagnostic system with source span reporting

### Remaining Short-term Work ⚠️

**High Priority (Technical Debt):**
- ❌ Switch statements (1 of 6 short-term features remaining)
- ❌ Eliminate ~25 panic sites in compiler (`.unwrap()`, `.expect()`)
- ❌ Consolidate duplicate type mapping code

**Medium Priority (Quality):**
- ❌ Module resolution and multi-file compilation
- ❌ Object literals
- ❌ Arrow functions (non-capturing)
- ❌ Standard library basics (console.log, Math, Array methods)

---

## Technical Debt & Improvement Plan

### Priority 1: Robustness and Error Handling

#### Problem: Panics in Compiler

**Current State:** ~25 instances of `.unwrap()` and `.expect()` in code generation logic.

**Risk:** These panics provide poor error messages and crash the compiler instead of providing helpful diagnostics.

**Affected Files:**
- `crates/oats/src/codegen/mod.rs` (19 instances)
- `crates/oats/src/codegen/helpers.rs` (4 instances)
- `crates/oats/src/codegen/expr.rs` (1 instance)

**Examples:**
```rust
// Bad: Can panic with generic message
.expect("Failed to build implicit return")
.expect("alloca failed")
.unwrap()

// Good: Returns diagnostic with context
.map_err(|_| Diagnostic::simple("failed to allocate variable"))?
```

**Estimated Effort:** 2-3 hours
**Impact:** High - Better user experience, more maintainable code

#### Solution: Incremental Migration to Result Types

Follow the migration plan to convert lowering functions to return `Result<_, Diagnostic>`:

1. **Start with leaf functions** in `helpers.rs`
2. **Propagate Result types** up through `expr.rs` 
3. **Complete with statement lowering** in `mod.rs`
4. **Add span-aware diagnostics** where possible

### Priority 2: Code Quality and Maintainability

#### 2.1 Consolidate Redundant Type Mapping Code

**Current State:** Two nearly identical functions exist:
- `map_ts_type_to_oats()` in `crates/oats/src/bin/aot_run.rs` (lines 15-32)
- `map_ts_type()` in `crates/oats/src/types.rs` (lines 62-95)

**Problem:** The aot_run version is missing support for `Promise<T>` types.

**Differences:**
| Feature | types.rs | aot_run.rs |
|---------|----------|------------|
| Promise<T> | ✅ Supported | ❌ Missing |
| Array<T> | ✅ Recursive | ✅ Recursive |
| Nominal types | ✅ Full | ✅ Full |

**Solution:**
1. Delete `map_ts_type_to_oats()` from aot_run.rs
2. Replace usage with `oats::types::map_ts_type()`
3. Update import in aot_run.rs

**Implementation:**
```rust
// Before (aot_run.rs line 311)
.and_then(|ann| map_ts_type_to_oats(&ann.type_ann))

// After
.and_then(|ann| oats::types::map_ts_type(&ann.type_ann))
```

**Estimated Effort:** 15 minutes
**Impact:** Medium - Eliminates duplication, fixes Promise support

#### 2.2 Formalize TODO Tracking

**Current State:** 5 TODO comments scattered in codebase:

1. **Arrow Function Closure Capture** (`codegen/expr.rs:1311`)
   - "TODO: Detect captured variables and implement closure support"

2. **Arrow Function Type Inference** (`codegen/expr.rs:1346`)
   - "TODO: Implement type inference"

3. **Closure Struct Creation** (`codegen/expr.rs:1423`)
   - "TODO: Create proper closure struct when capturing is supported"

4. **Labeled Break** (`codegen/mod.rs:377`)
   - "TODO: Handle labeled breaks if _break_stmt.label is Some"

5. **Labeled Continue** (`codegen/mod.rs:396`)
   - "TODO: Handle labeled continues if _continue_stmt.label is Some"

**Solution:**
- Create GitHub Issues for each TODO
- Remove comments and reference issue numbers instead
- Add to formal project roadmap

### Priority 3: Future-Proofing

#### 3.1 Labeled Break/Continue Support

**Current State:** Loop context infrastructure exists, but labels are ignored.

**TypeScript Feature:**
```typescript
outer: for (let i = 0; i < 10; i++) {
    inner: for (let j = 0; j < 10; j++) {
        if (i * j > 50) break outer;  // Break outer loop
        if (j == 5) continue inner;    // Continue inner loop
    }
}
```

**Implementation Requirements:**
1. **Data Structure:** Add label→LoopContext mapping
2. **Statement Lowering:** Check for `label` field in break/continue
3. **Block Entry:** Register labeled loops in label map
4. **Block Exit:** Unregister labels after loop completion

**Estimated Effort:** 1-2 hours
**Impact:** Medium - Completes loop control flow implementation

---

## Error Handling Migration

### Migration Strategy: Lowering → Result<_, Diagnostic>

**Goal:** Replace panic-prone lowering with structured errors and centralized diagnostic emission.

**Success Criteria:**
- No new panics in lowering paths
- Diagnostics emitted for builder/IR failures instead of panics
- Tests verify diagnostic output for malformed inputs

### Contract Per Lowering Function

**Inputs:** AST node + codegen context (function, param_map, locals)
**Outputs:** `Result<BasicValueEnum<'a>, Diagnostic>` (or Result of appropriate type)
**Error Modes:** Unsupported AST shapes, builder failures, coercion errors

### Incremental Implementation Plan

#### Phase 1: Thin Adapters
Add small adapter methods that wrap current `Option`-returning helpers into `Result`:

```rust
// Add to CodeGen impl
fn lower_expr_result(
    &self,
    expr: &ast::Expr,
    function: FunctionValue<'a>,
    param_map: &HashMap<String, u32>,
    locals: &RefCell<HashMap<String, LocalVar<'a>>>,
) -> Result<BasicValueEnum<'a>, Diagnostic> {
    self.lower_expr(expr, function, param_map, locals)
        .ok_or_else(|| Diagnostic::simple("Failed to lower expression"))
}
```

#### Phase 2: Migrate in Small Batches

**Batch 1 (high impact):** Return lowering and `If` test lowering
- Convert `gen_function_ir` return statements
- Replace `lower_expr` calls with `lower_expr_result` and propagate errors

**Batch 2 (loops):** Loop test/index/length checks
- Convert loop condition and update expression lowering
- Handle builder call/load/cast failures

**Batch 3 (array/member):** Array and member access lowering
- Convert array literal lowering
- Convert member read/write operations

**Batch 4 (helpers):** Coercion and utility helpers
- Migrate `coerce_to_f64`, `coerce_to_i64`, `to_condition_i1`
- Convert `build_phi_merge` to return Result

#### Phase 3: Propagate and Cleanup

**Batch 5 (propagate):** Change higher-level functions to return Result
**Batch 6 (cleanup):** Remove remaining `.expect()`/`.unwrap()` calls
**Batch 7 (tests & CI):** Add tests for diagnostic output

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

**Unit Tests:** Helper error cases and boundary conditions
**Integration Tests:** Run aot_run on malformed examples, assert diagnostic output
**CI Steps:** Verify `cargo build --all`, `cargo test --all`, diagnostic smoke tests

### Timeline Estimate

- **Prep/adapters:** ~0.5 day
- **Batches 1-3:** ~1-2 days  
- **Helpers & propagation:** ~2-3 days
- **Tests + CI:** ~0.5-1 day
- **Total:** ~4-7 days

---

## Constructor Parameter Properties

### Problem Description

TypeScript allows shorthand syntax where constructor parameters with accessibility modifiers automatically create and initialize class fields:

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

The compiler was correctly collecting parameter properties as fields, but constructor codegen was NOT initializing them - it only stored parameters into local variables without writing to object field memory.

**Symptoms:**
- Class constructed successfully
- Methods could access `this` pointer  
- But field values were uninitialized (garbage or zero)
- Example: `person.printName()` would print nothing or crash

### Solution Implementation

Added auto-assignment logic in `gen_constructor_ir()` after parameter locals are created:

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

Constructor receives `name` parameter, stores it at offset 8, and increments its RC.

### Testing

- `examples/test_class_simple.oats` - Prints "Alice" correctly
- `examples/comprehensive_test.oats` - Full integration test with classes
- `cargo test -p oats` - All tests passing

### Files Modified

- `crates/oats/src/codegen/mod.rs` - Added field initialization in `gen_constructor_ir()`
- `.github/copilot-instructions.md` - Documented heap object system and constructor behavior

---

## Development Workflow

### Recommended Implementation Order

**Week 1 - Quick Wins:**
1. Consolidate type mapping (15 min)
2. Formalize TODO tracking (30 min)  
3. Add labeled break/continue (2 hours)

**Week 2 - Robustness:**
4. Eliminate panics - helpers.rs (1 hour)
5. Eliminate panics - expr.rs (1 hour)
6. Eliminate panics - mod.rs (2 hours)

**Week 3 - Verification:**  
7. Run full test suite
8. Add regression tests for error handling
9. Update documentation

**Total Estimated Time:** 7-8 hours

### Risk Mitigation

**Small Patches:** Keep changes focused (1-3 files), build after each commit
**Revert Strategy:** If patch causes large compile churn, revert and split smaller
**Test Coverage:** Verify existing tests still pass after each change
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

## Contributing Guidelines

### Setting Up Development Environment

1. **Install LLVM 18:**
   ```bash
   # Linux
   sudo apt-get install llvm-18 llvm-18-dev clang-18
   
   # macOS  
   brew install llvm@18
   ```

2. **Setup Environment:**
   ```bash
   cd /path/to/oats
   source ./scripts/setup_env.sh  # or setup_env.zsh for zsh
   ```

3. **Build and Test:**
   ```bash
   cargo build -p oats
   cargo test -p oats
   ./scripts/run_aot_tempdir.sh  # Test with examples/add.oats
   ```

### Development Commands

**Quick Build and Test:**
```bash
cd /home/ellie/Dev/oats 
OATS_OUT_DIR=./test_out cargo run -p oats --bin aot_run -- examples/YOURFILE.oats
./test_out/YOURFILE
```

**Run All Tests:**
```bash
cargo test --workspace
```

**Check for Panics/Unwraps:**
```bash
grep -r "\.unwrap()" crates/oats/src/
grep -r "\.expect(" crates/oats/src/
```

### Code Quality Standards

**Error Handling:**
- New code MUST use `Result<_, Diagnostic>` for fallible operations
- No new `.unwrap()` or `.expect()` calls in production code paths
- Always provide meaningful error messages with source context

**Testing:**
- Add unit tests for all new features
- Integration tests for end-to-end functionality  
- Update existing tests if behavior changes

**Documentation:**
- Update architecture docs for significant changes
- Add inline comments for complex algorithms
- Update examples/ directory with new language features

### Pull Request Guidelines

1. **Small, Focused Changes:** One logical change per PR
2. **Tests Included:** All new functionality must have tests
3. **Documentation:** Update relevant docs and examples
4. **Build Verification:** Ensure `cargo build` and `cargo test` pass
5. **No Regressions:** Existing tests must continue passing

### Getting Help

**Good First Issues:**
- Consolidate type mapping duplication
- Add labeled break/continue support
- Convert specific `.unwrap()` calls to proper error handling

**Harder Issues (Need Mentoring):**
- Module resolution and multi-file compilation
- Object literals implementation
- Arrow functions with closure capture

**Recommended Learning Path:**
1. Start with small fixes (typos, test improvements)
2. Implement missing unary operators or simple statements
3. Work on error handling migration
4. Tackle new language features

### Architecture Principles

This improvement plan focuses on **technical debt reduction** rather than new features. It prepares the codebase for medium-term roadmap items:

- Arrow functions with closure capture
- Object literals  
- Module imports/exports
- Interface types
- Generics

By eliminating panics and consolidating code now, future features will be easier and safer to implement.

---

## Additional Notes

### Memory Management Health ✅
- Unified heap object system operational
- Reference counting working correctly  
- 40+ tests verify RC correctness
- No memory corruption issues
- **Known Limitation:** Cycle detection not implemented (acceptable for most TypeScript patterns)

### Test Coverage ✅  
- 51 tests passing
- All major features covered
- Integration and unit tests
- Real-world example programs

### Documentation Status ✅
- Comprehensive architecture documentation
- Implementation guides for major features
- Development workflow documentation
- Contributor guidelines

**Gap:** Need more examples and tutorials for new contributors

This development guide provides the foundation for maintaining and extending the Oats compiler while ensuring code quality and contributor productivity.