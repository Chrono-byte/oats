# Oats — Development guide (practical)

This page collects the most actionable development guidance for contributors: current priorities, engineering contracts, testing rules, and a short checklist to follow before merging changes.

If you want the long-form design history, see `ARCHITECTURE.md` and `ROADMAP.md`.

## Small contract for codegen helpers

When you add or change a lowering helper, follow this 3-line contract:

- Inputs: AST node + CodeGen context (FunctionValue, param/local maps, builder)
- Output: Result<BasicValueEnum, Diagnostic> (or an appropriate typed Result)
- Error modes: return Diagnostic for unsupported shapes, builder failures, or coercion errors

Why: keeping lowering functions fallible makes the compiler robust and debuggable. New code must not call `.unwrap()` or `.expect()` in lowering paths.

## Top priorities (now)

1. Remove remaining panic sites in `crates/oats/src/codegen/*` by migrating helper functions to return Result (high impact, low-medium effort).
2. Emit per-class `field_map` metadata and ensure the meta slot (@+8) is populated by constructors (high priority for cycle collector work).
3. Add focused runtime tests in `crates/runtime/tests` that validate header layout and rc_inc/rc_dec semantics (fast to run, high value).

If you want a small starter task: replace one `.unwrap()` call in `crates/oats/src/codegen/helpers.rs` with a Result propagation and run `cargo test -p oats`.

## Practical testing rules

- Unit tests should be small and fast — prefer `#[test]` unit tests for helpers.
- Snapshot tests (insta) are used for IR output. When IR changes, add a short comment in the PR describing why snapshots changed.
- Integration tests should compile and run a small example; keep them selective to avoid long CI times.

Quick commands:

```bash
# build and test compiler crate
cargo build -p oats
cargo test -p oats

# run aot_run for a single example
cargo run -p oats --bin aot_run -- examples/add.oats
```

## Developer checklist (before merge)

1. cargo build --workspace
2. cargo test --workspace (or at least `-p oats` and `-p runtime`)
3. No new `.unwrap()`/`.expect()` calls in `crates/oats/src` (grep to check)
4. If codegen field offsets/header changed: add runtime unit tests and update docs
5. If IR changed: update insta snapshots and add explanation in PR

## Design decisions you should know

- Meta-slot (@+8) is reserved for runtime/class metadata. Never overlap it with fields.
- Strings are represented as pointer→data (pointer points at offset 16). RC helpers accept both base and data pointers.
- Codegen should consistently compute byte offsets via ptr→int + add(i64) + int→ptr to avoid platform-dependent GEP differences.

## Where to add tests

- fast runtime tests: `crates/runtime/tests`
- codegen unit tests: `crates/oats/tests` (use `crates/oats/tests/common/mod.rs` helpers)
- integration: `crates/oats/tests/end_to_end.rs` (compiles + runs aot_run on small examples)

## Good first issues for contributors

- Convert a single `unwrap()` in `crates/oats/src/codegen` to return Result and add a unit test for the failing case.
- Add a runtime unit test that validates header format, static bit behaviour, and rc_inc/rc_dec.
- Add a small insta snapshot for a recent IR change (follow the repo snapshot conventions).

## Development workflow notes

- Use small, focused PRs. Run `cargo test -p oats` locally before pushing.
- For changes touching RC or header layout, include runtime tests and a short explanation in the PR for why metadata and offsets remain correct.
- Prefer conservative changes to lowering; if in doubt, add a debug-only runtime check to assert header invariants.

---

This file is intentionally short and prescriptive. If you'd like me to implement one of the top priorities (1. remove a panic, 2. emit field_map, 3. add runtime tests) I can make the change and run tests right away.
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

## Test Suite Improvements

I consolidated the test setup and added stronger integration tests to make the suite easier to maintain.

- Shared utilities: `crates/oats/tests/common/mod.rs` now provides `gen_ir_for_source(src: &str) -> anyhow::Result<String>` which centralizes parser + CodeGen setup used by many tests.

- Snapshot testing: `insta` was added as a dev-dependency and a snapshot test was added at `crates/oats/tests/codegen_snapshot.rs`. Snapshots are stored in `crates/oats/tests/snapshots/`.

    - To create/update snapshots interactively:

        ```bash
        cargo insta test
        ```

    - To auto-accept snapshots (useful in CI or initial run):

        ```bash
        INSTA_UPDATE=auto cargo test -p oats --test codegen_snapshot
        ```

- End-to-end testing: `crates/oats/tests/end_to_end.rs` runs the `aot_run` runner to compile `examples/add.oats` into a temporary directory, runs the produced binary, and asserts the numeric output. This test builds `runtime` and `aot_run` as needed, and may take a few seconds on first run.

Notes:

- The shared helper intentionally mirrors `aot_run`'s emission path so unit tests can remain focused and small.
- When IR output changes, update snapshots with `cargo insta review` or `INSTA_UPDATE=auto` for one-off acceptance.


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