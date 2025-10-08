# Oats Compiler Improvement Plan

This document outlines technical debt and improvement opportunities identified after completing the short-term feature roadmap (while/do-while loops, break/continue, unary operators).

## Current Status

**Completed Features:**
- ✅ While/do-while loops
- ✅ Break/continue statements  
- ✅ All unary operators (!, -, +, ~, ++, --)
- ✅ 51 passing tests
- ✅ Heap object system with reference counting

**Remaining Short-term:**
- ❌ Switch statements (1 of 6 remaining)

---

## Priority 1: Robustness and Error Handling

### 1.1 Eliminate Panics in Compiler

**Current State:** ~25 instances of `.unwrap()` and `.expect()` in code generation logic that can panic at runtime.

**Problem:** These panics provide poor error messages and crash the compiler instead of providing helpful diagnostics to users.

**Solution:** Follow the migration plan in `docs/migration_to_result.md` to incrementally refactor all lowering functions to return `Result<_, Diagnostic>`.

**Affected Files:**
- `crates/oats/src/codegen/mod.rs` (19 instances)
- `crates/oats/src/codegen/helpers.rs` (4 instances)
- `crates/oats/src/codegen/expr.rs` (1 instance)

**Examples Found:**
```rust
// Bad: Can panic with generic message
.expect("Failed to build implicit return")
.expect("alloca failed")
.unwrap()

// Good: Returns diagnostic with context
.map_err(|_| Diagnostic::simple("failed to allocate variable"))?
```

**Implementation Plan:**
1. Start with leaf functions in `helpers.rs`
2. Propagate `Result` types up through `expr.rs`
3. Complete by updating `mod.rs` statement lowering
4. Add span-aware diagnostics where possible

**Estimated Effort:** 2-3 hours
**Impact:** High - Better user experience, more maintainable code

---

## Priority 2: Code Quality and Maintainability

### 2.1 Consolidate Redundant Type Mapping Code

**Current State:** Two nearly identical functions exist:
- `map_ts_type_to_oats()` in `crates/oats/src/bin/aot_run.rs` (lines 15-32)
- `map_ts_type()` in `crates/oats/src/types.rs` (lines 62-95)

**Problem:** The aot_run version is missing support for `Promise<T>` types, causing inconsistency.

**Differences:**
| Feature | types.rs | aot_run.rs |
|---------|----------|------------|
| Promise<T> | ✅ Supported | ❌ Missing |
| Array<T> | ✅ Recursive | ✅ Recursive |
| Nominal types | ✅ Full | ✅ Full |
| Keywords | ✅ Full | ✅ Full |

**Solution:**
1. Delete `map_ts_type_to_oats()` from aot_run.rs
2. Replace all calls with `oats::types::map_ts_type()`
3. Ensure types.rs function is public (already is)

**Affected Locations:**
- `crates/oats/src/bin/aot_run.rs:311` - Single usage of `map_ts_type_to_oats()`

**Implementation:**
```rust
// Before (aot_run.rs line 311)
.and_then(|ann| map_ts_type_to_oats(&ann.type_ann))

// After
.and_then(|ann| oats::types::map_ts_type(&ann.type_ann))
```

**Estimated Effort:** 15 minutes
**Impact:** Medium - Eliminates duplication, fixes Promise support in aot_run

---

### 2.2 Formalize TODO Tracking

**Current State:** 5 TODO comments scattered in codebase:

1. **Arrow Function Closure Capture** (`codegen/expr.rs:1311`)
   - "TODO: Detect captured variables and implement closure support"
   - Context: Arrow functions currently only support non-capturing lambdas

2. **Arrow Function Type Inference** (`codegen/expr.rs:1346`)
   - "TODO: Implement type inference"
   - Context: Arrow functions require explicit type annotations

3. **Closure Struct Creation** (`codegen/expr.rs:1423`)
   - "TODO: Create proper closure struct when capturing is supported"
   - Context: Currently returns raw function pointer

4. **Labeled Break** (`codegen/mod.rs:377`)
   - "TODO: Handle labeled breaks if _break_stmt.label is Some"
   - Context: `break label;` syntax not implemented

5. **Labeled Continue** (`codegen/mod.rs:396`)
   - "TODO: Handle labeled continues if _continue_stmt.label is Some"
   - Context: `continue label;` syntax not implemented

**Solution:**
- Create GitHub Issues for each TODO
- Remove comments and reference issue numbers instead
- Add to formal project roadmap

**Estimated Effort:** 30 minutes
**Impact:** Low - Better project organization

---

## Priority 3: Future-Proofing

### 3.1 Labeled Break/Continue Support

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
1. **Data Structure:** Add label->LoopContext mapping
   - `loop_labels: RefCell<HashMap<String, LoopContext<'a>>>`

2. **Statement Lowering:** Check for `label` field
   - `_break_stmt.label.as_ref().map(|id| id.sym.to_string())`

3. **Block Entry:** Register labeled loops
   - Extract label from `ast::Stmt::Labeled` wrapper
   - Push to label map before loop body

4. **Block Exit:** Unregister labels
   - Pop from label map after loop completes

**Estimated Effort:** 1-2 hours
**Impact:** Medium - Completes loop control flow implementation

---

## Additional Observations

### Memory Management Health
✅ **Status: Good**
- Unified heap object system operational
- Reference counting working correctly
- 40+ tests verify RC correctness
- No memory corruption issues

**Known Limitation:** Cycle detection not implemented (acceptable for most TypeScript patterns)

### Test Coverage
✅ **Status: Excellent**
- 51 tests passing
- All major features covered
- Integration and unit tests
- Real-world example programs

### Documentation
✅ **Status: Good**
- Class implementation guide
- Heap object design document
- Async/await design notes
- Roadmap document
- Unary operators summary

**Gap:** Need contributor guide for new developers

---

## Recommended Implementation Order

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

---

## Success Metrics

After completing this improvement plan:
- ✅ Zero `.unwrap()` calls in codegen paths
- ✅ Zero duplicate functions
- ✅ Zero TODO comments (all tracked in issues)
- ✅ Full loop control flow support
- ✅ Better error messages for users
- ✅ More maintainable codebase

---

## Notes

This plan focuses on **technical debt reduction** rather than new features. It prepares the codebase for the medium-term roadmap items:
- Arrow functions with closure capture
- Object literals
- Module imports/exports
- Interface types
- Generics

By eliminating panics and consolidating code now, future features will be easier and safer to implement.
