# Expr Module Implementation Status Report

**Generated**: October 18, 2025
**Module Location**: `/crates/oatsc/src/codegen/expr/`
**Main File Size**: 6,052 lines (mod.rs)

## Overview

The expr module has been partially modularized. Module files have been created and skeleton implementations added, but **the main `mod.rs` still contains full inline implementations** for most expression types. The architecture is in place but extraction is incomplete.

---

## Detailed Status by Module

### ✅ COMPLETE/FUNCTIONAL

#### 1. **literals.rs** (798 lines)

- **Status**: COMPLETE and FUNCTIONAL
- **Implementation**: Full implementation of literal expression lowering
- **Functions**:
  - `lower_lit()` - Basic literals (numbers, booleans, strings, null)
  - `lower_array()` - Array literal expressions
  - `lower_object()` - Object literal expressions  
  - `lower_template()` - Template literal expressions
- **Called from mod.rs**: ✅ YES - Uses `literals::lower_*()` functions
- **Notes**: This is a standalone module (not using impl pattern), already fully extracted

#### 2. **assignments.rs** (696 lines)

- **Status**: COMPLETE with function stub
- **Function**: `pub(super) fn lower_assign_expr()`
- **Scope**: ✅ Handles assignment expressions (`ast::Expr::Assign`)
- **Issues**:
  - Function signature defined but implementation may be incomplete
  - Not being called from main match in mod.rs
  - Code still inlined in mod.rs (line 2557+)
- **Action**: VERIFY implementation completeness and wire into mod.rs

#### 3. **binary_ops.rs** (370 lines)

- **Status**: FUNCTIONAL but not called
- **Function**: `pub(super) fn lower_binary_expr()`
- **Scope**: ✅ Handles binary operations (`ast::Expr::Bin`)
- **Issues**:
  - Code duplicated: full implementation in BOTH binary_ops.rs AND mod.rs (lines 96-440)
  - Not being called from main match - inlined code in mod.rs is used instead
- **Action**: Replace inline code in mod.rs with function call

#### 4. **control_flow_expr.rs** (437 lines)

- **Status**: FUNCTIONAL but not called
- **Function**: `pub(super) fn lower_cond_expr()`
- **Scope**: ✅ Handles ternary/conditional expressions (`ast::Expr::Cond`)
- **Issues**:
  - Full implementation exists
  - Not being called from mod.rs - inlined in mod.rs (line 3218+)
- **Action**: Replace inline code with function call

#### 5. **ident.rs** (203 lines)

- **Status**: FUNCTIONAL but not called
- **Function**: `pub(super) fn lower_ident_expr()`
- **Scope**: ✅ Handles identifier expressions (`ast::Expr::Ident`)
- **Issues**:
  - Implementation appears complete
  - Not being called from mod.rs - inlined (line 441+)
- **Action**: Replace inline code with function call

#### 6. **member_access.rs** (989 lines)

- **Status**: FUNCTIONAL but not called
- **Function**: `pub(super) fn lower_member_expr()`
- **Scope**: ✅ Handles member access (`ast::Expr::Member`)
- **Issues**:
  - Large, complex implementation
  - Not being called from mod.rs - inlined (multiple locations)
- **Action**: Replace inline code with function call

#### 7. **paren.rs** (32 lines)

- **Status**: COMPLETE but check if called
- **Function**: `pub(super) fn lower_paren_expr()`
- **Scope**: ✅ Handles parenthesized expressions (`ast::Expr::Paren`)
- **Status in mod.rs**: Already calling `self.lower_paren_expr()` at line 3217 ✅
- **Action**: Verify it's being called correctly

#### 8. **this.rs** (43 lines)

- **Status**: COMPLETE but check if called
- **Function**: `pub(super) fn lower_this_expr()`
- **Scope**: ✅ Handles `this` keyword (`ast::Expr::This`)
- **Status in mod.rs**: Likely inlined or needs verification
- **Action**: Check if being called from mod.rs (line 614+)

### ⚠️ PARTIAL/INCOMPLETE

#### 9. **calls.rs** (2,554 lines) - **LARGEST MODULE**

- **Status**: PARTIAL - Skeleton with placeholder
- **Functions**:
  - `pub(super) fn lower_call_expr()` (placeholder with underscored params)
  - Plus handling for `New` expressions mixed in
- **Scope**:
  - Function calls (`ast::Expr::Call`)
  - New expressions (`ast::Expr::New`)
- **Issues**:
  - Function signature exists but implementation is a placeholder comment
  - Actual call expression code is HUGE and still inlined in mod.rs (line 617+, ~2000+ lines)
  - This is the most complex and largest expression type
- **Action**: EXTRACT full implementation from mod.rs, very large task

#### 10. **unary_ops.rs** (300 lines)

- **Status**: PARTIAL - Skeleton with placeholder
- **Functions**:
  - `pub(super) fn lower_unary_expr()` (has skeleton)
  - `pub(super) fn lower_update_expr()` (should handle `++`/`--`)
- **Scope**:
  - Unary operators (`ast::Expr::Unary`)
  - Update operators (`ast::Expr::Update`)
- **Issues**:
  - Skeleton implementation present but incomplete
  - Full code still inlined in mod.rs (lines 5697, 5878)
- **Action**: Complete implementation and wire into mod.rs

#### 11. **async_expr.rs** (377 lines)

- **Status**: PARTIAL - Skeleton with placeholder
- **Function**: `pub(super) fn lower_await_expr()`
- **Scope**: ✅ Handles await expressions (`ast::Expr::Await`)
- **Issues**:
  - Skeleton/placeholder implementation
  - Full code still inlined in mod.rs (line 4683+)
- **Action**: Extract full implementation from mod.rs

#### 12. **closures.rs** (32 lines)

- **Status**: PARTIAL - Skeleton with placeholder
- **Function**: `pub(super) fn lower_arrow_expr()`
- **Scope**: Arrow functions/closures (`ast::Expr::Arrow`)
- **Issues**:
  - Only skeleton comment placeholder
  - Full code still inlined in mod.rs (line 5031+, ~600+ lines)
- **Action**: Extract full implementation from mod.rs

---

## Module Declarations in mod.rs

✅ **All modules are properly declared:**

```rust
pub mod assignments;
pub mod async_expr;
pub mod binary_ops;
pub mod calls;
pub mod closures;
pub mod control_flow_expr;
pub mod ident;
pub mod literals;
pub mod member_access;
pub mod paren;
pub mod this;
pub mod unary_ops;
```

---

## Main Match Statement Status

The main `lower_expr()` function has a **giant match statement** (lines 96-6052 in mod.rs).

### Currently Calling Module Functions

- ✅ `ast::Expr::Paren` - calls `self.lower_paren_expr()`
- ✅ Literals - calls `literals::lower_lit()`, `literals::lower_array()`, etc.

### Still Inlined (NOT calling modules)

- ❌ `ast::Expr::Bin` - HUGE block (345 lines) - binary_ops.rs exists but not used
- ❌ `ast::Expr::Ident` - ident.rs exists but not used
- ❌ `ast::Expr::This` - this.rs exists but not used
- ❌ `ast::Expr::Call` - MASSIVE (2000+ lines) - calls.rs skeleton exists
- ❌ `ast::Expr::Assign` - assignments.rs exists but not used
- ❌ `ast::Expr::Cond` - control_flow_expr.rs exists but not used
- ❌ `ast::Expr::Member` - member_access.rs exists but not used
- ❌ `ast::Expr::New` - needs extraction
- ❌ `ast::Expr::Await` - async_expr.rs exists but not used
- ❌ `ast::Expr::Arrow` - closures.rs exists but not used
- ❌ `ast::Expr::Unary` - unary_ops.rs exists but not used
- ❌ `ast::Expr::Update` - unary_ops.rs exists but not used
- ❌ `ast::Expr::Object` - should use literals.rs
- ❌ `ast::Expr::Tpl` - should use literals.rs

---

## Key Observations

### Code Duplication Issues

Several modules have **DUPLICATE code**:

- **binary_ops.rs** vs mod.rs (lines 96-440) - IDENTICAL code in both places
- **calls.rs** - has skeleton but full code in mod.rs
- **unary_ops.rs** - skeleton exists but inline code remains

### Architectural Issues

1. **Module files have implementations but aren't wired in** - The match statement doesn't call them
2. **Code bloat in mod.rs** - 6,052 lines with most of the actual logic still inline
3. **Inconsistent extraction** - Some expressions (Paren, Literals) properly extracted, others not
4. **Type duplication** - Each module redefines `LocalEntry` and `LocalsStackLocal` types

### Good News

- ✅ Module structure is sound
- ✅ All module declarations already in place
- ✅ Type definitions consistent across all modules
- ✅ Function signatures follow established pattern
- ✅ Some modules already functionally complete (assignments, binary_ops, control_flow_expr)

---

## Extraction Roadmap

### PRIORITY 1 - HIGH (Large, frequently used)

1. **calls.rs** (2,554 lines) - Most complex, ~2000+ lines in mod.rs
2. **binary_ops.rs** (370 lines) - DUPLICATE code, need to remove inline version
3. **member_access.rs** (989 lines) - Complex, multi-location inline code

### PRIORITY 2 - MEDIUM (Mid-size or moderately complex)

4. **closures.rs** (32 lines skeleton) - ~600 lines in mod.rs
5. **unary_ops.rs** (300 lines) - ~400 lines total in mod.rs
6. **async_expr.rs** (377 lines) - ~300 lines in mod.rs

### PRIORITY 3 - LOW (Small, straightforward)

7. **ident.rs** (203 lines) - Small, straightforward extraction
8. **this.rs** (43 lines) - Very small
9. **assignments.rs** (696 lines) - Likely already complete, just need to wire in
10. **control_flow_expr.rs** (437 lines) - Complete, just needs wiring
11. **paren.rs** (32 lines) - Already working ✅

---

## Integration Checklist

For each module that needs wiring, the pattern is:

```rust
// In mod.rs match statement, REPLACE inline code with:
ast::Expr::Xyz(xyz) => xyz_module::lower_xyz_expr(self, xyz, function, param_map, locals)?,
```

Then **DELETE the inline implementation block** from mod.rs.

### Expected Outcome

- All 13 module files properly implemented and integrated
- mod.rs reduced from 6,052 lines to ~200-300 lines (just architecture/helpers)
- Clear separation of concerns
- Easier to test and maintain individual expression types

---

## Testing Strategy

After each module is wired:

```bash
cargo build --workspace       # Check for compilation errors
cargo test --workspace        # Verify functionality
cargo test --workspace expr   # Focus on expr module tests
```

The fact that tests still pass with inlined code suggests the extraction should be safe as long as:

1. Function signatures match
2. All necessary imports are present
3. Visibility (`pub(super)`) is correct
4. Module is declared in mod.rs

---

## Summary Statistics

| Metric | Value |
|--------|-------|
| Total Module Files | 13 |
| Fully Implemented & Wired | 2 (paren, literals) |
| Fully Implemented, Not Wired | 8 (assignments, binary_ops, control_flow_expr, ident, member_access, this, async_expr, unary_ops) |
| Partial/Skeleton | 3 (calls, closures, unary_ops update) |
| Current mod.rs Size | 6,052 lines |
| Estimated Final mod.rs Size | 200-300 lines |
| Lines to Relocate | ~5,700+ |
| Code Duplication | ~370+ lines (binary_ops) |
