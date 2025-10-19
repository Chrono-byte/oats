# Expr.rs Modularization Plan

## Current Status

- **File**: `crates/oatsc/src/codegen/expr/mod.rs` (6,053 lines)
- **Directory structure**: `crates/oatsc/src/codegen/expr/` already exists with subdirectories for individual modules
- **Module files already created**: assignments.rs, async_expr.rs, binary_ops.rs, calls.rs, closures.rs, control_flow_expr.rs, ident.rs, literals.rs, member_access.rs, paren.rs, this.rs, unary_ops.rs

## Main Match Arms to Extract

### 1. **Binary Operations** (lines 96-440) ~345 lines

- **File**: `binary_ops.rs` (already has skeleton)
- **Function**: `pub(super) fn lower_binary_expr()`
- **Extracts**: `ast::Expr::Bin(bin) =>` with all logic
- **Status**: Code still in mod.rs, module file exists but empty

### 2. **Identifier Handling** (line 441+)

- **File**: `ident.rs`  
- **Function**: `pub(super) fn lower_ident_expr()`
- **Extracts**: `ast::Expr::Ident(id) =>`
- **Status**: Check what's actually in this arm

### 3. **This Keyword** (line 614)

- **File**: `this.rs`
- **Function**: `pub(super) fn lower_this_expr()`
- **Extracts**: `ast::Expr::This(this_expr) =>`
- **Status**: Likely short, verify location

### 4. **Function Calls** (line 617+) ~2000+ lines

- **File**: `calls.rs` (created with skeleton)
- **Function**: `pub(super) fn lower_call_expr()`
- **Extracts**: `ast::Expr::Call(call) =>` - LARGE block
- **Status**: CRITICAL - Most complex module

### 5. **Assignments** (line 2557+) ~500+ lines

- **File**: `assignments.rs` (exists)
- **Function**: `pub(super) fn lower_assign_expr()`
- **Extracts**: `ast::Expr::Assign(assign) =>`
- **Status**: Code still in mod.rs

### 6. **Paren Expressions** (line 3217)

- **File**: `paren.rs`
- **Function**: `pub(super) fn lower_paren_expr()`  
- **Extracts**: `ast::Expr::Paren(paren) =>`
- **Status**: Already calling `lower_paren_expr` - verify implemented

### 7. **Ternary/Conditional** (line 3218+) ~300+ lines

- **File**: `control_flow_expr.rs`
- **Function**: `pub(super) fn lower_cond_expr()`
- **Extracts**: `ast::Expr::Cond(cond) =>`
- **Status**: Code still in mod.rs

### 8. **Literals** (line 3668)

- **File**: `literals.rs`
- **Functions**: `lower_lit()`, `lower_array()`, `lower_object()`, `lower_template()`
- **Extracts**: `ast::Expr::Lit()`, `ast::Expr::Array()`, `ast::Expr::Object()`, `ast::Expr::Tpl()`
- **Status**: Already using `literals::lower_*` - verify complete

### 9. **Member Access** (lines 1635, 3672+) ~500+ lines

- **File**: `member_access.rs`
- **Function**: `pub(super) fn lower_member_expr()`
- **Extracts**: `ast::Expr::Member(member) =>`
- **Status**: Large code block still in mod.rs

### 10. **New Expressions** (line 4627)

- **File**: `calls.rs` (or separate)
- **Function**: Extract with Call handling
- **Extracts**: `ast::Expr::New(new_expr) =>`
- **Status**: Separate from regular Call, may belong in calls.rs

### 11. **Await Expressions** (line 4683+) ~300+ lines

- **File**: `async_expr.rs`
- **Function**: `pub(super) fn lower_await_expr()`
- **Extracts**: `ast::Expr::Await(await_expr) =>`
- **Status**: Code still in mod.rs

### 12. **Arrow Functions/Closures** (line 5031+) ~600+ lines

- **File**: `closures.rs`
- **Function**: `pub(super) fn lower_arrow_expr()`
- **Extracts**: `ast::Expr::Arrow(arrow) =>`
- **Status**: Code still in mod.rs

### 13. **Unary & Update Operations** (lines 5697, 5878)

- **File**: `unary_ops.rs`
- **Functions**: `pub(super) fn lower_unary_expr()`, `pub(super) fn lower_update_expr()`
- **Extracts**: `ast::Expr::Unary(unary)`, `ast::Expr::Update(update)`
- **Status**: Code still in mod.rs

## Extraction Process

### Step-by-step for each module

1. **Identify block boundaries** in mod.rs using line numbers
2. **Read entire match arm** into the target module file
3. **Add function signature** with `pub(super) fn lower_*_expr()`
4. **Parameters**: `&self, <expr_type>, function, param_map, locals`
5. **Return type**: `Result<BasicValueEnum<'a>, Diagnostic>`
6. **Add type definitions** (LocalEntry, LocalsStackLocal) at top of module
7. **Add imports** as needed
8. **Replace in mod.rs** with: `ast::Expr::XYZ(...) => module::lower_xyz_expr(...)?`
9. **Add module declaration** in mod.rs: `pub mod xyz;`

## Template Pattern

Each module should follow:

```rust
use crate::diagnostics::Diagnostic;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};
use deno_ast::swc::ast as ast;

// Re-export LocalEntry type for consistency
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(super) fn lower_xyz_expr(
        &self,
        expr_param: &deno_ast::swc::ast::XyzExpr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        // CODE HERE
    }
}
```

## Key Implementation Notes

- **Type Definitions**: Each module repeats LocalEntry and LocalsStackLocal - this is fine for now
- **Imports**: Each module needs deno_ast types, inkwell types, diagnostics, CodeGen
- **Visibility**: Use `pub(super)` so functions are visible within expr module but not outside
- **mod.rs declaration**: Add `pub mod xyz;` at top level before impl block
- **Module order in mod.rs**: Doesn't matter functionally, suggest alphabetical

## Testing & Validation

After each extraction:

```bash
cargo build --workspace
cargo test --workspace
```

## Files to Modify

1. `crates/oatsc/src/codegen/expr/mod.rs` - Main dispatcher
2. `crates/oatsc/src/codegen/expr/binary_ops.rs`
3. `crates/oatsc/src/codegen/expr/calls.rs`
4. `crates/oatsc/src/codegen/expr/assignments.rs`
5. `crates/oatsc/src/codegen/expr/control_flow_expr.rs`
6. `crates/oatsc/src/codegen/expr/closures.rs`
7. `crates/oatsc/src/codegen/expr/async_expr.rs`
8. `crates/oatsc/src/codegen/expr/unary_ops.rs`
9. `crates/oatsc/src/codegen/expr/member_access.rs`
10. (Verify) `crates/oatsc/src/codegen/expr/ident.rs`
11. (Verify) `crates/oatsc/src/codegen/expr/this.rs`
12. (Verify) `crates/oatsc/src/codegen/expr/paren.rs`
13. (Verify) `crates/oatsc/src/codegen/expr/literals.rs`

## Priority Order

1. **HIGH**: binary_ops, calls, assignments (large, commonly used)
2. **MEDIUM**: member_access, closures, unary_ops, control_flow_expr
3. **LOW**: async_expr, ident, this, paren, literals (verify already done or simple)
