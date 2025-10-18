# Implementation Plan: Enforcing Strict Static Typing in Oats

## Overview

This document outlines the specific code changes required to implement strict static typing in the Oats compiler, as described in `static.md`. The changes will enforce that all variable declarations, function return types, and object shapes must be explicitly typed, eliminating type inference.

## Current Implementation Analysis

### Variable Declarations (`crates/oatsc/src/codegen/stmt.rs`)

**Current Behavior:**

- Variables can be declared without type annotations: `let x = 10;`
- When `ident.type_ann` is `None`, the compiler infers type from `decl.init` using `infer_type()`
- Object literals without explicit types generate anonymous nominal types

**Key Locations:**

- Lines ~100-150: Type annotation parsing and mapping
- Line 189: `let init_inferred = crate::types::infer_type(None, Some(init));`
- Lines ~550-650: Special handling for object literals without `declared_nominal`

### Function Return Types

**Regular Functions (`crates/oatsc/src/types.rs`)**

- `check_function_strictness()` assumes `Number` return type when no annotation present

**Arrow Functions (`crates/oatsc/src/codegen/expr.rs`)**

- Lines ~5195-5206: If no `return_type`, calls `infer_return_type_from_arrow_body()`

## Required Changes

### 1. Require Explicit Types for All Variable Declarations

**File:** `crates/oatsc/src/codegen/stmt.rs`

**Change Location:** After type annotation processing (~line 130), before initializer lowering.

**Code Change:**

```rust
// After determining declared_mapped, declared_union, etc.

// NEW: Enforce strict typing - require explicit type annotation for variables with initializers
if ident.type_ann.is_none() && decl.init.is_some() {
    return Err(crate::diagnostics::Diagnostic::simple(
        "Variable declaration requires explicit type annotation"
    ));
}
```

**Rationale:** This prevents `let x = 10;` and forces `let x: number = 10;`.

### 2. Require Explicit Return Types for Functions

#### Regular Functions

**File:** `crates/oatsc/src/types.rs`

**Change Location:** `check_function_strictness()` function, return type handling.

**Code Change:**

```rust
let ret_type = if let Some(return_type) = &func_decl.return_type {
    if let Some(mapped) = map_ts_type(&return_type.type_ann) {
        mapped
    } else {
        return Err(anyhow::anyhow!("Function return type not supported"));
    }
} else {
    // REMOVED: Assume Number fallback
    return Err(anyhow::anyhow!("Function requires explicit return type annotation"));
};
```

#### Arrow Functions

**File:** `crates/oatsc/src/codegen/expr.rs`

**Change Location:** Arrow function return type determination (~lines 5195-5206).

**Code Change:**

```rust
let ret_type = if let Some(return_type) = &arrow.return_type {
    if let Some(mapped) = crate::types::map_ts_type(&return_type.type_ann) {
        mapped
    } else {
        return Err(Diagnostic::simple("Arrow return type not supported"));
    }
} else {
    // REMOVED: Inference fallback
    return Err(Diagnostic::simple("Arrow function requires explicit return type annotation"));
};
```

### 3. Disallow Anonymous Object Shapes

**File:** `crates/oatsc/src/codegen/stmt.rs`

**Change Location:** Object literal handling without `declared_nominal` (~line 550).

**Code Change:**

```rust
// Special case: object literal without explicit type annotation
if declared_nominal.is_none()
    && let deno_ast::swc::ast::Expr::Object(obj_lit) = &**init
{
    // REMOVED: Generate anonymous nominal type
    return Err(crate::diagnostics::Diagnostic::simple(
        "Object literals require explicit type annotation conforming to a named interface or type"
    ));
}
```

**Rationale:** Forces all objects to be typed as `let point: Point = { x: 10, y: 20 };` where `Point` is a defined interface.

## Implementation Order

1. **Variable declarations** - Core change, affects most code
2. **Function return types** - Regular functions first, then arrows
3. **Object shapes** - May require updating existing code that relies on inference

## Testing Strategy

- Update existing test cases to include explicit types
- Add new test cases that verify errors are thrown for missing annotations
- Run full test suite after each change to ensure no regressions

## Breaking Changes

This will be a **breaking change** for all existing Oats code that relies on type inference. All user code will need to be updated to include explicit type annotations.

## Migration Path

1. Implement changes with clear error messages
2. Provide examples of how to fix each error type
3. Consider a gradual migration flag if needed for compatibility

## Files to Modify

- `crates/oatsc/src/codegen/stmt.rs` - Variable declarations and object literals
- `crates/oatsc/src/types.rs` - Regular function return types
- `crates/oatsc/src/codegen/expr.rs` - Arrow function return types

## Validation

After implementation:

- Run `cargo test` to ensure no existing functionality breaks
- Test compilation of example programs with and without explicit types
- Verify error messages are clear and actionable
