# Locality of Behaviour Review

**Date:** January 2025
**Based on:** [htmx.org/essays/locality-of-behaviour/](https://htmx.org/essays/locality-of-behaviour/)

## Executive Summary

This document reviews the Oats codebase through the lens of the **Locality of Behaviour (LoB)** principle: *"The behaviour of a unit of code should be as obvious as possible by looking only at that unit of code."*

**Overall Assessment:** The codebase demonstrates good separation of concerns but has several areas where behavior is spread across multiple files, making it harder to understand code units in isolation.

---

## The LoB Principle

> "The behaviour of a unit of code should be as obvious as possible by looking only at that unit of code."

This principle helps developers understand code by minimizing "spooky action at a distance" - where behavior is defined far from where it's used.

### Trade-offs

LoB often conflicts with:

- **DRY (Don't Repeat Yourself)**: Avoiding redundancy
- **SoC (Separation of Concerns)**: Splitting code into distinct sections

The key is finding the right balance. The further behavior gets from the code unit it affects, the more severe the LoB violation.

---

## Identified LoB Violations

### 1. Environment Variable Lookups Scattered Across Code

**Violation:** Environment variables like `OATS_SRC_FILE`, `OATS_OATSC_PATH`, `OATS_RUNTIME_PATH` are checked in multiple places, making it unclear where configuration comes from.

**Locations:**

- `crates/oatsc/src/builder.rs:74` - checks `OATS_SRC_FILE`
- `crates/toasty/src/compiler.rs:13` - checks `OATS_OATSC_PATH`
- `crates/toasty/src/commands.rs:326` - checks `OATS_SRC_FILE`
- `crates/toasty/src/config.rs:31-44` - checks multiple `OATS_*` variables

**Problem:** When reading `builder.rs`, you can't tell that `OATS_SRC_FILE` might be set elsewhere. The behavior depends on external state that's not obvious at the call site.

**Recommendation:**

- Centralize environment variable access in a single module (e.g., `crates/toasty/src/env.rs`)
- Use a configuration struct that's explicitly passed around
- Document all environment variables in one place

**Example Improvement:**

```rust
// Instead of:
let src_path = if args.len() > 1 {
    args[1].clone()
} else if let Some(ref opts) = options {
    opts.src_file.clone()
} else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
    p
} else {
    anyhow::bail!("...");
};

// Prefer:
let src_path = resolve_source_path(args, options, &env::load())?;
```

---

### 2. Runtime Function Names as Magic Strings

**Violation:** Runtime helper function names like `"rc_inc"`, `"rc_dec"`, `"array_alloc"` are hardcoded as string literals in multiple files.

**Locations:**

- `crates/oatsc/src/codegen/mod.rs:605` - `"array_alloc"`
- `crates/oatsc/src/codegen/mod.rs:618` - `"rc_inc"`
- `crates/oatsc/src/codegen/mod.rs:707` - `"rc_dec"`
- And many more throughout `codegen/` modules

**Problem:** When reading code that calls `self.get_rc_inc()`, you can't immediately see what function name will be generated. The connection between the codegen call and the runtime function is hidden.

**Recommendation:**

- Define runtime function names as constants in a single module
- Use these constants when declaring functions
- Make the connection explicit at the call site

**Example Improvement:**

```rust
// In crates/oatsc/src/runtime_functions.rs
pub mod names {
    pub const RC_INC: &str = "rc_inc";
    pub const RC_DEC: &str = "rc_dec";
    pub const ARRAY_ALLOC: &str = "array_alloc";
    // ... etc
}

// In codegen/mod.rs
fn get_rc_inc(&self) -> FunctionValue<'a> {
    if let Some(f) = *self.fn_rc_inc.borrow() {
        return f;
    }
    let fn_type = self.context.void_type()
        .fn_type(&[self.i8ptr_t.into()], false);
    let f = self.module.add_function(runtime_functions::names::RC_INC, fn_type, None);
    *self.fn_rc_inc.borrow_mut() = Some(f);
    f
}
```

**Benefit:** Now when you see `runtime_functions::names::RC_INC`, it's immediately clear what function name is being used, and you can easily find all usages.

---

### 3. Configuration Loading Far From Usage

**Violation:** Configuration is loaded in `crates/toasty/src/config.rs` but used throughout the codebase without making the dependency obvious.

**Locations:**

- `crates/toasty/src/config.rs:27` - `Config::load()` reads from file and env
- `crates/toasty/src/compiler.rs:13` - Uses `OATS_OATSC_PATH` directly instead of config
- `crates/toasty/src/commands.rs` - Uses config indirectly

**Problem:** When reading code that uses configuration, it's not obvious where the values come from or what the precedence order is (file vs env var).

**Recommendation:**

- Make configuration loading explicit at entry points
- Pass configuration structs explicitly rather than reading env vars directly
- Document configuration precedence in one place

**Example Improvement:**

```rust
// In main.rs or command handlers
let config = Config::load()?;  // Explicit: we're loading config here
let compiler_path = config.compiler_path
    .or_else(|| std::env::var("OATS_OATSC_PATH").ok())
    .unwrap_or_else(|| "oatsc".to_string());

// Pass config explicitly
invoke_oatsc(&options, &config)?;
```

---

### 4. External Function Resolution Logic Spread Across Files

**Violation:**** Logic for resolving external function signatures is split between `builder.rs` and `extern_resolution.rs`, with metadata parsing happening far from where it's used.

**Locations:**

- `crates/oatsc/src/builder.rs:268-293` - Reads metadata files and parses signatures
- `crates/oatsc/src/extern_resolution.rs:27-58` - Similar logic duplicated
- `crates/oatsc/src/extern_resolution.rs:25` - `parse_function_signature_from_metadata` function

**Problem:** When reading `builder.rs`, the external dependency resolution logic is inline and hard to understand. The connection to `extern_resolution.rs` is not obvious.

**Recommendation:**

- Move all external resolution logic to `extern_resolution.rs`
- Make `builder.rs` call a single function: `resolve_external_dependencies()`
- Keep metadata parsing close to where it's used

**Example Improvement:**

```rust
// In builder.rs - simple and obvious
let external_fns = extern_resolution::resolve_all(
    &context,
    &module,
    &extern_oats,
)?;

// All the complexity is in extern_resolution.rs where it belongs
```

---

### 5. CLI Flag Strings as Magic Constants

**Violation:** CLI flag names like `"--extern-oats"`, `"--out-name"` are hardcoded strings scattered across argument building code.

**Locations:**

- `crates/toasty/src/compiler.rs:47` - `"--extern-oats"`
- `crates/toasty/src/compiler.rs:32` - `"--out-name"`
- `crates/oatsc/src/main.rs` - Similar flag strings

**Problem:** Flag names are not obvious at the call site, and changes require searching for string literals.

**Recommendation:**

- Define CLI flags as constants in the CLI module
- Use these constants when building arguments

**Example Improvement:**

```rust
// In crates/toasty/src/cli.rs
pub mod flags {
    pub const EXTERN_OATS: &str = "--extern-oats";
    pub const OUT_NAME: &str = "--out-name";
    pub const LINKER: &str = "--linker";
    // ... etc
}

// In compiler.rs
args.push(cli::flags::EXTERN_OATS.to_string());
args.push(format!("{}={}", import_path, meta_path));
```

---

### 6. Diagnostic Emission Pattern Not Obvious

**Violation:** Diagnostic emission uses `diagnostics::emit_diagnostic()` but the behavior (where it goes, how it's formatted) is not obvious at call sites.

**Locations:**

- `crates/oatsc/src/builder.rs:93` - `diagnostics::emit_diagnostic(diag, Some(&source))`
- Throughout codegen modules

**Problem:** When you see `emit_diagnostic()`, you can't tell if it prints to stderr, writes to a file, or collects errors. The behavior is hidden.

**Recommendation:**

- Make diagnostic behavior explicit (e.g., `diagnostics::emit_to_stderr()`)
- Or pass a diagnostic handler explicitly
- Document the behavior in the function name or signature

**Example Improvement:**

```rust
// More explicit about behavior
diagnostics::emit_to_stderr(diag, Some(&source));

// Or even better, explicit handler
let diag_handler = diagnostics::StderrHandler::new();
diag_handler.emit(diag, Some(&source));
```

---

## Positive Examples (Good LoB)

### 1. CodeGen Structure

The `CodeGen` struct in `crates/oatsc/src/codegen/mod.rs` demonstrates good LoB:

- All codegen state is in one place
- Helper methods like `get_rc_inc()` are defined where they're used
- The connection between method calls and LLVM function declarations is clear

### 2. Type System

The type system in `crates/oatsc/src/types.rs`:

- Type definitions are centralized
- Type checking logic is co-located with type definitions
- The relationship between AST types and `OatsType` is clear

### 3. Module Organization

The workspace structure shows good separation:

- `oats_parser` - parsing logic isolated
- `oats_ast` - AST definitions shared
- `oatsc` - compiler logic
- `runtime` - runtime library

Each crate has a clear, local purpose.

---

## Recommendations Summary

### High Priority ✅ COMPLETED

1. ✅ **Centralize environment variable access** - Created `crates/toasty/src/env.rs`
2. ✅ **Define runtime function name constants** - Created `crates/oatsc/src/runtime_functions.rs`
3. ✅ **Define CLI flag constants** - Created `crates/toasty/src/cli_flags.rs`

### Medium Priority

4. **Consolidate external resolution** - `builder.rs` duplicates logic from `extern_resolution.rs`
   - Current: Logic is inlined in `builder.rs` with comment about lifetime issues
   - Recommendation: Investigate if lifetime issues can be resolved, or at least extract to a helper function
5. **Make diagnostic behavior explicit** - Use more descriptive function names or explicit handlers
6. **Document configuration precedence** - Add clear docs about file vs env var priority

### Low Priority

7. **Consider inline documentation** - Add brief comments explaining non-obvious behavior
8. **Group related constants** - Use modules to organize magic strings/numbers
9. **Magic numbers** - Constants like `1u64 << 32` (static header bit) appear in multiple places
   - Consider: `const STATIC_HEADER_BIT: u64 = 1u64 << 32;` in a constants module
10. **Module name constant** - `"oats_aot"` is hardcoded in `builder.rs`
    - Consider: `const MODULE_NAME: &str = "oats_aot";`

---

## Implementation Strategy

1. **Start with constants** - Easiest wins with low risk
   - Runtime function names
   - CLI flags
   - Environment variable names

2. **Refactor configuration** - Medium effort, high impact
   - Centralize env var access
   - Make config explicit at entry points

3. **Consolidate logic** - Higher effort, improves maintainability
   - Move external resolution to one place
   - Make diagnostic behavior explicit

---

## Implementation Status

### Completed Improvements

1. ✅ **Runtime Function Constants** (`crates/oatsc/src/runtime_functions.rs`)
   - Centralized all runtime function names
   - Updated `codegen/mod.rs` and `codegen/helpers.rs` to use constants
   - Updated `builder.rs` to use `STD_CONSOLE_LOG` constant

2. ✅ **CLI Flag Constants** (`crates/toasty/src/cli_flags.rs`)
   - Centralized all CLI flag names
   - Updated `compiler.rs` to use constants

3. ✅ **Environment Variable Module** (`crates/toasty/src/env.rs`)
   - Centralized environment variable names and accessors
   - Updated `compiler.rs` and `commands.rs` to use helper functions

### Additional Improvements Completed

4. ✅ **Magic Numbers Constants** (`crates/oatsc/src/constants.rs`)
   - Created constants module for `STATIC_HEADER_BIT`, `META_MAGIC`, `CLOSURE_TYPE_TAG`
   - Updated `codegen/mod.rs` to use `STATIC_HEADER_BIT` constant
   - Note: Runtime already has `HEADER_STATIC_BIT` in `runtime/src/header.rs` - could be unified

5. ✅ **Naming Pattern Constants** (`crates/oatsc/src/runtime_functions.rs::naming`)
   - Added constants for string literal prefix, monomorphization suffixes, constructor naming
   - Updated `codegen/mod.rs` to use `STRING_LITERAL_PREFIX`

6. ✅ **Module Name Constants** (`crates/oatsc/src/runtime_functions.rs::modules`)
   - Added `OATS_AOT` and `TEST_MODULE` constants
   - Updated `builder.rs` to use `OATS_AOT`

7. ✅ **Additional Function Name Constants**
   - Added `STR_CONCAT` and `OATS_MAIN` to runtime function names
   - Updated `builder.rs` and `codegen/mod.rs` to use these constants
   - Updated `codegen/helpers.rs` to use `STR_CONCAT` constant

### Remaining Opportunities

1. **External Resolution Duplication**
   - `builder.rs` lines 267-293 duplicate logic from `extern_resolution.rs`
   - Comment mentions lifetime issues, but should be investigated

2. **More String Literal Prefix Updates**
   - `codegen/expr/literals.rs` and `codegen/expr/ident.rs` still use hardcoded `"strlit."`
   - Should use `runtime_functions::naming::STRING_LITERAL_PREFIX`

3. **More Static Header Bit Updates**
   - `codegen/expr/literals.rs` and `codegen/expr/ident.rs` still use hardcoded `1u64 << 32`
   - Should use `constants::STATIC_HEADER_BIT`

4. **More Function Name Lookups**
   - `codegen/expr/literals.rs` and `codegen/expr/binary_ops.rs` still use hardcoded `"str_concat"`
   - Should use `runtime_functions::names::STR_CONCAT`

5. **Constructor Naming Patterns**
   - `codegen/emit/constructors.rs` uses hardcoded format strings like `format!("{}_ctor", class_name)`
   - Should use `runtime_functions::naming::CTOR_SUFFIX` constants

6. **Monomorphization Naming**
   - `codegen/mod.rs` uses hardcoded `format!("{}_mono", func_name)`
   - Should use `runtime_functions::naming::MONO_SUFFIX`

## Conclusion

The Oats codebase is well-structured overall, and we've made significant improvements to Locality of Behaviour:

- ✅ **Magic strings** are now constants defined in centralized modules
- ✅ **Configuration** is now accessed through explicit helper functions
- ⚠️ **Related logic** still has some duplication (external resolution) that could be improved

These improvements make the codebase easier to understand and maintain, especially for new contributors who need to understand behavior by reading code in isolation.

The trade-off with DRY and SoC is acceptable - a small amount of repetition or coupling is worth the improved clarity and maintainability that LoB provides.
