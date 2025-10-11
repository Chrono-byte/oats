# Oats — AI Agent Guide

Oats is an experimental AOT compiler transforming TypeScript → native code via LLVM. This guide helps AI agents immediately contribute to the codebase with critical patterns, workflows, and conventions.

## Project Architecture

**Rust workspace:** `crates/oats` (compiler) + `crates/runtime` (C-callable runtime library).

**Compilation Pipeline:**
1. **Parse** → `deno_ast` parses TypeScript to AST (`parser.rs`)
2. **Type Check** → Map TS types to `OatsType` enum (`types.rs`)
3. **Codegen** → Lower AST to LLVM IR (`codegen/*.rs` using `inkwell`)
4. **Link** → Link object file with runtime to produce executable

**Key Modules:**
- `crates/oats/src/parser.rs` - Enforces semicolons, rejects `var` keyword, enforces source size limits
- `crates/oats/src/types.rs` - Type system (`OatsType` enum: Number, String, Array, Union, Weak, NominalStruct, etc.)
- `crates/oats/src/codegen/mod.rs` - `CodeGen` struct with LLVM context, caches runtime function declarations
- `crates/oats/src/codegen/emit.rs` - Top-level function/constructor lowering
- `crates/oats/src/codegen/expr.rs` - Expression lowering via `lower_expr()`
- `crates/oats/src/codegen/stmt.rs` - Statement lowering via `lower_stmt()`
- `crates/oats/src/diagnostics.rs` - Rustc-style error reporting with `Diagnostic` struct
- `crates/runtime/src/lib.rs` - RC helpers, allocators, string/array ops (with resource limits)

## Critical: Heap Object Memory Layout

**ALL heap objects have unified 64-bit header at offset 0:**
- Bits 0-31: Strong refcount (atomic u32)
- Bit 32: Static flag (1=immortal, don't touch RC)
- Bits 33-48: Weak refcount (u16)
- Bits 49-63: Type tag/flags

**Object Layouts (byte offsets):**
```
Static string:    [header+static][i64 len][data+NUL]  → codegen returns ptr to data (offset 16)
Heap string:      [header RC=1][i64 len][data+NUL]    → runtime returns ptr to data (offset 16)
Array:            [header][i64 len][elements...]      → returns base ptr (offset 0)
Class/Object:     [header][i64 meta_ptr][fields...]   → returns base ptr (offset 0)
                  ↑ offset 0  ↑ offset 8 (reserved!)  ↑ fields start at offset 16
```

**CRITICAL:** Offset +8 is the **meta-slot** (field map pointer). Never overwrite with fields. Fields start at offset 16.

**Pointer Rules:**
- Runtime RC helpers accept: base pointers (offset 0) OR string data pointers (offset 16)
- Runtime uses `get_object_base(p)` to canonicalize both types
- Always call `rc_inc` when storing pointers, `rc_dec` when releasing
- Static literals have static bit set → RC ops become no-ops

## Error Handling Pattern

**All codegen functions return `Result<T, Diagnostic>` - NO `.unwrap()` or `.expect()`:**
```rust
// CORRECT:
pub fn lower_expr(...) -> Result<BasicValueEnum<'a>, Diagnostic> {
    let val = some_operation().map_err(|_| 
        Diagnostic::simple("Operation failed"))?;
    Ok(val)
}

// Use Diagnostic::simple(msg) or Diagnostic::simple_with_span(msg, byte_offset)
// Propagate with `?` operator
```

## Development Workflows

**Setup LLVM 18 environment (REQUIRED before building):**
```bash
source ./scripts/setup_env.sh
```

**Build & test:**
```bash
cargo build --workspace
cargo test --workspace
cargo clippy --workspace
```

**Compile & run an .oats file:**
```bash
cargo run -p oats --bin aot_run -- examples/hello.oats
./hello
```

**Run all proper_tests examples:**
```bash
./scripts/run_all_proper_tests.sh
```

**Fuzz testing (security testing):**
```bash
# Quick 60-second test
./scripts/run_fuzzing.sh

# Continuous fuzzing (24 hours)
FUZZ_TIME=86400 ./scripts/run_fuzzing.sh
```

**Snapshot testing (uses `insta`):**
- Tests in `crates/oats/tests/codegen/` use `insta::assert_snapshot!(ir)`
- Review snapshots: `cargo insta review`
- Accept changes: `cargo insta accept`
- Always explain snapshot changes in PR

**Common test utilities:**
- `crates/oats/tests/common/mod.rs::gen_ir_for_source()` - Generate IR from source string
- Use `let _guard = oats::diagnostics::suppress();` to silence stderr in tests

## Adding Runtime Functions

**Two-step process (BOTH required):**

1. **Add to `crates/runtime/src/lib.rs`:**
```rust
#[unsafe(no_mangle)]
pub extern "C" fn my_new_helper(arg: *const c_void) -> i64 {
    // implementation
}
```

2. **Declare in `crates/oats/src/codegen/mod.rs` in `CodeGen` struct:**
```rust
pub struct CodeGen<'a> {
    // ...
    pub fn_my_new_helper: RefCell<Option<FunctionValue<'a>>>,
}

impl<'a> CodeGen<'a> {
    fn get_my_new_helper(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_my_new_helper.borrow() {
            return f;
        }
        let fn_type = self.i64_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("my_new_helper", fn_type, None);
        *self.fn_my_new_helper.borrow_mut() = Some(f);
        f
    }
}
```

## Reference Counting Rules

**When to call RC ops:**
- **Storing pointer in field/local:** Call `rc_inc` first
- **Overwriting pointer:** Call `rc_dec` on old value before overwrite
- **Function return:** Call `rc_dec` on locals before returning
- **Loop break/continue:** Call `rc_dec` on loop-scoped locals before jumping
- **Weak pointers:** Use `rc_weak_inc`/`rc_weak_dec`/`rc_weak_upgrade` instead

**LocalEntry tuple structure (used throughout codegen):**
```rust
type LocalEntry<'a> = (
    PointerValue<'a>,   // alloca ptr
    BasicTypeEnum<'a>,  // LLVM type
    bool,               // is_mutable (let vs const)
    bool,               // is_param
    bool,               // is_weak (Weak<T>)
    Option<String>,     // nominal type name (for classes)
);
```

## Module System

**Entry point:** `crates/oats/src/main.rs` handles transitive module loading
- Resolves relative imports (`./foo`, `../bar`) with extensions `.ts`, `.oats`
- Tries `index.ts`/`index.oats` for directories
- Canonicalizes paths to avoid duplicates/cycles
- Stores in `HashMap<String, ParsedModule>`

## Testing Checklist

Before committing changes:
- [ ] `cargo test --workspace` passes
- [ ] No new `.unwrap()`/`.expect()` in `crates/oats/src/` (use `Result<_, Diagnostic>`)
- [ ] If touching object layout or RC: add runtime test in `crates/runtime/tests/`
- [ ] If IR changes: update insta snapshots with explanation
- [ ] If adding runtime function: declared in both runtime AND CodeGen struct
- [ ] Run `cargo clippy` and fix warnings

## Runtime Diagnostics

```bash
# Enable runtime logging
export OATS_RUNTIME_LOG=1
export OATS_COLLECTOR_LOG=1  # for cycle collector logs
```

## Common Patterns

**Type mapping:**
```rust
// OatsType → LLVM type via CodeGen::map_type_to_llvm()
Number → f64
Boolean → i1 or i8 (context dependent)
String → i8* (pointer to data at offset 16)
Array(T) → i8* (pointer to base at offset 0)
NominalStruct(name) → i8* (pointer to base at offset 0)
Union → f64 (numeric-only) or i8* (mixed/pointer)
```

**Codegen structure:**
- `lower_stmts()` returns `bool` (has terminator)
- `lower_expr()` returns `Result<BasicValueEnum, Diagnostic>`
- `emit_rc_dec_for_locals(&locals_stack)` cleans up before return/break/continue

## Resources

- **Architecture:** `docs/ARCHITECTURE.md` - Object layouts, contracts
- **Roadmap:** `docs/ROADMAP.md` - Phases, work items
- **Development:** `docs/DEVELOPMENT.md` - Contributing guidelines