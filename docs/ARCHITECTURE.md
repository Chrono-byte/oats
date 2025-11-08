# Architecture & Language Design Reference

**Last Updated:** October 18, 2025

This reference summarizes the current compiler architecture, runtime object model, language design, and the non-negotiable contracts between codegen and the runtime. It is intended to be the authoritative overview for contributors touching core systems.

## Current Implementation Status

**October 18, 2025:** Oats implements a substantial language surface with the following key features:

- **Language**: Classes, async/await, generics, unions, interfaces, enums, destructuring, template literals
- **Types**: Full primitive set (integers, floats, char), tuples, promises, weak/unowned references
- **Memory**: ARC with cycle collection, escape analysis for RC elimination
- **Runtime**: C-callable with helpers for strings, arrays, RC operations
- **Parser**: ~90.43% syntax compatibility with TypeScript (for migration purposes)

## Overview

Oats is an ahead-of-time compiler that compiles the Oats language to native executables via LLVM. It targets predictable performance, a small safe runtime, and a compact language surface suitable for systems programming. The language surface and type system use syntax similar to TypeScript for familiarity; the memory model and ownership semantics are inspired by Swift's ARC (Automatic Reference Counting) adapted for a deterministic, low-level runtime.

## Design Goals

- Predictable performance and memory layout for interop with native code.
- Clear, small semantics that map well to LLVM and a C-callable runtime.
- Safe, deterministic reference-counted memory management with weak refs and cycle collector support.
- Developer ergonomics with familiar syntax, but trimmed for AOT compilation.

## Source Model & Syntax

- Syntax is similar to TypeScript but tailored for AOT compilation:
  - `let` / `let mut`, `function`, `class`, `import`/`export`. Semicolons are enforced; `var` is rejected. `const` is supported as an immutable binding (preferred for values that should not change). The language also includes an explicit `let mut` form to declare mutable bindings when needed.
  - Source size limits apply (enforced in parser).
- No dynamic eval. Modules are statically resolved at compile time.

## Types

- Core type set (OatsType):
  - Number (f64)
  - Boolean
  - String
  - Array`T`
  - NominalStruct / Class
  - Union (either numeric-only or pointer)
  - Weak`T` (zeroing weak references)
  - Nominal typing metadata retained for runtime semantics
  - Optional/nullable values are represented using union syntax (for example `T | null` / `T | undefined`) and are lowered according to the target `OatsType` semantics.
- Mapping to LLVM:
  - Number → `f64`
  - Boolean → `i1` or `i8` as needed
  - String / Array / Struct → `i8*` (pointer to data or base)

## Workspace Overview

- `crates/oats` – Ahead-of-time compiler (parser, type checker, codegen).
  - `parser.rs` – Oats language parsing via `deno_ast`.
  - `types.rs` – `OatsType` representation and type inference helpers.
  - `codegen/` – LLVM IR lowering (`emit.rs`, `expr.rs`, `stmt.rs`, `helpers.rs`, `const_eval.rs`, `escape.rs`).
  - `builder.rs` – Entry point that wires parsing, type checking, and codegen.
- `crates/runtime` – C-callable runtime linked into user binaries (ARC helpers, allocators, string/array primitives, logging, and the cycle collector).
- `examples/` – Representative `.oats` programs that compile under the current language surface.
- `docs/` – Design references (this folder).

## Compilation Pipeline

```text
Oats source (.oats)
  ↓  parser.rs (deno_ast)
Abstract Syntax Tree (AST)
  ↓  types.rs (type checking / OatsType mapping)
Typed AST
  ↓  codegen/* (inkwell) + escape.rs + const_eval.rs
LLVM IR module (.ll)
  ↓  LLVM (opt/bc emission handled by inkwell)
Object file (.o)
  ↓  Link with crates/runtime
Native executable
```

### Key Codegen Responsibilities

- `CodeGen` owns the LLVM context, module, and builder plus caches for runtime helper declarations (see `codegen/mod.rs`).
- `emit.rs` lowers top-level items (functions, constructors, globals).
- `expr.rs` lowers expressions and enforces `Result<T, Diagnostic>` error propagation.
- `stmt.rs` handles block/loop lowering and scope-driven RC cleanup.
- `const_eval.rs` materializes compile-time constants into LLVM globals and reuses them via stable interning keys.
- `escape.rs` computes intra-procedural escape analysis so locals that do not escape can skip redundant ARC operations.

### Async and Generics

- Async functions are lowered into poll-state machines with resume blocks and slot maps stored on `CodeGen`. Escape analysis is async-aware by tracking locals live across `await` points.
- Nested generic functions are monomorphized on demand; `CodeGen` caches specialization keys to avoid regenerating IR for the same instantiation.

## Heap Object Layout (Canonical Contract)

All heap-allocated objects share a 64-bit header at offset 0. Violating this layout risks memory corruption.

- Offset 0: 64-bit header (atomic)
  - Bits 0–31: strong reference count (`u32`)
  - Bit 32: `STATIC` flag (1 = immortal literal; ARC ops are no-ops)
  - Bits 33–48: weak reference count (`u16`)
  - Bits 49–63: reserved for type tags / flags
- Offset 8: meta-slot (pointer to metadata / vtable). Never store user data here.
- Offset 16+: user fields or element data.

### Object Shapes

- **String**: `[header][i64 length][utf8 bytes ... NUL]` – Codegen and runtime operate on a pointer to the data at offset +16.
- **Array**: `[header][i64 length][element data ...]` – Runtime returns the base pointer (offset 0).
- **Class/Object**: `[header][i64 meta_ptr][field data ...]` – Base pointer is used by codegen; fields begin at offset +16.

## Reference Counting Protocol

- All codegen paths must call `rc_inc` before storing a strong pointer into a local, struct field, or array slot.
- Always call `rc_dec` on the old value before overwriting a pointer slot and when leaving a scope (`emit_rc_dec_for_locals` handles block cleanup).
- Weak references use `rc_weak_inc`, `rc_weak_dec`, and `rc_weak_upgrade`; they do not affect the strong count and are zeroed when the referent drops.
- Escape analysis may elide `rc_inc`/`rc_dec` pairs for locals that provably do not escape. Field stores and returns are conservative and still emit RC ops.
- Static literals have the `STATIC` bit set; RC helpers detect this and skip mutations.

## Values & Heap Object Layout

All heap objects share a 64-bit header at offset 0. This header layout is the canonical contract between codegen and runtime. The runtime implements deterministic ARC-style ownership similar to Swift's ARC: references are strong by default, weak references are non-owning and will be zeroed when the referent is deallocated, and a small set of explicit ownership annotations control reference behaviour in generated code.

Header (64 bits):

- Bits 0–31: strong refcount (atomic u32)
- Bit 32: static flag (1 = immortal; RC ops are no-ops)
- Bits 33–48: weak refcount (u16)
- Bits 49–63: type tag / flags

Object layouts (byte offsets)

- Static string:
  - \[header+static\]\[i64 len\]\[data + NUL\]
  - Codegen returns pointer to string data (offset 16)
- Heap string:
  - \[header RC=1\]\[i64 len\]\[data + NUL\]
  - Runtime returns pointer to string data (offset 16)
- Array:
  - \[header\]\[i64 len\]\[elements...\]
  - Returns base pointer (offset 0)
- Class / Object:
  - \[header\]\[i64 meta_ptr\]\[fields...\]
  - Returns base pointer (offset 0)
  - Offset +8 is the meta-slot (reserved for field map pointer) — never overwrite with fields. Fields start at offset 16.

Pointer rules and ARC semantics:

- Runtime RC helpers accept either base pointers (offset 0) or string data pointers (offset 16).
- Runtime uses `get_object_base(p)` to canonicalize pointers.
- Strong references: default for fields, locals and parameters. The compiler must emit `rc_inc` when storing a new strong pointer into a heap field/local and `rc_dec` when overwriting or releasing it.
- Weak references: declared with `weak`/`Weak<T>`. Weak references do not increment the strong count; the runtime tracks weak owners using the weak refcount and/or side tables. When an object is deallocated its weak references are automatically zeroed (set to `null`), matching Swift's zeroing weak semantics. Use `rc_weak_upgrade` to attempt to convert a weak reference to a strong one; the result is optional and must be checked.
- Unowned references: an `unowned` annotation expresses a non-owning, non-optional reference that is expected to outlive the referent. `unowned` references do not increment the strong count and are not zeroed; using an invalid `unowned` reference is undefined behaviour at the runtime level (implementations may trap). Use `unowned` only when lifetimes are guaranteed by program structure.
- Static literals have the static bit set; RC ops are no-ops for them.

## Reference Counting & Memory Management

- Mandatory ARC discipline (compiler responsibilities):
  - Strong is the default: storing a pointer in a field/local must emit `rc_inc` for the new value and `rc_dec` for the old value (if any) before overwrite.
  - Overwriting a pointer: `rc_dec` the old value before replacing it.
  - Returning from a function: release (`rc_dec`) any function-scoped strong locals before returning as required by the calling convention and ownership contract.
  - Breaking/continuing loops: release (`rc_dec`) loop-scoped strong locals before jumping out of scope.
  - Prefer `weak` for breaking cycles. Use `rc_weak_inc`, `rc_weak_dec`, and `rc_weak_upgrade` for weak operations; `rc_weak_upgrade` returns an optional strong reference.
- Runtime exposes helpers for allocation, string/array ops, RC ops, and a cycle collector. New runtime helpers require both runtime implementation and declaration in the CodeGen struct.

## Runtime ABI Contracts

- All heap allocations must go through runtime helpers (strings, arrays, objects). They initialize headers with `RC=1` and return canonical pointers.
- `CodeGen` lazily declares runtime helpers (e.g., `get_rc_inc`, `get_union_box_ptr`). Every new runtime function needs both an implementation in `crates/runtime/src/lib.rs` and a getter in `codegen/mod.rs`.
- String/array helpers expect canonical pointers; use runtime canonicalization helpers if the pointer shape is ambiguous.
- Cycle collector relies on the meta-slot and field metadata emitted during class lowering; do not repurpose the slot.

## Compiler Pipeline

1. Parse: `deno_ast` → AST (parser enforces semicolons, rejects `var`, enforces size limits).
2. Type check: map Oats type annotations to `OatsType`.
3. Codegen:
   - `CodeGen` struct holds LLVM context, cached runtime function declarations, and helper types (`i8ptr_t`, `i64_t`, etc.).
   - Lowering is split:
     - `emit.rs` — top-level functions & constructors
     - `expr.rs` — `lower_expr() -> Result<BasicValueEnum, Diagnostic>`
     - `stmt.rs` — `lower_stmt() -> Result<bool, Diagnostic>` (returns whether BB has terminator)
   - Local values tracked as `LocalEntry<'a> = (PointerValue<'a>, BasicTypeEnum<'a>, bool /*is_mut*/, bool /*is_param*/, bool /*is_weak*/, Option<String> /*nominal*/)`
4. Link: link object files with runtime to produce executable.

## Modules & Imports

- Entry point resolves transitive imports.
- Relative imports resolved with `.oats` extensions; directory imports try `index.oats`.
- Paths are canonicalized to avoid duplicate compilation and cycles.
- Compiled modules stored in a `HashMap<String, ParsedModule>`.

## Runtime Interop

- Runtime is a small C-callable library with:
  - RC helpers, allocators, string/array APIs
  - Cycle collector hooks and logging
- Adding a runtime helper requires:
  1. Implement in `crates/runtime/src/lib.rs` as `extern "C" fn`.
  2. Declare and cache in `crates/oats/src/codegen/mod.rs` (add a `RefCell<Option<FunctionValue>>` + getter that inserts the function into the module).
- Runtime logging via `OATS_RUNTIME_LOG` and collector logs via `OATS_COLLECTOR_LOG`.

## Diagnostics and Error Handling

- All fallible compiler APIs return `Result<T, Diagnostic>`.
- `.unwrap()`/`.expect()` are forbidden within `crates/oats/src` (enforced by ongoing audits). Use `Diagnostic::simple` or `Diagnostic::simple_with_span` and propagate via `?`.

## Testing Expectations

- `cargo build --workspace`
- `cargo test --workspace`
- Snapshot tests via `cargo insta review` / `cargo insta accept` for IR changes.
- Fuzzing (`cargo +nightly fuzz run fuzz_parser`) to cover parser robustness.
- Example smoke tests via `./scripts/run_all_proper_tests.sh` when making runtime or codegen changes.

## Quick Reference

- `LocalEntry<'a>`: `(PointerValue, BasicTypeEnum, is_initialized, is_const, is_weak, nominal_type_name, oats_type)`; used to drive scope cleanup and type-aware lowering.
- Async lowering context fields on `CodeGen` must be cleared after emitting a poll function to avoid leaking state across compilations.
- When adding new object types, update both codegen and runtime layouts, plus the memory design doc and relevant tests.

## Future Work

- Expand type system for generics and better nominal typing.
- Improve cycle collector heuristics and diagnostics.
- Compiler optimizations: escape analysis to elide RC ops where safe.
