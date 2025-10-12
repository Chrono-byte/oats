# Architecture Reference

**Last Updated:** October 12, 2025

This reference summarizes the current compiler architecture, runtime object
model, and the non-negotiable contracts between codegen and the runtime. It is
intended to be the authoritative overview for contributors touching core
systems.

## Workspace Overview

- `crates/oats` – Ahead-of-time compiler (parser, type checker, codegen).
  - `parser.rs` – TypeScript subset parsing via `deno_ast`.
  - `types.rs` – `OatsType` representation and type inference helpers.
  - `codegen/` – LLVM IR lowering (`emit.rs`, `expr.rs`, `stmt.rs`,
    `helpers.rs`, `const_eval.rs`, `escape.rs`).
  - `builder.rs` – Entry point that wires parsing, type checking, and codegen.
- `crates/runtime` – C-callable runtime linked into user binaries (ARC helpers,
  allocators, string/array primitives, logging, and the cycle collector).
- `examples/` – Representative `.oats` programs that compile under the current
  language surface.
- `docs/` – Design references (this folder).

## Compilation Pipeline

```
TypeScript-like source (.oats)
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

- `CodeGen` owns the LLVM context, module, and builder plus caches for runtime
  helper declarations (see `codegen/mod.rs`).
- `emit.rs` lowers top-level items (functions, constructors, globals).
- `expr.rs` lowers expressions and enforces `Result<T, Diagnostic>` error
  propagation.
- `stmt.rs` handles block/loop lowering and scope-driven RC cleanup.
- `const_eval.rs` materializes compile-time constants into LLVM globals and
  reuses them via stable interning keys.
- `escape.rs` computes intra-procedural escape analysis so locals that do not
  escape can skip redundant ARC operations.

### Async and Generics

- Async functions are lowered into poll-state machines with resume blocks and
  slot maps stored on `CodeGen`. Escape analysis is async-aware by tracking
  locals live across `await` points.
- Nested generic functions are monomorphized on demand; `CodeGen` caches
  specialization keys to avoid regenerating IR for the same instantiation.

## Heap Object Layout (Canonical Contract)

All heap-allocated objects share a 64-bit header at offset 0. Violating this
layout risks memory corruption.

- Offset 0: 64-bit header (atomic)
  - Bits 0–31: strong reference count (`u32`)
  - Bit 32: `STATIC` flag (1 = immortal literal; ARC ops are no-ops)
  - Bits 33–48: weak reference count (`u16`)
  - Bits 49–63: reserved for type tags / flags
- Offset 8: meta-slot (pointer to metadata / vtable). Never store user data
  here.
- Offset 16+: user fields or element data.

### Object Shapes

- **String**: `[header][i64 length][utf8 bytes ... NUL]` – Codegen and runtime
  operate on a pointer to the data at offset +16.
- **Array**: `[header][i64 length][element data ...]` – Runtime returns the base
  pointer (offset 0).
- **Class/Object**: `[header][i64 meta_ptr][field data ...]` – Base pointer is
  used by codegen; fields begin at offset +16.

## Reference Counting Protocol

- All codegen paths must call `rc_inc` before storing a strong pointer into a
  local, struct field, or array slot.
- Always call `rc_dec` on the old value before overwriting a pointer slot and
  when leaving a scope (`emit_rc_dec_for_locals` handles block cleanup).
- Weak references use `rc_weak_inc`, `rc_weak_dec`, and `rc_weak_upgrade`; they
  do not affect the strong count and are zeroed when the referent drops.
- Escape analysis may elide `rc_inc`/`rc_dec` pairs for locals that provably do
  not escape. Field stores and returns are conservative and still emit RC ops.
- Static literals have the `STATIC` bit set; RC helpers detect this and skip
  mutations.

## Runtime ABI Contracts

- All heap allocations must go through runtime helpers (strings, arrays,
  objects). They initialize headers with `RC=1` and return canonical pointers.
- `CodeGen` lazily declares runtime helpers (e.g., `get_rc_inc`,
  `get_union_box_ptr`). Every new runtime function needs both an implementation
  in `crates/runtime/src/lib.rs` and a getter in `codegen/mod.rs`.
- String/array helpers expect canonical pointers; use runtime canonicalization
  helpers if the pointer shape is ambiguous.
- Cycle collector relies on the meta-slot and field metadata emitted during
  class lowering; do not repurpose the slot.

## Diagnostics and Error Handling

- All fallible compiler APIs return `Result<T, Diagnostic>`.
- `.unwrap()`/`.expect()` are forbidden within `crates/oats/src` (enforced by
  ongoing audits). Use `Diagnostic::simple` or `Diagnostic::simple_with_span`
  and propagate via `?`.

## Testing Expectations

- `cargo build --workspace`
- `cargo test --workspace`
- Snapshot tests via `cargo insta review` / `cargo insta accept` for IR changes.
- Fuzzing (`cargo +nightly fuzz run fuzz_parser`) to cover parser robustness.
- Example smoke tests via `./scripts/run_all_proper_tests.sh` when making
  runtime or codegen changes.

## Quick Reference

- `LocalEntry<'a>`:
  `(PointerValue, BasicTypeEnum, is_initialized, is_const,
  is_weak, nominal_type_name, oats_type)`;
  used to drive scope cleanup and type-aware lowering.
- Async lowering context fields on `CodeGen` must be cleared after emitting a
  poll function to avoid leaking state across compilations.
- When adding new object types, update both codegen and runtime layouts, plus
  the memory design doc and relevant tests.
