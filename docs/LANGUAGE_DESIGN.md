# Oats Language Design

## Overview

Oats is an ahead-of-time compiler that lowers a TypeScript-compatible language
to native executables via LLVM. It targets predictable performance, a small safe
runtime, and a compact language surface suitable for systems programming. The
language surface and type system follow TypeScript conventions where practical;
the memory model and ownership semantics are inspired by Swift's ARC (Automatic
Reference Counting) adapted for a deterministic, low-level runtime.

## Design Goals

- Predictable performance and memory layout for interop with native code.
- Clear, small semantics that map well to LLVM and a C-callable runtime.
- Safe, deterministic reference-counted memory management with weak refs and
  cycle collector support.
- Developer ergonomics inspired by TypeScript syntax and types, but trimmed for
  AOT compilation.

## Source Model & Syntax

- Syntax is a restricted subset of TypeScript:
  - `let` / `let mut`, `function`, `class`, `import`/`export`. -- Semicolons are
    enforced; `var` is rejected. `const` is supported as an immutable binding
    (preferred for values that should not change). The language also includes an
    explicit `let mut` form to declare mutable bindings when needed.
  - Source size limits apply (enforced in parser).
- No dynamic eval. Modules are statically resolved at compile time.

## Types

- Core type set (OatsType):
  - Number (f64)
  - Boolean
  - String
  - Array<T>
  - NominalStruct / Class
  - Union (either numeric-only or pointer)
  - Weak<T> (zeroing weak references)
  - Nominal typing metadata retained for runtime semantics
  - Optional/nullable values are represented using TypeScript-style unions (for
    example `T | null` / `T | undefined`) and are lowered according to the
    target `OatsType` semantics.
- Mapping to LLVM:
  - Number → `f64`
  - Boolean → `i1` or `i8` as needed
  - String / Array / Struct → `i8*` (pointer to data or base)

## Values & Heap Object Layout

All heap objects share a 64-bit header at offset 0. This header layout is the
canonical contract between codegen and runtime. The runtime implements
deterministic ARC-style ownership similar to Swift's ARC: references are strong
by default, weak references are non-owning and will be zeroed when the referent
is deallocated, and a small set of explicit ownership annotations control
reference behaviour in generated code.

Header (64 bits):

- Bits 0–31: strong refcount (atomic u32)
- Bit 32: static flag (1 = immortal; RC ops are no-ops)
- Bits 33–48: weak refcount (u16)
- Bits 49–63: type tag / flags

Object layouts (byte offsets)

- Static string:
  - [header+static][i64 len][data + NUL]
  - Codegen returns pointer to string data (offset 16)
- Heap string:
  - [header RC=1][i64 len][data + NUL]
  - Runtime returns pointer to string data (offset 16)
- Array:
  - [header][i64 len][elements...]
  - Returns base pointer (offset 0)
- Class / Object:
  - [header][i64 meta_ptr][fields...]
  - Returns base pointer (offset 0)
  - Offset +8 is the meta-slot (reserved for field map pointer) — never
    overwrite with fields. Fields start at offset 16.

Pointer rules and ARC semantics:

- Runtime RC helpers accept either base pointers (offset 0) or string data
  pointers (offset 16).
- Runtime uses `get_object_base(p)` to canonicalize pointers.
- Strong references: default for fields, locals and parameters. The compiler
  must emit `rc_inc` when storing a new strong pointer into a heap field/local
  and `rc_dec` when overwriting or releasing it.
- Weak references: declared with `weak`/`Weak<T>`. Weak references do not
  increment the strong count; the runtime tracks weak owners using the weak
  refcount and/or side tables. When an object is deallocated its weak references
  are automatically zeroed (set to `null`), matching Swift's zeroing weak
  semantics. Use `rc_weak_upgrade` to attempt to convert a weak reference to a
  strong one; the result is optional and must be checked.
- Unowned references: an `unowned` annotation expresses a non-owning,
  non-optional reference that is expected to outlive the referent. `unowned`
  references do not increment the strong count and are not zeroed; using an
  invalid `unowned` reference is undefined behaviour at the runtime level
  (implementations may trap). Use `unowned` only when lifetimes are guaranteed
  by program structure.
- Static literals have the static bit set; RC ops are no-ops for them.

## Reference Counting & Memory Management

- Mandatory ARC discipline (compiler responsibilities):
  - Strong is the default: storing a pointer in a field/local must emit `rc_inc`
    for the new value and `rc_dec` for the old value (if any) before overwrite.
  - Overwriting a pointer: `rc_dec` the old value before replacing it.
  - Returning from a function: release (`rc_dec`) any function-scoped strong
    locals before returning as required by the calling convention and ownership
    contract.
  - Breaking/continuing loops: release (`rc_dec`) loop-scoped strong locals
    before jumping out of scope.
  - Prefer `weak` for breaking cycles. Use `rc_weak_inc`, `rc_weak_dec`, and
    `rc_weak_upgrade` for weak operations; `rc_weak_upgrade` returns an optional
    strong reference.
- Runtime exposes helpers for allocation, string/array ops, RC ops, and a cycle
  collector. New runtime helpers require both runtime implementation and
  declaration in the CodeGen struct.

## Compiler Pipeline

1. Parse: `deno_ast` → AST (parser enforces semicolons, rejects `var`, enforces
   size limits).
2. Type check: map TypeScript-like types to `OatsType`.
3. Codegen:
   - `CodeGen` struct holds LLVM context, cached runtime function declarations,
     and helper types (`i8ptr_t`, `i64_t`, etc.).
   - Lowering is split:
     - `emit.rs` — top-level functions & constructors
     - `expr.rs` — `lower_expr() -> Result<BasicValueEnum, Diagnostic>`
     - `stmt.rs` — `lower_stmt() -> Result<bool, Diagnostic>` (returns whether
       BB has terminator)
   - Local values tracked as
     `LocalEntry<'a> = (PointerValue<'a>, BasicTypeEnum<'a>, bool /*is_mut*/, bool /*is_param*/, bool /*is_weak*/, Option<String> /*nominal*/)`
4. Link: link object files with runtime to produce executable.

## Error Handling & Diagnostics

- All codegen lowering functions return `Result<T, Diagnostic>`.
- No `unwrap()`/`expect()` allowed in codegen sources—use
  `Diagnostic::simple(...)` or `Diagnostic::simple_with_span(...)`.
- Use the `?` operator to propagate diagnostics.
- Diagnostics follow a rustc-style model with spans and messages.

## Modules & Imports

- Entry point resolves transitive imports.
- Relative imports resolved with `.ts` / `.oats` extensions; directory imports
  try `index.ts` / `index.oats`.
- Paths are canonicalized to avoid duplicate compilation and cycles.
- Compiled modules stored in a `HashMap<String, ParsedModule>`.

## Runtime Interop

- Runtime is a small C-callable library with:
  - RC helpers, allocators, string/array APIs
  - Cycle collector hooks and logging
- Adding a runtime helper requires:
  1. Implement in `crates/runtime/src/lib.rs` as `extern "C" fn`.
  2. Declare and cache in `crates/oats/src/codegen/mod.rs` (add a
     `RefCell<Option<FunctionValue>>` + getter that inserts the function into
     the module).
- Runtime logging via `OATS_RUNTIME_LOG` and collector logs via
  `OATS_COLLECTOR_LOG`.

## Testing & Development Workflow

- Setup LLVM 18 environment before building: `source ./scripts/setup_env.sh`
- Build & test:
  - `cargo build --workspace`
  - `cargo test --workspace`
  - `cargo clippy --workspace`
- Snapshot testing uses `insta` for IR snapshots. Use `cargo insta review` and
  `cargo insta accept` with PR explanation.
- Fuzzing scripts available: quick and long-running modes.
- Before commit checklist:
  - All tests pass.
  - No new `unwrap()` / `expect()` in `crates/oats/src/`.
  - Runtime tests for changes to object layout/RC.
  - Update snapshots if IR changed; explain changes.

## Standard Library & Primitives

- Minimal standard runtime: strings, arrays, RC helpers, allocations.
- Primitive operations lowered to direct LLVM ops where possible; pointer/value
  conversions follow the type mapping table.

## Examples (minimal)

- Strings: compile-time static strings are emitted with static header and
  returned as data pointers (offset 16).
- Arrays: created via runtime allocators; compiler ensures `rc_inc` when stored
  and `rc_dec` when released.

## Future Work

- Expand type system for generics and better nominal typing.
- Improve cycle collector heuristics and diagnostics.
- Compiler optimizations: escape analysis to elide RC ops where safe.

## Reference Quick Notes

- LocalEntry tuple structure in codegen for locals and params.
- Object meta-slot at offset +8 — reserved.
- RC ops must be explicit in codegen; static objects bypass RC.
- Codegen functions must return `Result<T, Diagnostic>` and propagate
  diagnostics.

For implementation details, see docs/ARCHITECTURE.md and crates/{oats,runtime}
source files.
