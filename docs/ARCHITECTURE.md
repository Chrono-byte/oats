# Architecture & Language Design

**Last Updated:** October 18, 2025

## Current Status

Oats implements TypeScript/JavaScript with:

- Language: classes, async/await, generics, unions, interfaces, enums, destructuring, template literals
- Types: full primitive set (integers, floats, char), tuples, promises, weak/unowned references
- Memory: ARC with cycle collection, escape analysis for RC elimination
- Runtime: C-callable with helpers for strings, arrays, RC operations

## Overview

Oats is an AOT compiler that compiles TypeScript/JavaScript to native executables via LLVM. Goals: predictable performance, small safe runtime, full TypeScript/JavaScript compatibility. Memory model is inspired by Swift's ARC, adapted for a deterministic low-level runtime.

## Design Goals

- Predictable performance and memory layout for native interop
- Simple semantics that map well to LLVM and C-callable runtime
- Safe, deterministic reference counting with weak refs and cycle collection
- Full TypeScript/JavaScript compatibility

## Source Model & Syntax

- Full TypeScript/JavaScript syntax support (`let`, `const`, `function`, `class`, `import`/`export`, etc.)
- Source size limits enforced in parser
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
  - `parser.rs` – TypeScript/JavaScript parsing via `deno_ast`.
  - `types.rs` – `OatsType` representation and type inference helpers.
  - `codegen/` – LLVM IR lowering (`emit.rs`, `expr.rs`, `stmt.rs`, `helpers.rs`, `escape.rs`).
  - `builder.rs` – Entry point that wires parsing, type checking, and codegen.
- `crates/runtime` – C-callable runtime linked into user binaries (ARC helpers, allocators, string/array primitives, logging, and the cycle collector).
- `examples/` – Representative TypeScript/JavaScript programs that compile.
- `docs/` – Design references (this folder).

## Compilation Pipeline

```text
TypeScript/JavaScript source (.ts/.js/.oats)
  ↓  parser.rs (deno_ast)
Abstract Syntax Tree (AST)
  ↓  types.rs (type checking / OatsType mapping)
Typed AST
  ↓  codegen/* (inkwell) + escape.rs
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
- `escape.rs` computes intra-procedural escape analysis so locals that do not escape can skip redundant ARC operations.

### Async and Generics

- Async functions are lowered into poll-state machines with resume blocks and slot maps stored on `CodeGen`. Escape analysis is async-aware by tracking locals live across `await` points.
- Nested generic functions are monomorphized on demand; `CodeGen` caches specialization keys to avoid regenerating IR for the same instantiation.

## Heap Object Layout

All heap objects share a 64-bit header at offset 0. Don't violate this layout or you'll corrupt memory.

- Offset 0: 64-bit header (atomic)
  - Bits 0–31: strong reference count (`u32`)
  - Bit 32: `STATIC` flag (1 = immortal literal; ARC ops are no-ops)
  - Bits 33–48: weak reference count (`u16`)
  - Bits 49–63: reserved for type tags / flags
- Offset 8: meta-slot (pointer to metadata / vtable). Don't store user data here.
- Offset 16+: user fields or element data.

### Object Shapes

- **String**: `[header][i64 length][utf8 bytes ... NUL]` – Codegen and runtime operate on a pointer to the data at offset +16.
- **Array**: `[header][i64 length][element data ...]` – Runtime returns the base pointer (offset 0).
- **Class/Object**: `[header][i64 meta_ptr][field data ...]` – Base pointer is used by codegen; fields begin at offset +16.

## Reference Counting Protocol

- Call `rc_inc` before storing a strong pointer into a local, struct field, or array slot.
- Call `rc_dec` on the old value before overwriting a pointer slot and when leaving a scope (`emit_rc_dec_for_locals` handles block cleanup).
- Weak references use `rc_weak_inc`, `rc_weak_dec`, and `rc_weak_upgrade`. They don't affect the strong count and are zeroed when the referent drops.
- Escape analysis can skip `rc_inc`/`rc_dec` pairs for locals that don't escape. Field stores and returns are conservative and still emit RC ops.
- Static literals have the `STATIC` bit set; RC helpers detect this and skip mutations.

## Values & Heap Object Layout

All heap objects share a 64-bit header at offset 0. This is the contract between codegen and runtime. The runtime implements deterministic ARC-style ownership similar to Swift's ARC: references are strong by default, weak references are non-owning and get zeroed when the referent is deallocated, and explicit ownership annotations control reference behavior in generated code.

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

ARC discipline (compiler responsibilities):

- Strong is the default: storing a pointer in a field/local must emit `rc_inc` for the new value and `rc_dec` for the old value (if any) before overwrite.
- Overwriting a pointer: `rc_dec` the old value before replacing it.
- Returning from a function: release (`rc_dec`) any function-scoped strong locals before returning.
- Breaking/continuing loops: release (`rc_dec`) loop-scoped strong locals before jumping out of scope.
- Prefer `weak` for breaking cycles. Use `rc_weak_inc`, `rc_weak_dec`, and `rc_weak_upgrade` for weak operations; `rc_weak_upgrade` returns an optional strong reference.

Runtime exposes helpers for allocation, string/array ops, RC ops, and a cycle collector. New runtime helpers need both runtime implementation and declaration in the CodeGen struct.

## Runtime ABI Contracts

- All heap allocations must go through runtime helpers (strings, arrays, objects). They initialize headers with `RC=1` and return canonical pointers.
- `CodeGen` lazily declares runtime helpers (e.g., `get_rc_inc`, `get_union_box_ptr`). Every new runtime function needs both an implementation in `crates/runtime/src/lib.rs` and a getter in `codegen/mod.rs`.
- String/array helpers expect canonical pointers; use runtime canonicalization helpers if the pointer shape is ambiguous.
- Cycle collector relies on the meta-slot and field metadata emitted during class lowering; do not repurpose the slot.

## Compiler Pipeline

1. Parse: `deno_ast` → AST (parser enforces size limits).
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
- Relative imports resolved with standard TypeScript/JavaScript module resolution.
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
- `.unwrap()`/`.expect()` are forbidden within `crates/oats/src`. Use `Diagnostic::simple` or `Diagnostic::simple_with_span` and propagate via `?`.

## Testing Expectations

- `cargo build --workspace`
- `cargo test --workspace`
- Snapshot tests via `cargo insta review` / `cargo insta accept` for IR changes.
- Fuzzing (`cargo +nightly fuzz run fuzz_parser`) to cover parser robustness.

## Quick Reference

- `LocalEntry<'a>`: `(PointerValue, BasicTypeEnum, is_initialized, is_const, is_weak, nominal_type_name, oats_type)`; used to drive scope cleanup and type-aware lowering.
- Async lowering context fields on `CodeGen` must be cleared after emitting a poll function to avoid leaking state across compilations.
- When adding new object types, update both codegen and runtime layouts, plus the memory design doc and relevant tests.

## Compiler Architecture Review

### Overall Structure

The compiler follows a clean pipeline:

```text
Source → Parser → Type Checker → Code Generator → LLVM IR → Object File → Executable
```

**Strengths:**

- Clear separation between `oats_parser`, `oats_ast`, `oatsc` (compiler), `runtime`, and `toasty` (build system)
- Well-defined interfaces between components
- Modular design allows independent development

**Observations:**

- `builder.rs` is quite large (1370+ lines) and handles multiple concerns
- Could split into smaller modules: compilation orchestration, linking
- Note: Package-level concerns (manifest parsing, dependency resolution) are handled by `toasty`, not `oatsc`

### Parser

**Parser (`crates/oatsc/src/parser.rs`):**

- Uses `deno_ast` for parsing (good choice for TypeScript compatibility)
- Enforces security limits (MAX_SOURCE_SIZE, MAX_AST_DEPTH)
- Properly tracks mutable variable declarations
- Security-conscious with configurable limits

### Type System

**Type System (`crates/oatsc/src/types.rs`):**

- Comprehensive `OatsType` enum covering primitives, unions, generics, etc.
- Symbol table with lexical scoping
- Function signature tracking
- Type system is well-designed and extensible

### Code Generation

**Code Generation (`crates/oatsc/src/codegen/`):**

- Well-organized into submodules: `emit.rs`, `expr.rs`, `stmt.rs`, `helpers.rs`
- Proper LLVM IR generation using `inkwell`
- Escape analysis for RC optimization
- Const evaluation and interning
- Codegen is well-structured and feature-rich

### Runtime System

**Runtime (`crates/runtime/`):**

- C-callable interface for generated code
- Deterministic ARC with cycle collection
- Security limits (MAX_RECURSION_DEPTH, heap limits)
- Well-documented memory layout contracts

**Key Contracts:**

- 64-bit header layout is clearly documented
- Reference counting protocol is well-defined
- Object layouts are consistent and documented

## Security & Safety

### Security Measures

- Source file size limits (10MB default, configurable)
- AST depth limits (100 default, configurable)
- Runtime recursion depth limits (128, non-configurable)
- Heap limits (1GB default, configurable)
- Allocation size limits (256MB default, configurable)

Good defense-in-depth approach. Limits are configurable via environment variables. Some limits are hardcoded for security.

### Memory Safety

- ARC system with proper reference counting
- Cycle collector for handling circular references
- Escape analysis to optimize RC operations
- Weak references properly implemented

Runtime uses `unsafe` blocks (necessary for C FFI). Need to ensure all unsafe code is properly documented and verified.

### Type Safety

- Strong type system with comprehensive type checking
- Type inference for expressions
- Generic type support with monomorphization
- Union types properly handled

Type system is well-designed. Some type checking could be more strict (see TODOs in DEVELOPMENT.md).

## Performance Considerations

**Compilation Performance:**

- Single-pass parsing
- Const evaluation and interning (reduces code size)
- Escape analysis (optimizes RC operations)

Could optimize LLVM IR generation and improve const evaluation caching. Module caching and parallelization are handled by `toasty` at the package level.

**Runtime Performance:**

- ARC with escape analysis optimization
- Cycle collector runs in background
- Efficient object layouts
- Static literals avoid RC overhead

Runtime is designed for performance with a good balance between safety and speed.

## Future Work

- Expand type system for generics and better nominal typing
- Improve cycle collector heuristics and diagnostics
- Compiler optimizations: escape analysis to elide RC ops where safe
- Refactor large files (especially `builder.rs`) into smaller modules
- Complete error handling audit (remove all `.unwrap()`/`.expect()` calls)
