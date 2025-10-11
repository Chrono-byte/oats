# Oats — Architecture Reference

**Status:** Current and accurate as of October 10, 2025  
**For:** Contributors and maintainers implementing or modifying core systems

This document is a concise, developer-facing reference describing the runtime object model, memory management, and important codegen contracts used by the Oats TypeScript→LLVM AOT compiler. Keep this page short and authoritative — it should answer "how objects are laid out, who is responsible for RC, and what codegen must do".

If you need deeper implementation notes, see the code in `crates/runtime/src/lib.rs` and `crates/oats/src/codegen`.

## Quick reference — header & invariants

- Header size: 8 bytes at offset 0 for all heap objects.
- Header (u64) layout:
  - bits 0..31: strong reference count (atomic, u32)
  - bit 32: static flag (1 = immortal / do not touch RC)
  - bits 33..48: weak reference count (u16) — reserved but present in runtime
  - bits 49..63: type tag / flags (reserved)

Codegen and runtime must preserve these invariants. The runtime helpers expect either an object base pointer (points to offset 0) or a string-data pointer (points to offset 16); runtime helpers will canonicalize to the object base.

## Object layouts (canonical)

- Static string literal (global, constant):
  - offset 0: i64 header (static bit set)
  - offset 8: i64 length
  - offset 16: data bytes (N), NUL-terminated
  - codegen returns pointer → data (offset 16)

- Heap string:
  - offset 0: i64 header (RC = 1)
  - offset 8: i64 length
  - offset 16: data bytes (N), NUL-terminated
  - runtime returns pointer → data (offset 16)

- Array:
  - offset 0: i64 header (RC = 1)
  - offset 8: i64 length
  - offset 16: elements (homogeneous layout)
  - runtime returns pointer → base (offset 0)

- Class / object:
  - offset 0: i64 header (RC = 1)
  - offset 8: i64 meta slot (pointer to per-class metadata / field map)
  - offset 16+: fields (8 bytes per field)
  - constructors and runtime return pointer → base (offset 0)

Important: the 8-byte word at offset +8 is reserved as the meta-slot. Codegen must not overwrite it with plain fields; fields begin at offset 16.

## Pointer rules and RC helpers

- Two pointer kinds are accepted by runtime RC helpers:
  1) object base pointers (base@0)
  2) string data pointers (data@16)
- Runtime exposes a canonicalizer: get_object_base(ptr) — it tests offset 0 and offset -16 and returns the true base pointer or NULL if invalid.
- Always call rc_inc/rc_dec (and rc_weak_inc/rc_weak_dec if weak refs are used) when storing or releasing references.
- For function returns and local cleanup, emit appropriate rc_dec calls before returning to avoid leaks.

## Codegen contracts (must-follow checklist)

1. When you create a static string literal, set the header's static bit and expose a global whose address-of-data is returned (pointer -> offset 16).
2. Heap allocations for strings/arrays/objects must initialize header with RC=1 and set meta-slot to the emitted metadata pointer (classes).
3. When storing a pointer into a field or local, emit rc_inc for pointer types; when overwriting/dropping, emit rc_dec.
4. Use the runtime allocation helpers for arrays/strings when possible. Do not implement custom alloc logic that violates header layout.
5. Compute byte offsets using a consistent pattern (ptr -> int + add(i64) + int -> ptr) to avoid subtle mismatches that corrupt the meta-slot.

## Closure layout (summary)

Two practical layouts are used depending on static knowledge:

- compact closure (when return type is known):
  - offset 0: header
  - offset 8: meta
  - offset 16: fn_ptr
  - offset 24: env_ptr

- tagged fallback (when return type unknown):
  - same as compact, plus offset 32: i64 ret_tag

The compact layout is preferred where possible; the tagged layout is used conservatively.

## Memory-management roadmap (short)

- Phase 0 (current): Automatic reference counting with deterministic destruction. Compiler injects rc_inc/rc_dec.
- Phase 1 (planned): Escape analysis and stack allocation for short-lived objects.
- Phase 2 (planned): Cycle detection and reclaim (trial-deletion collector) + optional weak pointers runtime support.

## Runtime functions (high-level list)

- Allocation / RC: malloc, free, rc_inc, rc_dec, rc_weak_inc, rc_weak_dec, rc_weak_upgrade
- Strings: heap_str_alloc, str_concat, number_to_string
- Arrays: array_alloc, array_get_*, array_set_*
- I/O: print_str, print_f64

If you add a new runtime helper, document it in the runtime crate and declare it in the codegen's external function table.

## Testing & quality gates

- Unit tests in `crates/runtime/tests` should validate header layout and basic RC invariants.
- Codegen snapshot tests (insta) should capture IR that depends on object layout so accidental changes are visible.
- Before merging changes that touch header layout or codegen field stores, run full integration tests (compile + run selected examples) and include runtime tests that check meta-slot integrity.

---

Short completion checklist

- Where to look: `crates/oats/src/codegen/*`, `crates/runtime/src/lib.rs`.
- If you change field offsets or header semantics: update docs, tests, and add runtime assertions in debug mode.

This file is intentionally short. For design history and longer proposals (cycle collector, async, roadmap) see `CCRC.md`, `DEVELOPMENT.md`, and `ROADMAP.md`.