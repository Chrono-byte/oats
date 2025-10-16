# Memory Design

**Last Updated:** October 16, 2025

This document describes the canonical memory layout, reference-counting rules,
and safety guidance contributors must follow when modifying runtime or codegen.

## Object Layout (canonical)

- Offset 0: 64-bit header (atomic) — strong reference count (bits 0..31) and
  flags
- Bit 32 in header: STATIC bit (1 = immortal literal; RC ops are no-ops)
- Bits 33..48: weak reference count (u16)
- Bits 49..63: type tag / flags (reserved)
- Offset 8: meta-slot (pointer to field metadata / vtable)
- Offset 16+: fields or element data

Notes:

- Static string literals set the STATIC bit and expose a pointer to data at
  offset +16 (codegen must return data pointer for strings).
- Heap strings/arrays/objects are allocated by runtime helpers and return
  canonical pointers as documented in archived specs.

## Pointer rules & canonicalization

- Runtime RC helpers accept two forms: object base pointers (pointing at
  offset 0) or string-data pointers (pointing at offset +16).
- Use the runtime canonicalizer (`get_object_base()` / equivalent) when you
  receive external pointers before performing RC operations.
- Heuristics are used to distinguish pointers; generated code must avoid passing
  arbitrary integers or non-canonical pointers to RC helpers.

## RC rules (when to call)

1. When storing a pointer into a field or a heap slot: call `rc_inc(ptr)` before
   the store (for strong refs).
2. When overwriting a stored pointer: call `rc_dec(old_ptr)` before overwriting
   to avoid leaks.
3. For function returns and returning local-owned pointers: ensure ownership and
   RC semantics are preserved (emit `rc_dec` on locals you no longer own).
4. For weak references: use `rc_weak_inc` / `rc_weak_dec` and `rc_weak_upgrade`
   when temporarily upgrading to a strong ref.
5. For unowned (proposal): treat as raw pointers — no RC ops. Only use when
   lifetime guarantees are clear.

## Meta-slot and field metadata

- The 8-byte word at offset +8 is reserved for the object's metadata pointer
  (field map, vtable). Codegen must not use offset +8 for a regular field.
- The field map describes pointer-field offsets used by the cycle collector and
  neighbor gathering routines.

## Allocation & size safety

- Always use provided runtime allocation helpers for strings, arrays, and
  objects. Helpers perform header initialization and return the correct pointer
  shape.
- Use checked arithmetic (`checked_add`, `checked_mul`) when computing
  allocation sizes to avoid integer overflow and under-allocation.
- Observe resource limits (environment variables like `OATS_MAX_HEAP_BYTES`,
  `OATS_MAX_ALLOC_BYTES`, `OATS_MAX_SOURCE_BYTES`) in tools and tests.

## Cycle collection (high level)

- The planned collector uses a trial-deletion algorithm as a safety net for
  cycles created by strong references. -- Use weak/unowned references to break
  cycles when appropriate.

## Thread safety & atomicity

- All strong/weak count updates operate on the 64-bit header with atomic CAS
  loops. Use `Acquire/Release` ordering as documented in runtime code.
- The collector and some helper subsystems may use locks (e.g., `RwLock`) for
  root sets; avoid taking long locks in hot paths.

## Testing & validation

- Unit tests: add tests to `crates/runtime/tests/` that validate header layout
  and RC invariants (strong/weak behavior, upgrade semantics).
- Integration: add end-to-end tests that allocate objects, create cycles, and
  ensure collector reclaims cycles where expected.
- Snapshot tests: codegen snapshots should include IR that depends on header
  layout so accidental layout changes are caught.

## Quick checklist for code changes touching memory/RC

- [ ] Did I use runtime allocation helpers instead of manual alloc?
- [ ] Are all stores to pointer fields preceded by `rc_inc` (or marked unowned)?
- [ ] Are overwritten pointer slots decremented (`rc_dec`)?
- [ ] Did I run unit and integration tests (runtime and codegen snapshots)?
