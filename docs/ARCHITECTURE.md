# Architecture Reference

This concise architecture reference captures the essential runtime object model
and codegen contracts contributors must follow.

## Heap Object Layout

- Offset 0: 64-bit header (strong refcount + flags)
- Offset 8: meta-slot (pointer to field metadata)
- Offset 16+: fields or data

Notes:

- Strings: codegen returns pointer to data at offset +16
- Arrays/objects: runtime returns base pointer (offset 0)
- Never overwrite the meta-slot at offset +8; fields start at offset +16

## Pointer Rules & RC

- Runtime RC helpers accept either base pointers (offset 0) or string-data
  pointers (offset 16); use runtime canonicalizers before RC ops.
- When storing pointers into fields or locals: call `rc_inc` first; when
  dropping/overwriting call `rc_dec`.

## Codegen Contracts

1. Static strings must have header static bit set.
2. Heap allocations should use runtime helpers and initialize header with RC=1.
3. Emit `rc_inc`/`rc_dec` according to storage semantics.
