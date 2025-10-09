Phase A complete - Stabilization summary

This small document summarizes the verification steps taken for Phase A and
outlines the immediate next actions for Phase B.

What we completed

- Enforced object layout:
  - header@0 (u64), meta_ptr@8 (pointer to metadata global), fields@16+
- Codegen:
  - Constructors and object-literals reserve `meta_slot` and zero-init fields
  - Per-class `<Class>_field_map` global created (meta0 + offsets)
  - Stores pointer to global at object offset +8
- Runtime:
  - Hardened `validate_meta_block` with unit tests
  - Trial-deletion collector scaffold implemented; logging opt-in via OATS_COLLECTOR_LOG
  - rc_inc/rc_dec robustified (plausibility guards during transition)
- Tests:
  - Unit tests for runtime and codegen passed
  - Integration test `regress_cycle_reclaim` (AOT compile + run) passed

Quick verification commands

Run all tests:

```bash
cargo test
```

Run only codegen tests:

```bash
cargo test -p oats --test codegen
```

Next actions (Phase B)

1. Module resolution & multi-file compilation (critical)
2. Arrow functions (non-capturing) and object literal improvements
3. Closure capture impl planning (escape analysis + closure structs)

Notes

- Keep `rc_dec` plausibility guard for a little longer; remove after Phase B if
  no regressions observed.
- Add IR-level tests to assert metadata global alignment and constructor sizes
  (optional but recommended).