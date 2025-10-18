# Escape Analysis & Redundant RC Elimination

**Last Updated:** October 16, 2025

**Status:** Implemented for intra-procedural analysis (October 2025)

The compiler now performs a conservative, intra-procedural escape analysis
before lowering each function. The resulting `EscapeInfo` drives selective
omission of redundant reference-counting operations for locals that provably do
not escape their defining scope.

## Implementation Snapshot

- Location: `crates/oats/src/codegen/escape.rs`
- Entry point: `CodeGen::analyze_fn(&self, func: &ast::Function) -> EscapeInfo`
- Data captured:
  - `escapes`: locals that must retain full RC semantics.
  - `definitions` / `uses_before_def`: help distinguish parameters from locals.
  - `await_live`: locals that remain live across `await` points (async-aware).
- Integration: `gen_function_ir` runs escape analysis before lowering and stores
  the result in `CodeGen.current_escape_info`. Expression/statement lowering
  consults this map when deciding whether to emit `rc_inc`/`rc_dec` around local
  stores.

## Analysis Highlights

1. **Scoping:** Tracks lexical scopes and definitions so redeclarations are
   handled precisely.
2. **Control Flow:** Inspects loops, conditionals, try/catch, and blocks to
   catch stores and uses in all branches.
3. **Async Awareness:** Marks locals that span `await` boundaries as escaping so
   resume frames retain correct ownership.
4. **Escape Triggers:** A value is marked escaping if it is returned, stored
   into heap memory (fields, array slots), passed to potentially retaining
   calls, or captured by closures/async state machines.

Locals not marked as escaping are eligible for eliding initial `rc_inc` calls
when first stored and `rc_dec` calls when dropped, provided they remain purely
stack-local.

## Safety Guarantees

- Escaping locals continue to emit full ARC operations.
- Field stores, global writes, and returns are treated conservatively and never
  elide RC calls.
- Analysis defaults to "safe" when unsure: ambiguous patterns keep RC ops.
- Async lowering always treats resume-frame locals as escaping.

## Testing Strategy

- Unit tests under `crates/oats/tests/codegen/` assert the presence or absence
  of RC calls in generated IR for targeted examples.
- Integration tests (`examples/` and `run_all_proper_tests.sh`) ensure runtime
  behavior remains correct.
- Fuzzing (`cargo +nightly fuzz run fuzz_parser`) continues to guard parser and
  lowering invariants.

## Known Limitations & Next Steps

- Analysis remains intra-procedural; interprocedural escapes (e.g., captured by
  returned closures) are marked conservatively.
- Calls are currently treated as escaping unless the callee is recognized as a
  pure helper. Annotated call-site effects are a future enhancement.
- Union boxing/unboxing still materializes heap objects even for non-escaping
  locals; stack-allocated boxes are a follow-up optimization.
- Feature flag `OATS_ELIDE_ARC` is reserved for future experimentation but is
  not yet exposed as a user-facing toggle.