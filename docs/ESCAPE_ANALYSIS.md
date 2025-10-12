Escape Analysis & Redundant RC Elimination

Goal

Add a local escape-analysis pass and a simple redundant-ARC elimination phase to
reduce unnecessary runtime reference-count operations. This targets short-lived
temporaries and values that never escape their defining function or stack frame.

Scope & constraints

- Start with a conservative, intra-procedural escape analysis (no
  interprocedural or whole-program analysis).
- Only apply to pointer-like values (strings, arrays, objects, union-boxed
  pointers).
- Keep semantics identical: existing reference-counting invariants must be
  preserved. When in doubt, conservatively emit RC ops.
- Integrate safely with async lowering (resume frames) and closure captures by
  treating captured locals as escaping.

Design overview

1. Analysis phase (in codegen/analysis or codegen/escape.rs)
   - Input: function IR lowering AST context (before lowering into final LLVM
     instructions).
   - Build a local def-use graph for local allocas and temporary values tracked
     in `locals_stack`.
   - For each local, mark as "escapes" if it is:
     - Returned from the function.
     - Stored into a heap/global/field or a variable that may outlive the stack
       (e.g., a field write into an object, or stored into a captured closure
       local).
     - Passed as an argument to a call where the callee may retain it
       (conservatively assume external functions escape unless the callee is a
       known internal function with annotations).
     - Captured by an `async` state machine or closure.
   - Locals not marked as escaping are "stack-local" and safe for ARC elision.

2. Emission changes (codegen)
   - When lowering stores to locals/fields/returns, consult the escape analysis
     result:
     - If storing a pointer into a local that does NOT escape, avoid calling
       `rc_inc` when initializing it (no ownership transfer needed across heap).
     - If overwriting a local that does NOT escape, avoid calling `rc_dec` on
       the old value when dropping (still ensure no double-free for nested
       pointers).
     - For storing into fields or global objects, maintain existing
       `rc_inc/rc_dec` semantics (these are escapes).
   - For temporaries used only inside the function, avoid boxing and ARC ops
     where feasible (for union boxing, consider stack-allocated temporaries or
     scoped boxed region; this is a later improvement).

3. Safety & fallbacks
   - Provide codegen toggles via feature flag or environment variable (for
     testing): `OATS_ELIDE_ARC=1`.
   - Start with a conservative mode: only elide for locals that are clearly
     never used in escapes and where their destructor (if any) is trivial.

Testing plan

- Unit tests: add tests under `crates/oats/tests/codegen/escape_*` covering:
  - Non-escaping local pointer: ensure emitted IR omits `rc_inc/rc_dec` pairs
    around local stores.
  - Escaping local (returned or stored in a field): ensure `rc_inc/rc_dec` still
    emitted.
  - Async-local captured in state-machine: treated as escaping.
- Runtime tests: microbenchmarks comparing allocations & RC operations
  before/after elision on synthetic workloads.
- Fuzz/regression: run `./scripts/run_fuzzing.sh` to ensure no semantic
  regressions.

Implementation tasks

- [ ] Add `crates/oats/src/codegen/escape.rs` with API
      `fn analyze_fn(&self, func_ast: &ast::Function) -> EscapeInfo`.
- [ ] Wire analysis into `gen_function_ir` so analysis runs before lowering and
      result is stored in `CodeGen` (e.g., `current_escape_info`).
- [ ] Modify store/assign lowering paths (in `stmt.rs` and `expr.rs`) to consult
      `current_escape_info` when deciding whether to emit `rc_inc`/`rc_dec`
      around local/allocation stores.
- [ ] Add tests and benchmarks.

Notes

- This is intentionally conservative and local; future work can expand to
  interprocedural analysis, lifetime inference, and stack-scoped box
  optimization.
- If we later add a production cycle collector, keep RC-elision passes
  compatible so that collector invariants are not broken.
