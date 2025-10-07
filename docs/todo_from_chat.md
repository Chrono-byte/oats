# Oats — remaining TODO (from compiler chat)

This file summarizes remaining work discussed in the compiler chat and the repository notes, updated to reflect the short-term changes that were just implemented (diagnostics improvements + a couple of focused tests). Use this as a living, prioritized checklist.

## Summary (high level)

- Short-term items (easy): mostly done — diagnostics improved and a few focused tests added. A small set of extra tests and diagnostics unit tests remain.
- Medium-term items (moderate): design + implement richer type system, class lowering, and a basic module/import model.
- Long-term items (hard): closures + captures, a GC or improved memory management, and async/await + promise/runtime.

---

## Short-term (remaining / follow-ups)

These short-term items were the focus of the recent changes. Below is a concise status update — the detailed task list was intentionally removed and the Medium/Long-term sections below have been shifted up.

- Diagnostics improvements: Done (help hints added; `report_error_span_and_bail` includes hint text for tests).
- Focused tests: Done (added diagnostics + for-of tests; targeted tests pass locally).
- Member-write lowering (e.g. `this.x = ...`): Pending — requires a small, careful AST-based lowering change and a focused test asserting emitted IR for RC helpers.
- CI / infra (LLVM-18 job, aot_run smoke test): Pending (recommended next step after finishing member-write lowering).

Estimated effort to finish remaining short-term item (member-write lowering + test): ~0.5–1 day.

---

## Medium-term (design + implementation)

These are larger, multi-file changes that need careful design and tests.

- Expand the Type System
  - Tasks:
    - Add union, tuple, and generics support to `types.rs` (extend `OatsType`).
    - Update type-checker mapping (`map_ts_type`) and coercion rules.
    - Add codegen mapping for new types (how they lower to LLVM types and runtime representation).
  - Contract: API-level changes must preserve existing `NominalStruct`/array behavior and pass current tests.
  - Edge-cases: recursive type definitions, inference with array literals, type erasure vs monomorphization decision.

- Basic class implementation and field layout
  - Tasks:
    - Implement `ClassDecl` lowering: layout for `NominalStruct`, field offsets, constructors, and method emission.
    - Ensure `this` receiver handling and param type registration across `gen_function_ir`.
    - Add tests that compile a class with properties, generate IR, and assert `field_load`/`field_store` exist and offsets align with the runtime layout.
  - Contract: guarantee ABI stability between codegen and `crates/runtime` (update runtime helpers if field layout changes).

- Module system (imports/exports)
  - Tasks:
    - Design multi-file resolution (single AOT compile invocation that can parse multiple files or a separate symbol import model).
    - Update `aot_run` runner to accept multiple source files and resolve exported symbols.
    - Add tests asserting cross-file exports/imports link and that the final linked object contains expected symbols.
  - Edge-cases: name collisions, circular imports, symbol visibility.

Estimated effort: several days to a couple of weeks depending on design iterations.

---

## Long-term (hard / research + engineering)

- Closures and captured environments
  - Lower closures to boxed environment structs; implement capture analysis and RC semantics for captured boxes.
  - Tests: closure capture tests, lifetimes, and recursive closures.

- Memory management improvements
  - Evaluate and/or implement a tracing GC or hybrid approach to handle cycles (current RC breaks on cycles).
  - Update runtime crate and codegen to cooperate with new GC API.

- Async/await
  - Design lowering to state machines and provide runtime scheduler/event loop in `crates/runtime`.

These large features likely require deeper design, benchmarks, and careful testing (weeks to months).

---

## Concrete next steps (what I'd do next)

1. Add tests described in Short-term (3–5 focused tests): diagnostics stderr capture, more loop variants, member writes. (0.5–1 day.)
2. Add a CI job that installs LLVM-18 and runs `cargo test -p oats` (1 day).
3. Begin medium-term design: sketch `OatsType` extensions and a minimal migration plan so existing tests keep passing (1–2 days design + prototype).

---

## Suggested priority ordering

1. Add more tests (short-term) — prevents regressions and documents behavior.
2. CI with LLVM-18 — prevents platform surprises and ensures inkwell builds in CI.
3. Class layout + module system — enables larger demos and multi-file programs.
4. Type system expansion.
5. Closures / memory / async features.

---

## Quick checklist (quality gates)

- [ ] Build: cargo build -p oats (green)
- [ ] Lint/typecheck: cargo clippy / rustfmt where applicable
- [ ] Unit tests: add tests for diagnostics and member writes
- [ ] Integration: run existing integration tests in `crates/oats/tests/` and new ones
- [ ] CI: add GitHub Action to install LLVM-18 and run tests

---

## Notes / references

- Runtime helpers and names to keep in sync: `malloc`, `free`, `memcpy`, `strlen`, `print_f64`, `print_str`, `array_alloc`, `array_get_f64`, `array_get_ptr`, `array_set_ptr`, `rc_inc`, `rc_dec`.
- See `scripts/setup_env.sh` for how maintainers expect to set `LLVM_SYS_181_PREFIX` and `LD_LIBRARY_PATH` for LLVM-18.
- Existing tests that are good templates: `crates/oats/tests/arrays_and_loops.rs`, `crates/oats/tests/loops_and_fields.rs`, `crates/oats/tests/class_fields.rs`.

---

File created by: automated edit (requested by user) — path: `docs/todo_from_chat.md`

If you'd like I can now:
- Run the full test suite and report failures; or
- Add one of the short-term tests listed above (I can implement one now).