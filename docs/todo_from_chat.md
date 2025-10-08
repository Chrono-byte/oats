
# Oats — remaining TODO (from compiler chat)

This file is a living, prioritized checklist summarizing remaining work and the short/medium/long-term roadmap. It was updated to reflect recent feature work already present in the codebase.

## Summary (high level)

- Short-term: many short items are done — arrays, array literals, computed indexing, `for-of` lowering, runtime helpers, RC helpers, TDZ modeling, and several focused tests are implemented. Remaining short items are small and well-scoped.
- Medium-term: class lowering, dot-member access, and an improved type-system are the next larger efforts.
- Long-term: closures with captured environments, stronger memory management (GC or hybrid), and async/await are still research/large-engineering tasks.

---

## Short-term (implemented / follow-ups)

Implemented (already in repo):

- Arrays & array literals: lowering to runtime-backed arrays and helper calls (`array_alloc`, `array_set_ptr`, ...).
- Computed indexing: `a[expr]` lowering with numeric/pointer coercion and appropriate runtime helper calls.
- for-of lowering: implemented for arrays using runtime layout and element-kind branching.
- Runtime helpers declared and used: `malloc`, `free`, `memcpy`, `strlen`, `print_f64`, `print_str`, `str_concat`, plus array helpers.
- RC helpers wired: `rc_inc` / `rc_dec` are used on stores/returns and for local cleanup.
- TDZ/local model: params/locals are entry `alloca`'d with initialization flags and uninitialized reads trap.
- Expression lowering: binary numeric ops, comparisons, logical short-circuiting, and phi merges with coercion.
- Focused tests: several tests under `crates/oats/tests/` (arrays/loops, for-of, diagnostics) validate lowering paths and emitted IR.

Pending short-term items (small, actionable):

- Member-write lowering (`this.x = ...` / direct field stores): needs AST lowering and a focused test asserting IR uses field-store patterns and RC helpers. Estimated effort: 0.5–1 day.
- More targeted unit tests: add a small set (diagnostic unit tests, member-write IR test, and a couple of loop-edge cases) to lock behavior.
- CI wiring: add a GitHub Action that installs LLVM-18 and runs `cargo test -p oats`. (1 day estimate.)

Note: existing integration tests and scripts (see `scripts/setup_env.sh` and `scripts/run_aot_tempdir.sh`) explain how maintainers run the runner and set `LLVM_SYS_181_PREFIX`.

---

## Medium-term (design + implementation)

Prioritized medium work:

- Dot-member access & nominal struct fields
  - Implement direct `obj.field` lowering and a simple object layout for `NominalStruct`.
  - Add tests that assert correct `getelementptr` offsets / field loads/stores in IR.

- Class lowering and constructors/methods
  - Lower `ClassDecl` into `NominalStruct` with field layout, constructor emission, and method symbols.
  - Ensure `this` receiver handling and RC semantics for fields.

- Type-system expansion
  - Sketch and implement unions/tuples and a plan for generics (erasure vs monomorphization).
  - Update `types.rs`, `map_ts_type`, and coercion rules, and add codegen mappings.

These changes touch parsing, type-mapping, and codegen; expect multiple small integration tests to validate ABI/runtime helper compatibility.

Estimated effort: days → couple of weeks depending on scope and design choices.

---

## Long-term (hard / research + engineering)

- Closures & capture lowering: boxed environments, capture analysis, and RC semantics for captured boxes.
- Memory management: evaluate cycle-handling (tracing GC or hybrid with RC + cycle-collector) and a migration plan for runtime and codegen.
- Async/await: lowering to state machines and a runtime scheduler/event loop in `crates/runtime`.

These are large efforts that require design, benchmarks, and careful testing (weeks → months).

---

## Concrete next steps (recommended, short-to-medium)

1. Implement member-write lowering + unit test (0.5–1 day). This unlocks class-field tests and reduces regressions.
2. Add CI job for LLVM-18 + `cargo test -p oats` (1 day). Use `scripts/setup_env.sh` as guidance for environment setup.
3. Implement dot-member access and a small class-lowering prototype (2–5 days). Add integration tests that inspect emitted IR for field offsets and stores.
4. Sketch `OatsType` extension plan and implement the smallest necessary type-system changes incrementally.

If you want, I can implement step 1 (member-write lowering test) now.

---

## Suggested priority ordering

1. Member-write lowering + tests (short)
2. CI with LLVM-18 (infra)
3. Dot-member access + class lowering (medium)
4. Type system expansion (medium)
5. Closures / memory / async (long)

---

## Quick checklist (status)

- [x] Arrays & array literals — implemented
- [x] Computed indexing — implemented
- [x] for-of lowering — implemented
- [x] Runtime helpers declared/used — implemented
- [x] RC helpers (`rc_inc`/`rc_dec`) — implemented
- [x] TDZ/local model — implemented
- [x] Expression lowering (ops, comparisons, short-circuit) — implemented
- [x] Focused integration tests (arrays/loops/for-of/diagnostics) — present
- [ ] Member-write lowering (pending)
- [ ] Dot-member lowering (pending)
- [ ] Class lowering (pending)
- [ ] Type-system expansion (pending)
- [ ] CI (LLVM-18 job) (pending)

---

## Notes / references

- Runtime helpers and names to keep in sync: `malloc`, `free`, `memcpy`, `strlen`, `print_f64`, `print_str`, `str_concat`, `array_alloc`, `array_get_f64`, `array_get_ptr`, `array_set_ptr`, `rc_inc`, `rc_dec`.
- See `scripts/setup_env.sh` and `scripts/run_aot_tempdir.sh` for local run instructions and expected environment variables (`LLVM_SYS_181_PREFIX`, `LD_LIBRARY_PATH`).
- Tests to use as templates: `crates/oats/tests/arrays_and_loops.rs`, `crates/oats/tests/loops_and_fields.rs`, `crates/oats/tests/class_fields.rs`, `crates/oats/tests/aot_runner_integration.rs`.

---

File updated by: automated edit (requested by user) — path: `docs/todo_from_chat.md`

If you'd like, I can now:
- Run the full test suite and report failures; or
- Implement the short, high-leverage item: member-write lowering + a focused test (I can do that next).

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