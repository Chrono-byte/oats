# Project Roadmap (summary)

**Last Updated:** October 12, 2025

This document tracks short- and mid-term priorities and records recent progress
so the team and contributors can see what to work on next.

## Recent progress (highlights)

- Stabilized `crates/oats/src/codegen/stmt.rs`: fixed malformed edits that
  caused build failures and ensured initializer lowering stores the lowered
  value into pre-allocated locals (with `rc_inc` for pointers).
- Fixed monomorphization for generics: call-site inferred param types are now
  used when creating specialization keys so `getFirstElement(numbers)` and
  `getFirstElement(strings)` get distinct specializations.
- Replaced incorrect `strlen`-based truthiness lowering with header-length loads
  for object/array/string truthiness checks.
- Fixed array-literal lowering and runtime ABI mismatch: pass pointer-to-
  pointer (alloca address) into `array_set_ptr` so runtime can reallocate and
  update caller pointer.
- Implemented several RC fixes: template-literals/number->string lowering now
  emits `rc_dec` for temporaries, binary `+` concat and `println` codepaths
  materialize and decrement temporaries as appropriate.
- Added an IR-level test for the generics example and found/fixed regressions
  discovered by end-to-end tests (including cycle-reclaim).

These changes brought the workspace to a green state for the critical examples
and tests exercised during this session.

## Short-term priorities (next 2–4 weeks)

   - Audit all lowering sites that allocate heap temporaries: template literals,
     string concatenation, `number_to_string`, `union_box_*`,
     `array`/`tuple`/object literal lowering, and `println`/printing helpers.
   - Emit missing `rc_dec` where temporaries are created and ownership is not
     transferred.
   - Add focused tests (IR string-contains or insta snapshots) for each pattern
     to prevent regressions.


2. Address warnings and run lints
   - Clean up any remaining compiler warnings (unused assignments) found in
     `stmt.rs` and other files.
   - Run `cargo clippy --workspace` and fix critical lints.

3. Run fuzzing and integration smoke tests
   - Run `./scripts/run_fuzzing.sh` for fuzz targets and
     `./scripts/run_all_proper_tests.sh` to compile all proper_tests examples and
     catch regressions.

## Mid-term priorities (1–3 months)

- Hardening and optimization of ARC & cycle collector
  - Investigate redundant RC elimination and escape analysis opportunities.
  - Improve the cycle collector's coverage and add stress tests.

- Monomorphization & type-inference hardening
  - Add tests to ensure caching/specialization keys are robust across different
    call-site inference patterns and local variables.

- Documentation and developer ergonomics
  - Update `docs/DEVELOPMENT.md` and `docs/ARCHITECTURE.md` with the
    ownership/RC rules, object layout invariants, and the most important runtime
    ABI contracts (e.g., `array_set_ptr` signature, header layout).

## Release candidate and stabilization

- Once the rc_dec audit, snapshot tests, and lints are green, prepare a
  release-candidate branch that includes:
  - A summary of ownership/RC guarantees and codegen invariants
  - Updated tests and any new snapshots
  - Short smoke-test log showing proper_tests/examples pass

## Long-term / wishlist

- Performance and memory optimization: reduce IR size, improve allocator and
  runtime paths for hot workloads.
- Broader TypeScript coverage: incremental work toward more TS features as the
  type-system mapping and codegen matures.

## Unsupported TypeScript features (short recap)

The compiler intentionally omits or partially supports several advanced
TypeScript features (e.g., decorators, namespaces, ambient `.d.ts`, JSX, full
generators). See `docs/DEVELOPMENT.md` for details and rationale. These are
lower-priority for now; focus is on correctness, ownership/RC, and deterministic
lowering for the supported subset.

## Unsupported / Partially supported JavaScript & TypeScript features (detailed)

The compiler supports a meaningful subset of TypeScript and JavaScript needed
for the project examples and tests. The list below documents language features
that are either not implemented, only partially implemented, or explicitly
rejected by the compiler at parse-time or lower (codegen/runtime). This helps
contributors and users know which constructs to avoid or to test for before
expecting correct output.

- `var` declarations — Rejected at parse time. We only support `let` (and a
  `let mut` variant) for mutable locals; `const` is allowed for compile-time
  consts in limited contexts but user-level `const` is restricted.

- Decorators (class/method/accessor/property/parameter) — Not supported. The
  runtime and codegen do not implement decorator evaluation order, metadata
  wiring, or decorator factories.

- Namespaces / `namespace` (internal modules) — Not supported. The module system
  expects ES-style modules; TS `namespace`/`module` merging semantics are not
  implemented.

- Ambient declarations / `.d.ts` / top-level `declare` — Not consumed by the
  compiler. Type information should be present in-source where needed.

- Type-only imports/exports (`import type` / `export type`) and `import =`
  aliases — Limited support. The resolver and codegen focus on value-level
  imports; purely type-only imports may be ignored or treated as no-ops.

- Enums and `const enum` — Not fully supported. Enums require lowering and
  runtime representation decisions that are not implemented. Prefer numeric
  constants or small nominal wrappers when possible.

- Advanced type-level features — Many complex TypeScript type-system features
  are not reflected into lowering or codegen (they remain in the typechecker
  only): conditional types, recursive `infer` patterns, mapped types used for
  metaprogramming, and intricate intersection/union combinations beyond the
  project's supported union semantics.

- `keyof`, `typeof` (type-level usage), and other reflective type operators —
  Partially supported for basic cases; do not rely on them for complex
  metaprogramming that affects emit-time behavior.

- Declaration merging / module augmentation — Not implemented. The compiler does
  not model the TS rules for merging interfaces, namespaces, or module
  augmentation.

- JSX / TSX — Not supported. There's no JSX transform or runtime integration.

- Generators (`function*` / `yield`) — Partially unsupported. There is no
  generator runtime or lowering for full generator semantics; prefer
  `async`/`await` and explicit state machines where possible.

- `private`/`protected` enforcement and `#private` fields — Syntax may be
  accepted but runtime enforcement of JS private-field semantics is not
  implemented; use nominal/private patterns instead.

- Reflective/dynamic features: `eval`, `with`, `Proxy`, `Function`-constructor,
  dynamic `import()` (depending on usage) — These rely on dynamic runtime
  behavior and are unsupported or only partially supported.

- Some built-in JS runtime APIs and host interactions — While many common
  runtime helpers exist in `crates/runtime`, not every global or Node/DOM API is
  provided. Check `crates/runtime/src/lib.rs` and the runtime docs for the
  supported helper functions and their semantics.
