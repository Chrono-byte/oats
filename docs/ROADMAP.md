## Oats — Roadmap (refreshed)

This document is the canonical roadmap for the Oats ahead-of-time TypeScript to
native compiler. It replaces older planning notes with a concise, actionable

### Short-term (1-3 days)

1. **Finish error handling migration** (incremental)
   - Safety: Small commits, run `cargo build -p oats` after each

| ----------------------------- | --------------- | ---------- | -------- | |
Interfaces & type aliases | 2-3 weeks | Medium | P0 |

- [ ] **90% milestone:** Most npm packages

#### Performance Targets

- [ ] Compilation speed: <100ms per 1000 LOC Oats — Roadmap (concise)

## Goal

Ship a reliable, well-tested AOT TypeScript-to-native compiler that provides
deterministic memory semantics (RC) and a clear migration path to larger
language features.

## Principles

- Small, reviewable changes with tests
- Keep compiler (crates/oats) and runtime (crates/runtime) clearly separated
- Prioritize correctness, diagnostics, and reproducible CI before wide feature
  expansion

## Phases (short)

- Phase 1 — Stabilize & Test (now): CI, snapshots, runtime header/RC tests, fix
  panic sites.
- Phase 2 — Language & Modules (near-term): module resolution, interfaces,
  closures, object literals.
- Phase 3 — Advanced (long): generics, async/await, production hardening.

## Top work items (pick one to implement now)

1. CI + reproducible build (high) — Add GitHub Actions that run
   `cargo build --workspace`, `cargo test --workspace`, and `cargo clippy`.
2. Runtime tests (high) — Add unit tests under `crates/runtime/tests` that
   assert header layout, rc_inc/rc_dec behavior, and static-string semantics.
3. Snapshot coverage (high) — Add/extend `insta` snapshots for representative
   codegen IR and document snapshot-update policy.

## Acceptance & quality gates

- `cargo test --workspace` must pass on CI
- Snapshots updated only with PR explanation
- Any change touching header layout or field offsets must include runtime tests

## Short checklist for contributors

- Run `source ./scripts/setup_env.sh` (if needed) and `cargo test -p oats`
- No new `.unwrap()`/`.expect()` in `crates/oats/src`
- If IR changes: update insta snapshot + add comment in PR

## Where to look in the repo

- Compiler: `crates/oats/src` (parser.rs, types.rs, codegen/)
- Runtime: `crates/runtime/src` (header, rc helpers)
- Tests & examples: `crates/oats/tests`, `crates/runtime/tests`, `examples/`

## Immediate options — tell me which to implement and I'll do it + run tests

- Add CI workflow (GitHub Actions)
- Add runtime unit tests (header, rc_inc/rc_dec)
- Add a short CONTRIBUTING.md that documents the PR checklist and snapshot
  policy

Pick one and I'll implement it, run the tests, and report back with results.

- Release process, ABI stability guidance, and performance baselines.
