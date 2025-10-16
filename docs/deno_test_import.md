# Deno Test Import Plan

This document tracks the subset of `deno/tests` that map cleanly onto the Oats
compiler test harness and outlines how to vendor them.

## Source Repository

- Repo: <https://github.com/denoland/deno>
- License: MIT / Apache-2.0 (retain headers when vendoring)
- Baseline commit: _TBD_ (capture the exact SHA when we first import)

## Target Layout

```
third_party/
  deno_tests/
    README.md        # license notice + update instructions
    unit/            # mirrored subtrees from deno/tests/unit
    spec/            # mirrored subtrees from deno/tests/spec
    typescript/      # mirrored subtrees from deno/tests/typescript
```

The tests stay in TypeScript form so we can run them through the normal
parsing/type-check/codegen pipeline. Any runtime expectations are rewritten as
part of the harness layer instead of editing the imported files.

## Curated Manifest

| Path (under `deno/tests`)        | Status   | Notes |
|----------------------------------|----------|-------|
| `unit/`                          | Partial  | Keep pure language/TS semantics files. Skip anything importing `Deno.*`, `http`, workers, permissions, or filesystem APIs. |
| `spec/`                          | Partial  | Focus on ECMAScript conformance cases that run in isolation. `spec/**/host_binding` and similar runtime-heavy cases stay excluded. |
| `typescript/`                    | Partial  | Prefer compiler regression tests that only assert type errors. Exclude cases that expect Deno namespaces or Node compatibility types. |
| `runtime/`                       | Exclude  | Hard-wired to Deno's runtime APIs. |
| `worker/`                        | Exclude  | Requires multi-threading support and worker harness. |
| `ffi/`                           | Exclude  | Depends on Deno's FFI implementation. |
| `bench/`                         | Exclude  | Not part of the correctness test suite. |
| `integration/`                   | Exclude  | Relies on the full CLI behavior. |
| `testdata/`                      | Exclude  | Support files only reachable from the excluded suites. |

We will maintain a concrete allow-list file (see next section) so that CI pulls a
stable set of paths.

## Vendoring Script

Add `scripts/vendor_deno_tests.sh` with the following responsibilities:

1. Accept a commit SHA argument (or default to the pinned SHA in the allow-list).
2. Perform a sparse checkout of `deno/tests` at that revision.
3. Copy the allow-listed files into `third_party/deno_tests` while preserving
   relative paths.
4. Drop `.md` harness docs unless referenced by the imported tests.
5. Rewrite Windows line endings to Unix to stabilize diffs.
6. Update `third_party/deno_tests/README.md` with the new SHA.

## Harness Adapter

- Implement lightweight assertion shims in `crates/oats/tests/common/deno_adapter.rs`
  (see stubbed module).
- Provide utilities to run a vendored `.ts` file through the existing Oats test
  harness, e.g. `run_deno_fixture("unit/array_filter.ts")` that:
  1. Reads the source from `third_party/deno_tests`.
  2. Wraps it in an `export function main(): void { ... }` shell if the file is
     statement-only.
  3. Injects helper bindings (`assert`, `assertEquals`, etc.) using the adapter
     module so the original assertions work unchanged.
  4. Emits skip notices if the test references unsupported runtime APIs.

## Tracking Allow-List

Create `third_party/deno_tests/allowlist.txt` (one path per line) that the
vendoring script consumes. Example:

```
unit/async_iteration.ts
unit/array_filter.ts
spec/url/basic.ts
typescript/decorators/emit_metadata.ts
```

When we add new fixtures we update this file, rerun the script, and commit both
changes.

## Follow-Up Tasks

- [ ] Implement the vendoring script with sparse checkout support.
- [ ] Populate `third_party/deno_tests/allowlist.txt` with an initial slice of
      language-level tests.
- [ ] Add a `deno_compat` module under `crates/oats/tests/` that loads each
      vendored file as a regression test (grouped by directory).
- [ ] Document the refresh workflow in `docs/TESTING.md` once the harness is in place.
