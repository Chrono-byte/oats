# Oats — quick agent guide (root copy)

This is a compact, actionable copy of the repository guidance for AI assistants and contributors. You can move this into `.github/` or keep it here for quick edits.

## Big picture
- `crates/oats` — compiler front-end and IR emission (parsing, type checking, codegen using `inkwell`). See `crates/oats/src/*`.
- `crates/runtime` — runtime helpers compiled as a staticlib and linked into AOT outputs.
- `examples/` — sample `.oats` sources for quick tests.
- `scripts/` — helpers for building & running (e.g. `setup_env.sh`, `run_aot_tempdir.sh`).

Key integration points: `deno_ast` for parsing; `inkwell` + LLVM 18 for IR; emitted IR is compiled/linked with system toolchain and `crates/runtime`.

## Files to read first
- `crates/oats/src/main.rs` — runner: parse, find exported `main`, typecheck, emit IR (`oats_main`).
- `crates/oats/src/parser.rs`, `types.rs`, `codegen/` — core parse/check/codegen.
- `crates/oats/src/diagnostics.rs` — diagnostic helpers (span-based, byte offsets).
- `scripts/setup_env.sh` / `scripts/setup_env.zsh` — how LLVM 18 is detected and env vars set.
- `scripts/run_aot_tempdir.sh` — example build/run workflow.

## How to build/run (quick)
- Dev helper (Linux):

  . ./scripts/setup_env.sh
  ./scripts/run_aot_tempdir.sh

- Direct run (explicit):

  cargo build -p oats
  LLVM_SYS_181_PREFIX="/path/to/llvm18" OATS_SRC_FILE="examples/add.oats" OATS_OUT_DIR="./aot_out" \
    cargo run -p oats --bin aot_run -- examples/add.oats

- Simple recommended test workflow (no env sourcing usually required):

  cd /home/ellie/Dev/oats && OATS_OUT_DIR=./test_out cargo run -p oats --bin aot_run -- examples/YOURFILE.oats
  ./test_out/YOURFILE

- CI: LLVM 18 must be installed and `LLVM_SYS_181_PREFIX` set before `cargo build`.

## Heap object system (CRITICAL)
The runtime uses a unified 64-bit header for heap objects. This is central for correct memory/RC behavior. Keep these rules in your head when editing codegen or runtime.

Header layout (8 bytes at offset 0):
- Bits 0-31: Reference count (atomic, u32)
- Bit 32: Static flag (1 = immortal/don't modify RC, 0 = heap-allocated)
- Bits 33-63: Reserved for future flags/type tags

Constants: `crates/runtime/src/lib.rs` defines the masks and static bit: `HEADER_RC_MASK`, `HEADER_STATIC_BIT`, `HEADER_FLAGS_MASK`.

Object layouts (codegen expectations):
- Static string literals (in `.rodata`): layout = [header (static bit set)][length i64][data bytes + NUL]. Codegen/runtime return a pointer to the data field (offset 16).
- Heap strings: runtime allocates header (RC=1) + length + data; runtime returns pointer to data (offset 16).
- Arrays: layout starts at base (offset 0): [header][length i64][elements...]. Allocators return the base pointer.
- Classes/objects: layout starts at base (offset 0): [header][field0 (8)][field1 (8)]... Constructors return the base pointer.

Pointer rules & RC operations:
- The system accepts two pointer kinds: object base pointers (offset 0) and string data pointers (offset 16). All RC helpers accept either.
- Runtime provides `get_object_base(p)` which heuristically tests header validity at offset 0 and p-16 to find the real base for RC ops.
- `rc_inc` / `rc_dec`:
  - Resolve base via `get_object_base`.
  - If static bit set, skip mutation.
  - Atomically modify low-32-bit RC.
  - `rc_dec` calls destructor and frees when RC reaches 0.

Codegen must follow these rules:
1. Emit static string literals with the static bit set; return data pointer (offset 16).
2. Heap strings/arrays/classes are allocated with RC=1 by runtime helpers; codegen should use those helpers.
3. When storing pointers into locals or object fields, call `rc_inc`.
4. Before returning from a function, emit RC decrements for locals (there is a helper path for this).
5. Passing any pointer to `rc_inc` / `rc_dec` is safe; the runtime resolves data vs base.

## Common pitfalls & gotchas
- `inkwell` is pinned to LLVM 18 (feature `llvm18-1`). Mismatched `llvm-config` or libs will cause build failures.
- `diagnostics::report_error_span` expects 0-based byte offsets.
- Codegen stores runtime function hooks (malloc, free, print helpers) on `CodeGen`; if you rename runtime helpers, update both crates.
- Constructors must initialize parameter-property fields (e.g., `constructor(public name: string)`) — see `gen_constructor_ir` in codegen for the pattern.

## Short development notes (from docs/DEVELOPMENT.md)
- Testing: use `cargo test -p oats` and the example scripts; add regression tests when touching lowering logic.

## Roadmap highlights (short)
- Phase 0 (current): Automatic reference counting, deterministic destruction, core language features (classes, arrays, loops, unary ops).
- Phase 1 (short-term): Arrow functions (non-capturing), module resolution & multi-file compilation, object literals, union types, basic stdlib (console, Math, Array methods).
- Phase 2 (medium): Closures with capture, generics (monomorphization), try/catch, destructuring, tuples, spread/rest.
- Phase 3 (long-term): Full TS type-system features, module ecosystem/`node_modules`, performance optimizations, async/await state-machine.

## Quick checklist for code changes
- If adding a pointer field or storing pointers: update RC calls (rc_inc on store, rc_dec on drop).
- When adding runtime helpers: update both `crates/runtime` and codegen registration in `CodeGen`.
- When changing lowering to return `Result`: update callers to propagate or emit diagnostics and add tests.

## Where to add further guidance
- If you expand this document, prefer short bullet points and link back to the canonical docs in `docs/` (ARCHITECTURE.md, DEVELOPMENT.md, ROADMAP.md).