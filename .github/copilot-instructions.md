## Oats — quick agent guide

This repository is a small Rust prototype AOT compiler (workspace with `crates/oats` and `crates/runtime`). The goal of these notes is to help an AI coding agent become productive quickly by highlighting the project's structure, key files, workflows, and gotchas.

### Big picture
- `crates/oats` — the compiler front-end and IR emission (parsing, type checking, codegen using `inkwell`). See `crates/oats/src/*`.
- `crates/runtime` — helpers compiled as a staticlib and linked into the AOT output.
- Top-level `examples/` contains sample `.oats` sources (e.g. `examples/add.oats`).
- Build/run helpers live in `scripts/` (`setup_env.sh`, `run_aot_tempdir.sh`, `setup_env.zsh`).

Key integration points: `deno_ast` is used for parsing; `inkwell` + LLVM 18 for IR; emitted IR is compiled with `clang`/system toolchain and linked with `crates/runtime` artifacts.

### Files to read first (quick tour)
- `crates/oats/src/main.rs` — runner that parses a source, finds an exported `main`, typechecks, and emits IR (it generates `oats_main`).
- `crates/oats/src/parser.rs`, `types.rs`, `codegen.rs` — parse/check/codegen core.
- `crates/oats/src/diagnostics.rs` — diagnostic formatting helpers (simple rustc-like messages; span helpers take byte indices).
- `scripts/setup_env.sh` — how LLVM 18 is detected and `LLVM_SYS_181_PREFIX` / `LD_LIBRARY_PATH` are set when you source the script.
- `scripts/run_aot_tempdir.sh` — example workflow that builds and runs the AOT runner with `OATS_SRC_FILE` and `OATS_OUT_DIR` env vars.

### Important workflows & commands
- Local dev (POSIX): source env then run the helper

  . ./scripts/setup_env.sh
  ./scripts/run_aot_tempdir.sh    # writes artifacts to ./aot_out

- Direct run (explicit):

  cargo build -p oats
  LLVM_SYS_181_PREFIX="/path/to/llvm18" OATS_SRC_FILE="examples/add.oats" OATS_OUT_DIR="./aot_out" \
    cargo run -p oats --bin aot_run -- examples/add.oats

- CI requirement: ensure LLVM 18 is installed and `LLVM_SYS_181_PREFIX` is set before `cargo build`.

### Project-specific conventions & patterns
- The user script must `export` a `main` function in the AST; the runner finds it and the generator emits a symbol named `oats_main` to avoid collision with the C runtime. When modifying function emission, keep this mapping in mind.
- `diagnostics::report_error_span` expects byte offsets into the source (0-based). Use it for span-aware errors.
- Codegen stores runtime function hooks (malloc, free, print helpers) as `RefCell<Option<_>>` fields on `CodeGen`. If you add or rename runtime helpers, update both `codegen.rs` and the runtime crate.
- `inkwell` is pinned to LLVM 18 (feature `llvm18-1`). Builds will fail without a matching `llvm-config`/libs.

### Testing & targets
- Unit tests exist under `tests/` (Rust test files). Run `cargo test` in the workspace root to run all tests.
- When editing codegen or runtime linkage, add a small integration test that runs `aot_run` on a short example and verifies the produced artifacts or IR contains expected symbols.

### Common pitfalls for agents
- Do not assume LLVM is available; prefer adding guards or clear error messages and reference `scripts/setup_env.sh` for how maintainers expect env vars to be set.
- When manipulating source spans, prefer using byte indices consistently (the repo's diagnostics use bytes, not codepoints).
- Keep changes localized: many inkwell types and the `CodeGen` API are shared; renames in `codegen.rs` commonly require updates in `main.rs` test runner and runtime glue.

If anything here is ambiguous or you'd like more examples (small test to run, or the typical CI config), tell me which area to expand and I'll iterate.
### Quick IR example (what to expect)
If you run the aot runner on `examples/add.oats` the generator emits an internal function named `oats_main`. The IR will contain a function signature similar to:

    define double @oats_main(double %arg0, double %arg1) {
      ; ... fadd or other instructions ...
    }

You can generate and inspect the IR locally by running:

```sh
. ./scripts/setup_env.sh
cargo run -p oats --bin aot_run -- examples/add.oats > out.ll
sed -n '1,120p' out.ll
```

### CI / environment notes
- This project requires LLVM 18 for `inkwell`/`llvm-sys`. In CI you must install a matching LLVM and set `LLVM_SYS_181_PREFIX` to the LLVM prefix before invoking `cargo build` or `cargo test`.
- Example GitHub Actions fragment (Ubuntu) you can adapt for the repo:

```yaml
- name: Install LLVM-18
  run: |
    sudo apt-get update
    sudo apt-get install -y llvm-18 llvm-18-dev clang-18
    echo "LLVM_SYS_181_PREFIX=/usr/lib/llvm-18" >> $GITHUB_ENV
    echo "/usr/lib/llvm-18/lib" >> $GITHUB_PATH
```

### Debugging common build/runtime issues
- inkell/llvm errors: when `inkwell` fails to build, confirm `llvm-config` for LLVM 18 is on PATH and `LLVM_SYS_181_PREFIX` points to the correct prefix. The project's `scripts/setup_env.sh` shows how maintainers expect to set this.
- Undefined symbols when linking AOT outputs: check that `crates/runtime` exposes the runtime helpers (malloc/free/print). The `CodeGen` struct in `crates/oats/src/codegen.rs` stores these as `RefCell<Option<FunctionType>>` fields; ensure names and prototypes match the runtime crate.
- Diagnostic spans look wrong: `diagnostics::report_error_span` expects byte offsets (0-based) into the original source; use the parser's span byte offsets rather than character counts.

### Where to change runtime hooks
- `crates/oats/src/codegen.rs` — look for fields on `CodeGen`: `fn_malloc`, `fn_free`, `fn_print_str`, `fn_print_f64`, `fn_memcpy`, `fn_strlen`.
- `crates/runtime/src/lib.rs` — runtime implementations and exported symbol names. If you rename or change prototypes, update both places and add a small test that links the emitted object with `libruntime.a`.

### Minimal test pattern for future changes
- For modifications to codegen or runtime linkage, add a crate-level test under `crates/oats/tests/` that parses `examples/add.oats`, generates IR, and asserts the IR contains expected symbols (we added `aot_runner_integration.rs` as an example).

---

If you'd like, I can also:

- Add the generated IR sample produced by the test into this doc.
- Add a GitHub Actions workflow file that installs LLVM-18 and runs `cargo test`.

Tell me which and I'll add it.

### Roadmap — current status (updated)
This section replaces the previous TODO list with an accurate snapshot of what the compiler already supports and what remains. It highlights concrete next steps you can pick up.

What is implemented (key items)
- Arrays and array literals: array literal lowering and calls into a small set of runtime helpers are implemented; `array_alloc`, `array_set_ptr`, `array_get_ptr`, and `array_get_f64` are declared/used by the codegen (see `crates/oats/src/codegen/mod.rs`).
- Computed indexing (bracket form): `MemberExpr` with computed property (e.g. `a[2]`) is lowered; integer and float indices are coerced to i64 and routed to `array_get_f64` / `array_get_ptr` as appropriate.
- Runtime helpers and libc declarations: `malloc`, `free`, `memcpy`, `strlen`, `print_f64`, `print_str`, `str_concat` are declared so emitted IR links to `crates/runtime` or system libc.
- Reference-count helpers: `rc_inc` and `rc_dec` helpers are declared and used — parameters and pointer locals get `rc_inc` on store and `emit_rc_dec_for_locals` is called on returns to drop locals.
- TDZ and locals model: function params/locals are lowered to entry allocas with initialization flags; uninitialized reads emit traps/`unreachable` to model TDZ semantics.
- Expression lowering features: binary numeric ops, comparisons, logical short-circuiting, phi merges with coercion (float/int/pointer) are implemented.
- Tests: `crates/oats/tests/arrays_and_loops.rs` exists and asserts the emitted IR contains array helper calls; other test harnesses and integration tests are present under `crates/oats/tests/`.

What is not yet implemented / remaining high-value work
- Loops: C-style loops (`for`, `while`, `do/while`) have been implemented in `crates/oats/src/codegen/mod.rs` including basic handling for `break` and `continue` with correct RC decref of inner scopes. `for-of` lowering has been implemented; remaining loop work focuses on labeled `break`/`continue` handling and more advanced loop patterns.
- Dot property/member access: computed `obj[expr]` is supported; direct dot-member lowering (e.g. `obj.field`) and object layout/fields are not yet implemented.
 - Type system extensions: `TsArrayType` mapping has been implemented in `crates/oats/src/types.rs` and the codegen now treats arrays as opaque runtime pointers (mapped to `i8*` in LLVM). Unions, tuples, generics, interfaces, and type aliases are still not supported.
- Classes/constructors/methods: `ClassDecl` lowering is not present. `OatsType::NominalStruct` exists and is a natural landing point, but field layout and method dispatch need work.
- Module system (imports/exports): the runner collects and emits exported functions but a full import/link model for modules (separate files, default exports) is not implemented.
- Closures and advanced function features: arrow functions, default/optional/rest params, destructured params and closure capture/boxing are not implemented.
- Async/await & Promises: not supported — would require lowering to a state machine or runtime.

Concrete next steps (short / medium / long)
- Short (high-impact, small scope):
  - (DONE) `for-of` lowering implemented (iterate arrays using runtime layout and helpers); support for labeled `break`/`continue` remains.
  - Add tests that assert `for`/`while`/`do-while` lowering (including `break`/`continue`) emit expected basic blocks and that `for-of` uses array helpers correctly.
  - (DONE) `TsArrayType` mapping added in `crates/oats/src/types.rs`; array types are represented as `i8*` at the LLVM level. Ensure array literal type inference/annotations are supported for simple cases as a follow-up.
  - Add dot-member lowering for nominal structs (map field names to offsets) and a small runtime layout helper if needed.

- Medium (design+implementation):
  - Module import/export model: design how multi-file modules are resolved, how symbols are emitted/linked, and update `crates/oats/src/bin/aot_run.rs` to support multi-file builds or import resolution.
  - Basic classes: lower `ClassDecl` into `NominalStruct` layouts, emit constructors and method symbols, and wire tests.

- Long-term (harder):
  - Closures with captured environments, generics (monomorphization or erasure), and async/await.
  - Consider a GC or different ownership model if you plan heavy heap/object features; current RC approach is ok for small demos but has limits.

Files to edit / mappings
- `crates/oats/src/types.rs` — add `TsArrayType` mapping, extend `map_ts_type` for arrays/tuples/unions and represent basic field layouts for nominal structs.
- `crates/oats/src/codegen/mod.rs` — implement loop lowering, extend member lowering for dot-access, and refine array literal lowering if needed.
- `crates/oats/src/codegen/helpers.rs` — extend helpers for element-size / element-type selection and any small layout helpers.
- `crates/runtime/src/lib.rs` — confirm or add runtime helpers (`array_alloc`, `array_get_ptr`, `array_get_f64`, `array_set_ptr`, `rc_inc`, `rc_dec`, `str_concat`, `print_f64`, `print_str`) and keep signatures in sync with codegen.

Notes and quick pointers
- The workspace already contains `crates/oats/tests/arrays_and_loops.rs` which validates many array-paths in IR; use it as a starting integration test when changing array/rc/strlen behavior.
- `codegen::gen_str_concat` currently only declares `str_concat` (runtime provides the definition) to avoid duplicate-symbol issues when linking with the runtime staticlib.
- When adding features that change emitted runtime symbols, update both `codegen/mod.rs` (declarations/usages) and `crates/runtime/src/lib.rs` (definitions) and add a small test that compiles and links the emitted object with `libruntime.a`.

Verification performed
- Implemented `TsArrayType` mapping and lightweight codegen mapping for arrays (arrays -> `i8*`).
- Implemented `for-of` lowering (iterates runtime arrays and handles numeric vs pointer element kinds with proper rc_inc/rc_dec behavior).
- Ran `cargo test -p oats` locally after changes; all tests passed.

If you want, I can implement the short/low-effort items (loops + TsArrayType mapping + dot-member lowering) and wire tests now — tell me which short item to start with and I'll update code and tests, run the relevant unit tests, and iterate until green.
