# Oats AOT prototype

This repository contains a minimal prototype AOT compiler pipeline for "Oats" using `deno_ast` for parsing and `inkwell` for IR generation (LLVM 18).

Quick start (local)

1. Source the environment setup (POSIX):

```sh
. ./scripts/setup_env.sh
```

Or for Zsh users:

```zsh
source ./scripts/setup_env.zsh
```

2. Build & run the AOT runner in a temporary directory:

```sh
./scripts/run_aot_tempdir.sh    # deletes artifacts after run
./scripts/run_aot_tempdir.sh --keep  # keeps artifacts
```

3. Or use the helper from the zsh script after sourcing:

```zsh
oats_build
oats_run_aot
```

Notes and CI

- The project requires LLVM 18 installed on the system. The scripts try to auto-detect common prefixes such as `/usr/lib64/llvm18`. If auto-detection fails, set `LLVM_SYS_181_PREFIX` manually.
- For CI, ensure the runner installs LLVM 18 and sets `LLVM_SYS_181_PREFIX` in the environment before running `cargo build` or tests.

Design choices

- Currently the project emits IR using `inkwell` and compiles IR with `clang`. The runtime helpers (strings) are provided by a Rust staticlib in `crates/runtime` and linked in the AOT pipeline.
- Next improvements: emit libc-only helpers in the IR so no in-repo runtime is required (see TODO list).
# Oats (AOT compiler prototype)

This repository contains a minimal Rust prototype for the Oats AOT compiler. It uses `deno_ast` for parsing TypeScript-like syntax and `inkwell` for LLVM IR generation.

Files:
- `src/parser.rs` — parsing helper using `deno_ast::parse_module`
- `src/types.rs` — `OatsType`, `SymbolTable`, and a strictness checker
- `src/codegen.rs` — minimal `CodeGen` with type mapping and a function IR generator
- `src/main.rs` — orchestrator that parses a sample source, runs the checker, and prints LLVM IR

Try it:

```bash
cd /home/ellie/Dev/oats
cargo run
```

Notes about LLVM / inkwell:

- `inkwell` is a Rust wrapper over LLVM. Building it requires a system LLVM installation with `llvm-config` available on PATH.
- On Debian/Ubuntu you can install a recent LLVM (for example 18) with:

```bash
sudo apt install llvm-18-dev llvm-18
sudo ln -sfn /usr/bin/llvm-config-18 /usr/bin/llvm-config
```

- On macOS with Homebrew:

```bash
brew install llvm
export PATH="$(brew --prefix llvm)/bin:$PATH"
```

- After installing LLVM, re-run:

```bash
cargo build
cargo run
```

If you don't have LLVM installed locally, the repository will still contain the source scaffold, but the build will fail when compiling `inkwell` until `llvm-config` and the matching LLVM libraries are available.
