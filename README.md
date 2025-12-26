# Oats

[![License](https://img.shields.io/badge/license-zlib-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/built%20with-Rust-000000.svg?logo=rust)](https://www.rust-lang.org/)
[![LLVM](https://img.shields.io/badge/powered%20by-LLVM%2018-262D3A.svg)](https://llvm.org/)

Compiles TypeScript/JavaScript to native code using LLVM. Still experimental, but it works for basic stuff. Supports multi-file projects, generates standalone executables, and uses reference counting for memory management.

## Features

- Basic types: numbers, booleans, strings, arrays, classes, unions
- Multi-file compilation with a package system
- Generics (via monomorphization) and union types
- Async functions (still in preview, but they work)
- Reference counting with weak refs and cycle collection
- Outputs LLVM IR and links to a custom runtime

## Getting Started

### Prerequisites

You'll need:

- Rust (install via [rustup](https://rustup.rs/))
- LLVM 18 dev headers and tools
- clang (usually comes with LLVM)

Installing LLVM 18:

**Fedora**:

```bash
sudo dnf install llvm18 llvm18-devel clang18
```

**Arch Linux**:

```bash
sudo pacman -S llvm18 clang
```

### Quick Start (Recommended)

The easiest way is to use `toasty` standalone.

Install it:

```bash
cargo install --git https://gitlab.com/Chrono-byte/oats toasty
```

Then compile and run:

```bash
# Create a simple program
echo 'export function main(): void { println(5 + 3); }' > add.ts

# Build it
toasty build add.ts

# Or build and run
toasty run add.ts
```

The runtime gets cached in `~/.ts/runtime/` so subsequent builds are faster.

Environment variables:

- `OATS_RUNTIME_CACHE`: Change where the runtime cache goes

### Building from Source

If you want to hack on Oats:

```bash
git clone https://gitlab.com/Chrono-byte/oats.git
cd oats

# Build everything
cargo build --workspace --release

# Run an example
cargo run -p toasty --release -- run examples/add.ts
```

## Project Structure

- `crates/oatsc`: The actual compiler (parsing, type checking, LLVM IR generation)
- `crates/runtime`: Runtime library with ARC, allocators, logging, cycle collector.
- `crates/toasty`: CLI tool with `build` and `run` commands
- `crates/std`: Standard library (console, filesystem, networking, etc.)
- `docs/`: Documentation (architecture, dev guides, memory design, roadmap)
- `examples/`: Example programs and test suites
- `scripts/`: Helper scripts for dev/testing

Check out `docs/README.md` for more detailed docs.

## Contributing

Contributions welcome. Check out `docs/ROADMAP.md` to see what's planned.

To build and test:

```bash
# Build everything
cargo build --workspace

# Run tests
cargo test --workspace

# Run the end-to-end tests
# there is a test suite member called e2e_tests.rs that runs the end-to-end tests
```

## License

Licensed under the **zlib/libpng License with Acknowledgement**. See the
[LICENSE](LICENSE) file for details.
