# Oats 🌾

[![License](https://img.shields.io/badge/license-zlib-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/built%20with-Rust-000000.svg?logo=rust)](https://www.rust-lang.org/)
[![LLVM](https://img.shields.io/badge/powered%20by-LLVM%2018-262D3A.svg)](https://llvm.org/)

> An Ahead-of-Time (AOT) Compiler for a TypeScript-Inspired Language

Oats is an experimental ahead-of-time (AOT) compiler that converts a small,
well-defined subset of TypeScript into native machine code using LLVM. It
generates standalone executables and uses reference counting for predictable,
manual memory management.

---

## ✨ Features

- **TypeScript Subset**: Numbers, booleans, strings, arrays, classes, and unions
  lowered through `OatsType`.
- **Generics & Unions**: Call-site monomorphization with boxed union support.
- **Async (preview)**: Async functions lower into poll-state machines with
  resume blocks.
- **Memory Management**: Deterministic ARC with weak references and cycle
  collector scaffolding.
- **LLVM Integration**: Emits LLVM 18 IR and links against the Oats runtime to
  produce native executables.

---

## 🚀 Getting Started

### Prerequisites

- **Rust Toolchain**: Install via [rustup](https://rustup.rs/).
- **LLVM 18**: Install development headers and tools for your platform.
- **clang**: Required for linking (typically comes with LLVM).

#### Installing LLVM 18

**Fedora**:

```bash
sudo dnf install llvm18 llvm18-devel clang18
```

**Ubuntu/Debian**:

```bash
sudo apt update
sudo apt install llvm-18 llvm-18-dev clang-18
```

**macOS** (with Homebrew):

```bash
brew install llvm@18
```

<!-- add instructions for other platforms here! -->

### Standalone Toasty (Recommended)

Toasty can work standalone by automatically fetching pre-built runtime libraries from GitHub releases:

1. **Install toasty**:
   ```bash
   cargo install --git https://github.com/Chrono-byte/oats toasty
   ```

2. **Compile and run programs**:
   ```bash
   # Create a simple program
   echo 'export function main(): number { return 5 + 3; }' > add.oats
   
   # Build it (automatically fetches runtime if not cached)
   toasty build add.oats
   
   # Or build and run in one step
   toasty run add.oats
   ```

The runtime is automatically cached in `~/.cache/oats/runtime/` for faster subsequent builds.

**Environment variables**:
- `OATS_NO_REMOTE_RUNTIME`: Force local runtime build instead of fetching
- `OATS_RUNTIME_CACHE`: Override the cache directory

### Full Repository Setup (For Development)

If you want to develop Oats or work with the full repository:

### Compile Your First Program

1. **Clone the Repository**:

   ```bash
   git clone https://github.com/Chrono-byte/oats.git
   cd oats
   ```

2. **Oats Example**: Review or edit `examples/add.oats` for your first build.

3. **Compile and Run**:

```bash
# Prepare environment (sets LLVM paths)
source ./scripts/setup_env.sh

# Build in release mode for better performance
cargo build --workspace --release

# Run the `toasty` CLI in release mode to compile and run the example
cargo run -p toasty --release -- run examples/add.oats

# The compiled executable will be written to `./aot_out/add`
./aot_out/add
```

---

## 🔒 Platform Safeguards

- **Checked Allocation Sizes**: Runtime helpers use guarded arithmetic for all
  heap sizing.
- **Resource Limits**: Configure `OATS_MAX_HEAP_BYTES` and
  `OATS_MAX_ALLOC_BYTES` to cap total and per-allocation usage.
- **Fuzzing Infrastructure**: `cargo +nightly fuzz run fuzz_parser` hardens the
  parser against malformed input.

---

## 📁 Project Structure

- **`crates/oatsc`**: Core compiler for parsing, type checking, and LLVM IR
  generation. Includes runtime fetcher for standalone operation.
- **`crates/runtime`**: ARC helpers, allocators, logging, and cycle collector.
  Built as a static library and published via GitHub Actions.
- **`crates/toasty`**: CLI wrapper for the compiler with build/run commands.
- **`docs/`**: Architecture, development, memory, and roadmap references.
- **`examples/`**: Programs compiled as part of the test suites.
- **`.github/workflows/`**: CI/CD workflows including automated runtime builds.

See `docs/README.md` for a guided tour of the documentation set.

---

## 🤝 Contributing

Contributions are welcome! Start by exploring the `docs/` directory:

- **`ROADMAP.md`**: Long-term vision and feature plans.

### Development Workflow

1. **Set Up Environment**:

    ```bash
    source ./scripts/setup_env.sh
    ```

2. **Build and Test**:

   ```bash
   cargo build --workspace
   cargo test --workspace
   ```

---

## 📄 License

Licensed under the **zlib/libpng License**. See the [LICENSE](LICENSE) file for
details.
