# Oats 🌾

[![License](https://img.shields.io/badge/license-zlib-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/built%20with-Rust-000000.svg?logo=rust)](https://www.rust-lang.org/)
[![LLVM](https://img.shields.io/badge/powered%20by-LLVM%2018-262D3A.svg)](https://llvm.org/)

> An Ahead-of-Time (AOT) Compiler for a TypeScript-Inspired Language

Oats is an experimental ahead-of-time (AOT) compiler that converts a small, well-defined subset of TypeScript into native machine code using LLVM. It supports multi-file compilation with a package system for dependency management, generates standalone executables, and uses deterministic reference counting for predictable memory management.

-----

## ✨ Features

- **TypeScript Subset**: Compiles a rich subset of TypeScript including numbers, booleans, strings, arrays, classes, and unions.
- **Package System**: Supports multi-file compilation with dependency resolution, allowing complex applications to be built from modular components.
- **Generics & Unions**: Supports generics via call-site monomorphization and handles complex union types with runtime boxing.
- **Async (preview)**: `async` functions are lowered into efficient poll-state machines with resume blocks, enabling asynchronous operations.
- **Memory Management**: Implements deterministic Automatic Reference Counting (ARC) with support for weak references and a background cycle collector.
- **LLVM Integration**: Emits optimized LLVM 18 IR and links against a custom safe runtime to produce native executables.

-----

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

### Standalone Toasty (Recommended)

`toasty` can work as a standalone tool by automatically fetching pre-built runtime libraries from GitHub releases.

1. **Install `toasty`**:

    ```bash
    cargo install --git https://github.com/Chrono-byte/oats toasty
    ```

2. **Compile and run programs**:

    ```bash
    # Create a simple program
    echo 'export function main(): void { println(5 + 3); }' > add.oats

    # Build it (automatically fetches runtime if not cached)
    toasty build add.oats

    # Or build and run in one step
    toasty run add.oats
    ```

The runtime is automatically cached in `~/.cache/oats/runtime/` for faster subsequent builds.

**Environment variables**:

- `OATS_NO_REMOTE_RUNTIME`: Force local runtime build instead of fetching.
- `OATS_RUNTIME_CACHE`: Override the cache directory.

### Full Repository Setup (For Development)

If you want to develop Oats or work with the full repository:

1. **Clone the Repository**:

    ```bash
    git clone https://github.com/Chrono-byte/oats.git
    cd oats
    ```

2. **Compile and Run**:

    ```bash
    # Build in release mode for better performance
    cargo build --workspace --release

    # Run the `toasty` CLI to compile and execute the example
    cargo run -p toasty --release -- run examples/add.oats
    ```

-----

## 🛡️ Platform Safeguards

- **Checked Allocation Sizes**: Runtime helpers use guarded arithmetic for all heap sizing to prevent overflows.
- **Resource Limits**: Configure `OATS_MAX_HEAP_BYTES` and `OATS_MAX_ALLOC_BYTES` to cap total and per-allocation memory usage.
- **Fuzzing Infrastructure**: The project includes a fuzzing suite (`cargo +nightly fuzz run fuzz_parser`) to harden the parser against malformed input.

-----

## 📁 Project Structure

- **`crates/oatsc`**: Core compiler for parsing, type checking, and LLVM IR generation. Includes the runtime fetcher for standalone operation.
- **`crates/runtime`**: The safe runtime library providing ARC helpers, allocators, logging, and the cycle collector. Built as a static library and published via GitHub Actions.
- **`crates/toasty`**: The CLI wrapper for the compiler with `build` and `run` commands, including package-based compilation support.
- **`docs/`**: Comprehensive documentation including architecture, development guides, memory design, and roadmap.
- **`examples/`**: Sample programs demonstrating language features, organized into basic examples and comprehensive test suites.
- **`scripts/`**: Utility scripts for development, testing, and environment setup.
- **`.github/workflows/`**: CI/CD workflows, including automated runtime builds and releases.

See `docs/README.md` for a guided tour of the documentation set.

-----

## 🤝 Contributing

Contributions are welcome\! Start by exploring the `docs/` directory:

- **`ROADMAP.md`**: Outlines the long-term vision and feature plans.

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

-----

## 📜 License

Licensed under the **zlib/libpng License with Acknowledgement**. See the [LICENSE](LICENSE) file for details.
