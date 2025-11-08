# Oats 🌾

[![License](https://img.shields.io/badge/license-zlib-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/built%20with-Rust-000000.svg?logo=rust)](https://www.rust-lang.org/)
[![LLVM](https://img.shields.io/badge/powered%20by-LLVM%2018-262D3A.svg)](https://llvm.org/)

> An Ahead-of-Time (AOT) Compiler for the Oats Language

Oats is an experimental ahead-of-time (AOT) compiler that compiles the Oats
language into native machine code using LLVM. It
supports multi-file compilation with a package system for dependency management,
generates standalone executables, and uses reference counting for memory
management.

---

## Table of Contents

- [Features](#-features)
- [Getting Started](#-getting-started)
- [Project Structure](#-project-structure)
- [Contributing](#-contributing)
- [License](#-license)

---

## ✨ Features

- **Rich Language Features**: Supports numbers, booleans, strings, arrays,
  classes, and unions.
- **Package System**: Supports multi-file compilation with dependency
  resolution, allowing complex applications to be built from modular components.
- **Generics & Unions**: Supports generics via call-site monomorphization and
  handles complex union types with runtime boxing.
- **Async (preview)**: `async` functions are lowered into efficient poll-state
  machines with resume blocks, enabling asynchronous operations.
- **Memory Management**: Implements deterministic Automatic Reference Counting
  (ARC) with support for weak references and a background cycle collector.
- **LLVM Integration**: Emits optimized LLVM 18 IR and links against a custom
  safe runtime to produce native executables.

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

**Arch Linux**:

```bash
sudo pacman -S llvm18 clang
```

### Standalone Toasty (Recommended)

`toasty` can work as a standalone tool by automatically fetching pre-built
runtime libraries from GitHub releases.

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

The runtime is automatically cached in `~/.oats/runtime/` for faster
subsequent builds.

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

---

## 📁 Project Structure

- **`crates/oatsc`**: Core compiler for parsing, type checking, and LLVM IR
  generation.
- **`crates/runtime`**: The safe runtime library providing ARC helpers,
  allocators, logging, and the cycle collector. Built as a static library and
  published via GitHub Actions.
- **`crates/toasty`**: The CLI wrapper for the compiler with `build` and `run`
  commands, including package-based compilation support.
- **`crates/std`**: The Oats standard library providing console, filesystem,
  networking, and other system utilities.
- **`docs/`**: Comprehensive documentation including architecture, development
  guides, memory design, and roadmap.
- **`examples/`**: Sample programs demonstrating language features, organized
  into basic examples and comprehensive test suites (`proper_tests/`).
- **`fuzz/`**: Fuzz testing suite for the parser.
- **`scripts/`**: Utility scripts for development, testing, and environment
  setup.
- **`.github/workflows/`**: CI/CD workflows, including automated testing and
  release builds.

See `docs/README.md` for a guided tour of the documentation set.

---

## 🤝 Contributing

Contributions are welcome\! Start by exploring the `docs/` directory:

- **`ROADMAP.md`**: Outlines the long-term vision and feature plans.

### Development Workflow

**Build and Test**:

   ```bash
   # Build all crates
   cargo build --workspace

   # Run all unit and integration tests
   cargo test --workspace

   # Run the end-to-end "proper" tests
   ./scripts/run_all_proper_tests.sh
   ```

---

## 📜 License

Licensed under the **zlib/libpng License with Acknowledgement**. See the
[LICENSE](LICENSE) file for details.
