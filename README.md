# Oats 🌾

[![License](https://img.shields.io/badge/license-zlib-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/built%20with-Rust-000000.svg?logo=rust)](https://www.rust-lang.org/)
[![LLVM](https://img.shields.io/badge/powered%20by-LLVM%2018-262D3A.svg)](https://llvm.org/)

> An Ahead-of-Time (AOT) Compiler for TypeScript

Oats is an experimental Ahead-of-Time (AOT) compiler that transforms a strict
subset of TypeScript into native machine code. It leverages the power of LLVM to
produce fast, standalone executables with deterministic memory management via
reference counting.

## ✨ Features

Oats is a work in progress, but it already supports a solid set of modern
language features:

- **Type System**: `number`, `boolean`, `string`, `void`, and typed arrays
  (`number[]`, `string[]`)
- **Classes**: Full support for class declarations with constructors, methods,
  fields, and `this` binding
- **Control Flow**: `if`/`else`, `for` loops, `while` loops, `do-while` loops,
  and `for-of` loops
- **Operators**: Full support for binary and unary operators
- **Memory Management**: A robust, automatic reference counting (RC) system for
  all heap-allocated objects (strings, arrays, classes), ensuring deterministic
  memory cleanup
- **Toolchain**: A complete build driver that compiles Oats code into a
  standalone native executable

## 🚀 Getting Started

### Prerequisites

- **Rust Toolchain**: Install Rust via [rustup](https://rustup.rs/)
- **LLVM 18**: Oats is tested with LLVM 18; install development headers and `clang-18`/`llvm-18` packages for your platform

### Installing LLVM 18

#### Ubuntu / Debian:

```bash
sudo apt-get update
sudo apt-get install -y llvm-18-dev clang-18
```
#### Fedora:

```bash
sudo dnf install llvm18 llvm18-devel clang18
```

### Compiling Your First Program

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/Chrono-byte/oats.git
   cd oats
   ```

2. **Write Your Oats Program:**

   Create a file named `hello.oats`:
   ```typescript
   // hello.oats
   export function main(): void {
       println("Hello from Oats! 🌾");
   }
   ```

3. **Compile and Run:**

  Use the `toasty` binary to compile your program into a native executable.
   This single command handles everything: compiling your Oats code, building
   the runtime, and linking the final binary.

  ```bash
  # This will create an executable at ./aot_out/hello (preferred):
  cargo run -p oats --bin toasty -- ./hello.oats

  # Run your compiled program:
  ./hello
  ```

   You should see `Hello from Oats! 🌾` printed to your console.

### Runtime diagnostics

The runtime emits diagnostic logging only when explicitly enabled. To enable ad-hoc runtime diagnostics set the environment variable:

```bash
export OATS_RUNTIME_LOG=1
# or for collector-specific logs
export OATS_COLLECTOR_LOG=1
```

Additionally, the runtime includes a test-only helper `collector_test_enqueue()` which is not present in default release builds — it is compiled only when the `collector-test` Cargo feature is enabled for both the runtime and the compiler crates.

## � Security Features

Oats includes comprehensive security hardening to protect against malicious or malformed input:

### Resource Limits
- **Heap Limits**: Configurable max heap size (default: 1 GB)
- **Allocation Limits**: Configurable max single allocation (default: 256 MB)
- **Source Size Limits**: Configurable max source file size (default: 10 MB)
- **Recursion Depth**: Hard limit of 32 levels to prevent stack overflow

```bash
# Configure resource limits
export OATS_MAX_HEAP_BYTES=536870912      # 512 MB heap
export OATS_MAX_ALLOC_BYTES=67108864      # 64 MB per allocation
export OATS_MAX_SOURCE_BYTES=52428800     # 50 MB source files
```

### Security Hardening
- **Integer Overflow Protection**: All allocation size calculations use checked arithmetic
- **Fuzz Testing**: Comprehensive fuzz testing infrastructure (see `docs/FUZZING.md`)
- **Safety Documentation**: All unsafe code documented with safety contracts (see `docs/RUNTIME_SAFETY.md`)

For more details, see:
- **`SECURITY.md`**: Security policy and vulnerability reporting
- **`docs/SECURITY_HARDENING.md`**: Complete security improvements summary
- **`docs/RUNTIME_SAFETY.md`**: Safety contracts for unsafe code
- **`docs/FUZZING.md`**: Fuzz testing guide

## �📁 Project Structure

The Oats compiler is organized as a Rust workspace with two main crates:

- **`crates/oats`**: The core compiler, responsible for parsing, type checking,
  and generating LLVM IR from Oats source code
- **`crates/runtime`**: A small, C-callable runtime library written in Rust. It
  provides essential services like memory allocation, reference counting, and
  helpers for string and array manipulation

## 🤝 Contributing

Contributions are welcome! Oats is an ambitious project with a lot of exciting
work ahead. The best place to start is by reading through the extensive internal
documentation in the `docs/` directory, especially:

- **`docs/ROADMAP_TO_REAL_TYPESCRIPT.md`**: The long-term vision and feature
  plan
- **`docs/IMPROVEMENT_PLAN.md`**: A list of immediate technical debt and
  refactoring opportunities

### Development Workflow

1. **Set up your environment**: The `scripts/setup_env.sh` script can help
   configure your shell with the necessary environment variables for LLVM.
   ```bash
   source ./scripts/setup_env.sh
   ```

2. **Build and test**:
   ```bash
   cargo build
   cargo test --workspace
   ```

## 📄 License

This project is licensed under the **zlib/libpng License with Acknowledgement**. See the [LICENSE](LICENSE)
file for details.
