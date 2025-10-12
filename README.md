# Oats 🌾

[![License](https://img.shields.io/badge/license-zlib-blue.svg)](LICENSE)
[![Rust](https://img.shields.io/badge/built%20with-Rust-000000.svg?logo=rust)](https://www.rust-lang.org/)
[![LLVM](https://img.shields.io/badge/powered%20by-LLVM%2018-262D3A.svg)](https://llvm.org/)

> An Ahead-of-Time (AOT) Compiler for TypeScript

Oats is an experimental ahead-of-time (AOT) compiler that converts a small,
well-defined subset of TypeScript into native machine code using LLVM. It
generates standalone executables and uses reference counting for predictable,
manual memory management.

---

## ✨ Features

- **TypeScript Subset**: Supports `number`, `boolean`, `string`, `void`, arrays,
  and classes.
- **Control Flow**: Includes `if`/`else`, loops, and operators.
- **Memory Management**: Automatic reference counting for heap-allocated
  objects.
- **LLVM Integration**: Generates optimized native code.
- **Toolchain**: End-to-end compilation into standalone executables.

---

## 🚀 Getting Started

### Prerequisites

- **Rust Toolchain**: Install via [rustup](https://rustup.rs/).
- **LLVM 18**: Install development headers and tools for your platform.

#### Installing LLVM 18

**Ubuntu/Debian**:

```bash
sudo apt-get update
sudo apt-get install -y llvm-18-dev clang-18
```

**Fedora**:

```bash
sudo dnf install llvm18 llvm18-devel clang18
```

### Compile Your First Program

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/Chrono-byte/oats.git
   cd oats
   ```

2. **Write an Oats Program**:
   ```typescript
   // hello.oats
   export function main(): void {
     println("Hello from Oats! 🌾");
   }
   ```

3. **Compile and Run**:
   ```bash
   cargo run -p oats --bin toasty -- ./hello.oats
   ./hello
   ```

---

## 🔒 Security Features

- **Heap Limits**: Configurable max heap size (default: 1 GB).
- **Integer Overflow Protection**: Checked arithmetic for allocations.

---

## 📁 Project Structure

- **`crates/oats`**: Core compiler for parsing, type checking, and LLVM IR
  generation.
- **`crates/runtime`**: Runtime library for memory management and utilities.

---

## 🤝 Contributing

Contributions are welcome! Start by exploring the `docs/` directory:

- **`ROADMAP.md`**: Long-term vision and feature plans.
- **`IMPROVEMENT_PLAN.md`**: Technical debt and refactoring opportunities.

### Development Workflow

1. **Set Up Environment**:
   ```bash
   source ./scripts/setup_env.sh
   ```

2. **Build and Test**:
   ```bash
   cargo build
   cargo test --workspace
   ```

---

## 📄 License

Licensed under the **zlib/libpng License**. See the [LICENSE](LICENSE) file for
details.
