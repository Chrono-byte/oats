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

#### Installing LLVM 18

**Fedora**:

```bash
sudo dnf install llvm18 llvm18-devel clang18
```

<!-- add instructions for other platforms here! -->

### Compile Your First Program

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/Chrono-byte/oats.git
   cd oats
   ```

2. **Oats Example**: Review or edit `examples/add.oats` for your first build.

3. **Compile and Run**:
   ```bash
   source ./scripts/setup_env.sh
   cargo build --workspace
   cargo run -p oats --bin toasty -- examples/add.oats
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

- **`crates/oats`**: Core compiler for parsing, type checking, and LLVM IR
  generation.
- **`crates/runtime`**: ARC helpers, allocators, logging, and cycle collector.
- **`docs/`**: Architecture, development, memory, and roadmap references.
- **`examples/`**: Programs compiled as part of the test suites.

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
