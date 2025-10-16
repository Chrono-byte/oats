# Cross-Compilation Guide

This guide explains how to build the Oats project for Windows from a Linux machine using cross-compilation.

## Prerequisites

- Rust toolchain with support for cross-compilation
- MinGW-w64 toolchain for C/C++ cross-compilation
- Cargo configuration for Windows targeting

## Step-by-Step Setup

### 1. Install the Rust Target for Windows

Add the Windows target to your Rust toolchain:

```bash
rustup target add x86_64-pc-windows-gnullvm
```

### 2. Install a Cross-Compilation Toolchain for C/C++

For the gnullvm target, you'll need Clang and LLVM tools:

**On Debian/Ubuntu:**

```bash
sudo apt-get update
sudo apt-get install clang lld llvm
```

**On Fedora/CentOS:**

```bash
sudo dnf install clang lld llvm
```

### 3. Configure Cargo for Cross-Compilation

**Note:** Cross-compilation for Windows from Linux is not currently supported due to complex LLVM and system dependencies. The build process requires Windows-specific libraries and tools that are difficult to set up in a Linux environment. Consider building on Windows natively or using a Windows development environment.

If you still want to attempt cross-compilation, the configuration would be:

Create or edit `.cargo/config.toml` in the project root:

```toml
[target.x86_64-pc-windows-gnu]
linker = "x86_64-w64-mingw32-gcc"
ar = "x86_64-w64-mingw32-ar"
```

Or for gnullvm:

```toml
[target.x86_64-pc-windows-gnullvm]
linker = "clang"
ar = "llvm-ar"
```

### 4. Build the Project

Attempt to build the `toasty` executable for Windows:

```bash
cargo build --package toasty --target x86_64-pc-windows-gnu --release
```

or

```bash
cargo build --package toasty --target x86_64-pc-windows-gnullvm --release
```

This may fail due to missing cross-compiled dependencies.

### 5. Build Oats Programs

Use the cross-compiled `toasty.exe` to build your Oats programs:

```bash
./target/x86_64-pc-windows-gnullvm/release/toasty build examples/add.oats
```

This produces `add.exe` in the `aot_out/` directory.

## Summary

Cross-compilation from Linux to Windows is not currently supported due to LLVM dependencies.

If attempting anyway:

1. Install Rust Windows target: `rustup target add x86_64-pc-windows-gnu` or `x86_64-pc-windows-gnullvm`
2. Install toolchain: MinGW or Clang/LLVM
3. Configure Cargo with appropriate linker settings
4. Build: `cargo build --package toasty --target <target> --release`
5. May not work due to missing dependencies
