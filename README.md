Oats 🌾
An Ahead-of-Time (AOT) Compiler for TypeScript

Oats is an experimental Ahead-of-Time (AOT) compiler that transforms a strict subset of TypeScript into native machine code. It leverages the power of LLVM to produce fast, standalone executables with deterministic memory management via reference counting.

The goal is to explore the performance potential of TypeScript when freed from a JIT compiler and a traditional garbage collector, bringing it closer to the performance profile of systems languages like C++ and Rust.
Features

Oats is a work in progress, but it already supports a solid set of modern language features:

    Type System: number, boolean, string, void, and typed arrays (number[], string[]).

    Classes: Full support for class declarations with constructors, methods, fields, and this binding.

    Control Flow: if/else, for loops, while loops, do-while loops, and for-of loops.

    Operators: Full support for binary and unary operators.

    Memory Management: A robust, automatic reference counting (RC) system for all heap-allocated objects (strings, arrays, classes), ensuring deterministic memory cleanup.

    Toolchain: A complete build driver that compiles Oats code into a standalone native executable.

Getting Started
Prerequisites

    Rust Toolchain: Install Rust via rustup.

    LLVM 18: Oats requires LLVM version 18.

Installing LLVM 18

    Ubuntu / Debian:

    sudo apt-get update
    sudo apt-get install -y llvm-18-dev clang-18

    macOS (via Homebrew):

    brew install llvm@18

Compiling Your First Program

    Clone the Repository:

    git clone [https://github.com/your-username/oats.git](https://github.com/your-username/oats.git)
    cd oats

    Write Your Oats Program:
    Create a file named hello.oats:

    // hello.oats
    export function main(): void {
        println("Hello from Oats! 🌾");
    }

    Compile and Run:
    Use the aot_run binary to compile your program into a native executable. This single command handles everything: compiling your Oats code, building the runtime, and linking the final binary.

    # This will create an executable at ./aot_out/hello
    cargo run -p oats --bin aot_run -- ./hello.oats

    # Run your compiled program!
    ./aot_out/hello

    You should see Hello from Oats! 🌾 printed to your console.

Project Structure

The Oats compiler is organized as a Rust workspace with two main crates:

    crates/oats: The core compiler, responsible for parsing, type checking, and generating LLVM IR from Oats source code.

    crates/runtime: A small, C-callable runtime library written in Rust. It provides essential services like memory allocation, reference counting, and helpers for string and array manipulation.

Contributing

Contributions are welcome! Oats is an ambitious project with a lot of exciting work ahead. The best place to start is by reading through the extensive internal documentation in the docs/ directory, especially:

    docs/ROADMAP_TO_REAL_TYPESCRIPT.md: The long-term vision and feature plan.

    docs/IMPROVEMENT_PLAN.md: A list of immediate technical debt and refactoring opportunities.

Development Workflow

    Set up your environment: The scripts/setup_env.sh script can help configure your shell with the necessary environment variables for LLVM.

    source ./scripts/setup_env.sh

    Build and test:

    cargo build
    cargo test --workspace

License

This project is licensed under the zlib License. See the LICENSE file for details