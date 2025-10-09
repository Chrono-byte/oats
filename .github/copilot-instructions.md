# Oats — Comprehensive AI Agent Guide

This document provides in-depth guidance for AI assistants and contributors working on the Oats project.

## 1\. Project Overview

Oats is an experimental Ahead-of-Time (AOT) compiler that transforms a strict subset of TypeScript into native machine code. It leverages the power of LLVM to produce fast, standalone executables with deterministic memory management via reference counting.

The compilation pipeline is as follows:

1.  **Parse:** The TypeScript/Oats source code is parsed into an Abstract Syntax Tree (AST) using `deno_ast`.
2.  **Type Check:** The AST is traversed to check type annotations and infer types, mapping them to the internal `OatsType` representation.
3.  **Codegen:** The typed AST is lowered into LLVM Intermediate Representation (IR) using the `inkwell` crate.
4.  **Link:** The generated object file is linked with the Oats runtime (`crates/runtime`) to produce a native executable.

-----

## 2\. Core Components

The project is a Rust workspace with two main crates:

  * **`crates/oats`**: The core compiler.
      * `parser.rs`: Handles source code parsing and enforces syntax rules like semicolon usage.
      * `types.rs`: Defines the internal type system (`OatsType`) and logic for mapping from TypeScript types.
      * `codegen/`: The heart of the compiler, responsible for LLVM IR emission. It's modularized into `emit.rs`, `expr.rs`, `stmt.rs`, and `helpers.rs`.
      * `diagnostics.rs`: Provides utilities for reporting user-friendly, rustc-like error messages with source context.
  * **`crates/runtime`**: A `staticlib` that provides essential services to the compiled executable, such as memory allocation, reference counting, and helpers for string and array manipulation.

-----

## 3\. Heap Object System (CRITICAL)

The runtime uses a unified 64-bit header for all heap objects. This is central for correct memory and reference counting behavior.

**Header Layout (8 bytes at offset 0):**

  * **Bits 0-31:** Strong reference count (atomic, u32).
  * **Bit 32:** Static flag (1 = immortal/don't modify RC, 0 = heap-allocated).
  * **Bits 33-48:** Weak reference count (16 bits).
  * **Bits 49-63:** Type tag and other flags.

**Object Layouts:**

  * **Static string literals:** `[header (static bit set)][length i64][data bytes + NUL]`. The codegen returns a pointer to the data field (offset 16).
  * **Heap strings:** The runtime allocates a header (RC=1) + length + data; the runtime returns a pointer to the data (offset 16).
  * **Arrays:** `[header][length i64][elements...]`. Allocators return the base pointer (offset 0).
  * **Classes/Objects:** `[header][metadata ptr (8)][field0 (8)][field1 (8)]...`. Constructors return the base pointer (offset 0).

**Pointer Rules & RC Operations:**

  * The system accepts two pointer kinds: **object base pointers** (offset 0) and **string data pointers** (offset 16). All RC helpers in the runtime are designed to handle either type.
  * The runtime provides `get_object_base(p)`, which heuristically tests for a valid header at offset 0 and p-16 to find the true base for RC operations.
  * `rc_inc` / `rc_dec`: Atomically modify the strong count. `rc_dec` will call the object's destructor (if any) and `rc_weak_dec` when the strong count reaches zero.
  * `rc_weak_inc` / `rc_weak_dec`: Atomically modify the weak count. `rc_weak_dec` frees the object's memory only when both strong and weak counts are zero.
  * `rc_weak_upgrade`: Attempts to atomically create a strong reference from a weak one, returning `null` if the object is already destroyed.

-----

## 4\. How to Build & Run

1.  **Set up the environment:** The `scripts/setup_env.sh` script detects your LLVM 18 installation and exports the necessary environment variables.

    ```bash
    source ./scripts/setup_env.sh
    ```

2.  **Compile and Run an Example:** The `aot_run` binary handles the entire compilation and linking process.

    ```bash
    # This will create an executable at ./aot_out/hello
    cargo run -p oats --bin aot_run -- examples/hello.oats

    # Run your compiled program
    ./aot_out/hello
    ```

-----

## 5\. Development Guidance

**Roadmap Highlights:**

  * **Phase 1 (Short-term):** Focus is on achieving basic compatibility by implementing robust **module resolution**, full support for **interfaces and type aliases**, and expanding the **standard library**.
  * **Phase 2 (Medium-term):** Tackle advanced language features like **closures with capture**, **generics** via monomorphization, and **error handling** with `try/catch`.
  * **Phase 3 (Long-term):** Aim for production readiness with advanced type system features, `node_modules` compatibility, and performance optimizations like escape analysis.

**Checklist for Code Changes:**

  * **RC Management:** If adding a pointer field or storing pointers, ensure you call `rc_inc` on store and that `rc_dec` will be called on drop.
  * **Runtime Helpers:** When adding new runtime functions, you must update both `crates/runtime` and the `CodeGen` struct in `crates/oats` to declare the new function.
  * **Error Handling:** All fallible operations in the codegen logic should return a `Result<_, Diagnostic>` and propagate errors rather than panicking.
  * **Testing:** Add unit tests for all new features. Use `insta` for snapshot testing of generated IR and add integration tests for end-to-end functionality.