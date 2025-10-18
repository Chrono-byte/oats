# Roadmap: Evolving Oats into a Systems Language

The goal of transforming Oats into a true systems programming language is achievable by building upon its existing AOT compilation and deterministic memory model. The following areas represent the key features and design philosophies required to bridge the gap between its current TypeScript-inspired state and a language suitable for low-level systems development.

## 1. Granular Memory Management & Control

Systems programming requires precise control over memory layout and lifetime. While ARC provides excellent determinism, introducing more explicit control is essential.

### Current State:
- Automatic Reference Counting (ARC) for all heap objects.
- A background cycle collector to handle reference cycles.
- Escape analysis to eliminate some redundant RC operations.

### Next Steps:
- **Introduce unowned References**: As planned in the language design, implement unowned references. These would be non-owning pointers that do not perform any RC operations, providing a zero-cost abstraction for performance-critical code where object lifetimes can be guaranteed by the programmer.
- **Explicit Memory APIs**: Expose a low-level memory management API in the standard library (e.g., memory::alloc, memory::free, memory::copy). This would allow developers to bypass ARC entirely and manage memory manually for specific data structures or performance-sensitive algorithms.
- **Stack Allocation for Objects (structs)**: Introduce a struct keyword for value types that are always allocated on the stack. This provides a way to create complex data types without incurring heap allocation and RC overhead, a cornerstone of systems languages like C++ and Rust.

## 2. Full-Fledged Foreign Function Interface (FFI)

Direct interoperability with existing C and Rust libraries is non-negotiable for a systems language. The existing FFI plan is a critical first step.

### Current State:
- A detailed plan exists to implement a Deno-compliant FFI.

### Next Steps:
- **Execute the FFI Plan**: This is the highest-priority item. The ability to call C/Rust functions is the gateway to interacting with operating system APIs, hardware drivers, and high-performance native libraries.
- **Advanced FFI Features**:
  - Structs by Value: Support passing structs by value across the FFI boundary, allowing for seamless interaction with C APIs.
  - Callbacks: Enable native code to call back into Oats functions, allowing for asynchronous operations and event handling.
  - Unsafe Pointers: Introduce raw pointer types (*const T, *mut T) that can be used within unsafe blocks to perform raw memory manipulation, essential for FFI.

## 3. Low-Level Primitive Types

Systems programming requires precise control over data representation. The current number (f64) type is insufficient for this.

### Current State:
- A single number type, which maps to f64.

### Next Steps:
- **Introduce Explicit Integer and Float Types**:
  - Integers: i8, u8, i16, u16, i32, u32, i64, u64.
  - Floats: f32, f64.
- **Introduce Architecture-Sized Types**: isize and usize for representing pointers and memory offsets.
- **Character Type**: A char type for representing a single Unicode scalar value.

## 4. Advanced Concurrency Model

While async/await is excellent for I/O-bound tasks, systems programming often requires direct control over OS-level threads.

### Current State:
- An async/await implementation based on a poll-state machine.

### Next Steps:
- **OS Threading API**: Provide a standard library module (e.g., thread) for creating and managing OS threads (thread::spawn, thread::join).
- **Synchronization Primitives**: Implement low-level synchronization primitives like Mutex, RwLock, Condvar, and atomic types (AtomicU32, etc.). This would allow for safe multi-threaded programming without relying on a garbage collector.

## 5. Toolchain and Ecosystem (toasty as cargo)

A robust build system and package manager are critical for building large, multi-component systems like operating systems, databases, or game engines.

### Current State:
- A detailed plan exists to evolve toasty into a cargo-like tool.

### Next Steps:
- **Execute the Project Plan**: Implementing the phased plan to introduce Oats.toml, dependency management, and workspaces is a prerequisite for any serious systems development. The ability to build complex projects from multiple libraries is essential.

## Conclusion

Oats is uniquely positioned to become a systems-level dialect of TypeScript. Its foundation is strong. By focusing on giving the programmer more explicit control over memory, types, and concurrency, and by executing the existing plans for FFI and the cargo-style toolchain, Oats can offer a compelling combination of high-level ergonomics and low-level power.
