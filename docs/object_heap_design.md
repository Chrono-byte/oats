Of course. My apologies for the tool error. Here is the complete, updated object heap design document that incorporates the `Promise` layout from the older version into the modern, hybrid memory model.

***

# Object / Heap / Class Design for Oats (Hybrid RC Model)

This design doc describes a pragmatic, high-performance runtime and object model for the Oats AOT prototype. It focuses on implementing the performance benefits of modern systems languages (like Rust's stack allocation and deterministic destruction) while remaining fully compatible with standard TypeScript code.

## Goals and Constraints

* **TypeScript Compatibility:** The memory model must be transparent. Standard TypeScript code should work as expected with its default share-by-reference semantics without any source code modifications.
* **Performance:** Aim for performance competitive with AOT-compiled languages by eliminating unnecessary heap allocations and minimizing runtime overhead.
* **Deterministic Destruction:** Objects should be freed as soon as they are no longer reachable, providing predictable memory usage and avoiding the long pauses associated with traditional garbage collectors.

---

## High-Level Representation

A split representation between primitive values and heap-allocated objects remains the most effective approach for performance and simplicity.

* **Primitive Types:** `number` and `boolean` are unboxed values. They will be represented directly as LLVM `f64` and `i1`/`i8` types, respectively. They live on the stack or in registers, incurring no allocation overhead.

* **Reference Types:** `string`, `class` instances, arrays, object literals, and `Promise` objects are reference types. They are represented in generated code as pointers (`i8*`). The memory for the objects they point to will be managed by the hybrid strategy outlined below.

---

## Memory Management Strategy: A Rust-Inspired Hybrid Model

To achieve our goals without altering the TypeScript language, we will use a combination of techniques managed entirely by the compiler and runtime. This multi-layered approach allows us to apply powerful optimizations transparently.

### **Phase 0: Automatic Reference Counting (Current Foundation)**

The foundation of the memory model is **Automatic Reference Counting (RC)**. This aligns perfectly with TypeScript's default share-by-reference semantics and provides the core benefit of deterministic destruction.

* **How it Works:** The Oats compiler automatically injects `rc_inc` and `rc_dec` calls into the generated LLVM IR whenever references are created or go out of scope. When an object's reference count reaches zero, its memory is immediately deallocated.
* **Benefit:** Provides the deterministic, immediate cleanup that is a key advantage of Rust's memory model, leading to predictable memory usage.

### **Phase 1: Compiler-Managed Stack Allocation via Escape Analysis**

This is the most significant Rust-inspired optimization we can perform transparently. In many programs, an object created inside a function never "escapes" that function's scope (i.e., it's not returned or assigned to an external variable). Rust's compiler proves this at compile time and allocates such objects on the stack, which is dramatically faster than heap allocation.

* **How it Works:** The Oats compiler will perform a lightweight **Escape Analysis** pass on the AST before code generation.
    * If the compiler can prove that an object's lifetime is confined to the function in which it was created, it will emit an `alloca` instruction to place the object on the stack instead of calling the heap allocator (`malloc`).
    * No `rc_inc`, `rc_dec`, or `free` calls will be generated for this object. Its memory is automatically reclaimed when the function's stack frame is popped on return.
* **Benefit:** This provides a massive performance boost for functions with temporary, internal objects, directly mimicking a key advantage of Rust's ownership system without requiring any changes to the TypeScript code.

### **Phase 2: Cycle-Collecting RC**

The primary weakness of simple reference counting is **reference cycles** (e.g., when object A points to B, and B points back to A). These objects will never reach a reference count of zero and will leak memory.

Instead of falling back to a full tracing GC (which is non-deterministic), we can augment our RC system with a periodic **Cycle Collector**.

* **How it Works:** The runtime will occasionally run a lightweight tracing process that specifically looks for cycles among objects with non-zero reference counts.
    * This collector can be triggered when the heap grows beyond a certain threshold.
    * It identifies isolated subgraphs of objects that are only kept alive by internal references and then deallocates the entire cycle.
* **Benefit:** This solves the main weakness of RC while preserving its deterministic nature for the vast majority of objects. It provides the robustness of a GC for cycles without its unpredictable "stop-the-world" pauses.

---

## Heap Object Layout

All heap-allocated objects will share a common header structure to support this hybrid model.

### Common Header (`u64`)

* **High 32 bits:** Used for a **type tag** and **GC flags**. One bit will be reserved as the "mark bit" for the cycle collector.
* **Low 32 bits:** Used for the **atomic reference count**.

### Concrete Layouts

* **String:**
    * `u64` header
    * `u64` length
    * `u8[]` bytes (UTF-8) + null terminator

* **Array:**
    * `u64` header (type tag indicates if elements are numbers or pointers)
    * `u64` length
    * `u64` capacity
    * `void*` data buffer

* **Class Instance / Object Literal:**
    * `u64` header
    * Fields stored in a contiguous block, with layout determined at compile time.

* **Promise (for async/await):**
    * `u64` header (replaces the original `refcount` field, providing a unified object header)
    * `u8` status (0=Pending, 1=Resolved, 2=Rejected)
    * `void*` result (pointer to result value)
    * `void**` waiters (pointer to a dynamically-allocated array of waiting task pointers)
    * `u64` waiter_count

---

## Incremental Implementation Plan

1.  **Solidify RC (Current):** Continue to refine the existing reference counting implementation in `crates/runtime/src/lib.rs` and the `rc_inc`/`rc_dec` codegen in `crates/oats/src/codegen/mod.rs`. Ensure it is robust and thread-safe.

2.  **Implement Escape Analysis (Phase 1):**
    * Add an analysis pass to the compiler that walks the AST of each function to identify non-escaping object allocations.
    * Update the `CodeGen` logic to check the results of this analysis.
    * If an object is non-escaping, emit `alloca` and local `store` instructions. Otherwise, continue to generate calls to the heap allocator and RC functions.

3.  **Implement a Cycle Collector (Phase 2):**
    * In `crates/runtime/src/lib.rs`, add the logic for the cycle collector.
    * The collector will maintain a list of "potentially cyclic" objects (those with RC > 1).
    * When triggered, it will perform a mark-and-sweep pass only on this subset of objects to find and break cycles.