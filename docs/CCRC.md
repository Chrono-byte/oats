Technical Design: Cycle-Collecting Reference Counting for Oats

1. Executive Summary

The current Automatic Reference Counting (ARC) system in Oats provides fast, deterministic memory management but has a critical weakness: it cannot reclaim memory consumed by reference cycles. This document proposes a hybrid memory management system to solve this problem, making Oats's memory management as robust as a traditional garbage collector while retaining the performance benefits of deterministic cleanup.

The proposed solution has two components:

    Weak<T> References: A language-level, opt-in mechanism for developers to manually break cycles, inspired by Rust's std::rc::Weak.

    Automatic Cycle Collector: A runtime component that periodically detects and reclaims cyclical garbage that was not manually broken.

This hybrid approach provides a "best of both worlds" model: most objects are freed instantly, performance is predictable, and memory leaks from cycles are eliminated.

**Important implementation note:** The cycle collector depends on the compiler emitting
correct per-class `field_map` metadata and storing the pointer into the object's
meta slot at byte offset +8. A recent codegen fix standardized byte-offset
pointer arithmetic (ptr->int + add(i64) + int->ptr) across constructors and
field load/store code paths to guarantee that metadata is not corrupted by
field stores. When extending codegen to emit new offset calculations, follow
that pattern so the collector can safely traverse object fields.
2. Background and Motivation

Oats currently uses ARC, where the compiler injects rc_inc and rc_dec calls into the generated code [cite: chrono-byte/oats/Chrono-byte-oats-1d369636b8fe52f2d5246b067be5bf8651ddfc75/docs/ARCHITECTURE.md]. When an object's reference count (RC) drops to zero, its memory is immediately freed [cite: chrono-byte/oats/Chrono-byte-oats-1d369636b8fe52f2d5246b067be5bf8651ddfc75/crates/runtime/src/lib.rs].

This system fails when objects reference each other in a cycle. Consider the following example:

class Node {
    name: string;
    parent: Node | null;
    child: Node | null;

    constructor(name: string) {
        this.name = name;
        this.parent = null;
        this.child = null;
    }
}

function createCycle() {
    let parent = new Node("Parent"); // RC = 1
    let child = new Node("Child");   // RC = 1

    // Create references
    parent.child = child;  // Child RC becomes 2
    child.parent = parent; // Parent RC becomes 2

    // At the end of this function, 'parent' and 'child' go out of scope.
    // Their RCs are decremented from 2 to 1.
    // Since their RCs are not zero, they are never freed. This is a memory leak.
}

This is the classic ARC problem. The proposed system will solve it completely.
3. Proposed Solution: A Hybrid Approach

We will implement two complementary solutions. The first provides a manual, high-performance option for developers, while the second provides an automatic safety net.
Part A: Weak<T> References (Manual Cycle Breaking)

We will introduce Weak<T> and Option<T> types into the Oats language, allowing developers to create non-owning references.
Syntax and Usage

class TreeNode {
    value: number;
    parent: Option<Weak<TreeNode>>; // Parent reference does not own the child
    children: Array<TreeNode>;       // Children are owned by the parent

    constructor(value: number) {
        this.value = value;
        this.parent = null; // Using null for None for now
        this.children = [];
    }
}

let strongRef = new TreeNode(10);
let weakRef: Weak<TreeNode> = strongRef.downgrade(); // Create a weak reference

// To access the object, the weak reference must be upgraded
let access: Option<TreeNode> = weakRef.upgrade();
if (access) { // or if access != null
    // Use the strong reference temporarily
    println(access.value);
}

Implementation Details

    Object Header Modification: The 64-bit object header will be updated to include a weak reference count.

        Bits 0-31: Strong reference count (unchanged).

        Bit 32: Static flag (unchanged).

        Bits 33-48: Weak reference count (16 bits, allowing for ~65k weak references per object).

        Bits 49-63: Reserved for future use.

    Runtime Enhancements (crates/runtime/src/lib.rs):

        New functions rc_weak_inc(ptr) and rc_weak_dec(ptr) will be added to manage the weak count.

        The existing rc_dec function will be modified:

            When the strong count reaches zero, the object's destructor is called (to drop its children), and then rc_weak_dec is called on the object itself. The memory is not immediately freed.

            The rc_weak_dec function will only free the object's memory when the weak count also reaches zero. This ensures the control block persists as long as weak pointers might try to access it.

    Compiler and Type System:

        OatsType::Weak(Box<OatsType>) and OatsType::Option(Box<OatsType>) will be added to crates/oats/src/types.rs.

        The compiler will generate calls to rc_weak_inc and rc_weak_dec when Weak<T> references are created, copied, or dropped.

        Methods like .downgrade() and .upgrade() will be implemented. upgrade() will atomically check if the strong count is > 0 and, if so, increment it and return an Option<T> containing a strong reference.

Part B: Automatic Cycle Collector

For cases where Weak<T> is not used, an automatic cycle collector will run in the background to find and reclaim cyclical garbage. The design will be based on a trial-deletion algorithm.
Data Structures and Metadata

    Root List: The runtime will maintain a list of objects that are candidates for being part of a cycle. An object is added to this list whenever its reference count is decremented but does not reach zero.

    Object Metadata: For the collector to traverse the object graph, it needs to know which fields within an object are pointers. The Oats compiler will be modified to emit a type map for each class. This map will be a simple data structure (e.g., an array of integers) containing the byte offsets of all pointer fields within the class instance.

Collection Algorithm

The collector will run periodically (e.g., when the root list reaches a certain size). The process involves three phases:

    Mark (Trial Deletion):

        For each object in the root list, the collector performs a "trial" decrement of its reference count.

        It then recursively traverses the object graph from that root, decrementing the reference counts of all reachable objects.

        This traversal simulates what would happen if the initial root reference was removed.

    Sweep (Collection):

        After the trial decrements, the collector performs a second traversal. Any object whose reference count is now zero is part of a cycle (or reachable only from a cycle).

        These objects are identified and marked for collection. The collector will call their destructors and free their memory.

    Restore:

        For any object that was part of the traversal but was not collected (i.e., it is still reachable from outside the cycle), its reference count is restored to its original value.

4. Implementation Plan

This feature will be implemented in milestones to manage complexity.

    Milestone 1: Weak<T> and Option<T> Implementation

        Update the object header layout in crates/runtime/src/lib.rs to include a weak count.

        Implement the rc_weak_inc, rc_weak_dec, and modify the rc_dec functions.

        Add Weak<T> and Option<T> to the Oats type system in crates/oats/src/types.rs.

        Implement the compiler logic to generate code for .downgrade() and .upgrade().

        Outcome: Developers can manually break cycles.

    Milestone 2: Type Metadata Generation

        Design the format for the object metadata (type map).

        Modify CodeGen in crates/oats/src/codegen/ to emit this metadata as a constant alongside each class's vtable or constructor.

        Outcome: The runtime has the information it needs to traverse the object graph.

    Milestone 3: Cycle Collector Runtime

        Implement the root list data structure in crates/runtime/src/lib.rs.

        Implement the core Mark-Sweep-Restore algorithm.

        Add the logic for triggering collections (e.g., based on the size of the root list).

        Outcome: The runtime can automatically detect and collect cycles.

    Milestone 4: Testing and Benchmarking

        Write comprehensive tests involving complex, multi-object cycles to verify correctness.

        Benchmark the performance overhead of the cycle collector to tune its trigger threshold.

        Outcome: A robust and performant memory management system.

5. Conclusion

This hybrid approach provides a complete solution to the problem of reference cycles. It combines the predictable performance and deterministic cleanup of ARC with the safety and robustness of a traditional garbage collector. By giving developers both a high-performance manual tool (Weak<T>) and an automatic safety net, this system will make Oats a powerful and reliable choice for building high-performance native applications.