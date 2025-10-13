A Unified Parallelism Strategy for Oats (CPU & GPU)

The goal of this initiative is to build a unified, first-class parallelism model for Oats that seamlessly integrates both CPU and GPU execution. Drawing inspiration from Swift's modern concurrency, this approach prioritizes safety and clarity through structured concurrency and an actor-based model, making it easy to write correct and efficient programs for heterogeneous hardware.
1. Proposed Unified Parallelism Model

We will build a model around structured tasks that can be dispatched to either the CPU or the GPU, ensuring that all concurrent operations are safe and easy to reason about.

    Structured Concurrency (spawn let and TaskGroup):

        spawn let remains the core for creating structured, scope-bound concurrent tasks on the CPU.

        The TaskGroup API will be the primary mechanism for data parallelism on both the CPU and GPU. It will be extended with methods that allow the developer to express where a task should ideally run.

    Example Syntax (Unified TaskGroup):

    async function processData(items: MyDataItem[]) {
        // Use a TaskGroup for a collection of parallel work.
        await withTaskGroup(async (group) => {
            for (const item of items) {
                // The developer can hint that this is a GPU-suitable task.
                // The runtime makes the final decision based on data locality and workload.
                await group.spawnOnGpu(async () => {
                    return performGpuIntensiveWork(item);
                });
            }
        });
    }

    Actors (for Shared CPU State):

        The actor model remains a CPU-specific feature. It is the primary tool for safely managing shared mutable state that is accessed and modified by concurrent CPU tasks. Actors are not suitable for the GPU's memory model and will be restricted to the host.

2. Core Implementation Components

    Compiler Enhancements (oatsc):

        Unified AST: The parser will recognize spawn let, TaskGroup usage (including GPU-specific methods), and the actor class syntax.

        Concurrency-Aware Type System (Sendable): The concept of Sendable types will be expanded. The compiler will not only check if a type can be safely sent between CPU threads, but also if it can be serialized and transferred to GPU memory.

        GPU Code Analysis & Generation: The compiler will be responsible for:

            Verifying that code inside a spawnOnGpu closure uses a GPU-compatible subset of the Oats language (e.g., no file I/O, no string manipulation).

            Compiling these closures into GPU kernels (e.g., SPIR-V or PTX) in addition to the standard CPU code.

    Runtime Enhancements (runtime):

        Heterogeneous Scheduler: The "cooperative thread pool" will be upgraded to a heterogeneous scheduler. It will manage the CPU thread pool and the GPU device queue. It will be responsible for scheduling CPU tasks, managing data transfers to/from the GPU, and dispatching GPU kernels.

        Implicit Data Management: The runtime will automatically handle memory transfers. When a spawnOnGpu task is created with Sendable data, the runtime will manage copying that data to the GPU (ideally asynchronously to hide latency) before the kernel runs and copying the result back.

3. Key Design Questions (Answered by the Unified Model)

    Data Race Prevention: Solved by the compiler's Sendable checks for both CPU and GPU tasks, and the actor model for mutable state on the CPU.

    Task Granularity & Placement: The TaskGroup API provides an explicit hint from the developer. The runtime's heterogeneous scheduler makes the final decision, allowing for future optimizations where it could override the hint for performance reasons (e.g., if the GPU is busy or the data is too small).

    Integration with Async I/O: The unified scheduler will manage all task types—CPU-bound, GPU-bound, and I/O-bound—within the same async/await framework, preventing blocking.

4. Proposed Staged Implementation

    Stage 1: Runtime Foundation.

        Implement the core cooperative, work-stealing CPU thread pool and task scheduler.

        Add basic capabilities for detecting and interfacing with a GPU device.

    Stage 2: Structured CPU Concurrency.

        Implement the full CPU-side model: spawn let, a CPU-only TaskGroup, and the initial compiler checks for Sendable types.

    Stage 3: GPU Integration.

        Extend the TaskGroup API with spawnOnGpu.

        Enhance the compiler to analyze and generate GPU kernels from a subset of Oats.

        Upgrade the runtime scheduler to be heterogeneous, capable of managing the GPU command queue and data transfers.

    Stage 4: The Actor Model.

        Implement the actor class syntax as a CPU-specific feature for advanced state management.

    Stage 5: Performance Optimization & Automatic Offloading.

        With the explicit model in place, begin research into a profitability analyzer. This would allow the runtime to automatically offload certain CPU tasks to the GPU if it determines it would be faster, moving towards the "zero-effort" GPU acceleration goal.

This unified plan provides a robust, step-by-step approach to building a state-of-the-art concurrency system that leverages the full power of modern hardware.