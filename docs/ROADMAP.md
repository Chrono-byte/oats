# Project Plan: Achieving Full TypeScript Compatibility in Oats

**Objective:** To evolve Oats from a TypeScript-inspired language into a fully-compatible, high-performance TypeScript runtime. This plan aims to achieve parity with Deno's TypeScript implementation, covering the compiler, type system, runtime, and developer tooling.

**Current Status (October 18, 2025):** Oats currently supports a substantial TypeScript subset including classes, async/await, generics, unions, interfaces, enums, destructuring, and template literals. The parser achieves ~90.43% success rate on TypeScript conformance tests (4,547/5,028 files parsed successfully).

## Phase 1: Complete Syntactic & Type Definition Parity ✅ MOSTLY COMPLETE

**Goal:** Ensure the Oats parser and type system can understand 100% of valid TypeScript syntax and type definition files (.d.ts), even if it cannot yet fully type-check all advanced features.

### Currently Implemented ✅

- **Core syntax**: Functions, classes, interfaces, enums, generics, unions
- **Async/await**: Full `async`/`await` syntax and lowering
- **Destructuring**: Array and object destructuring
- **Template literals**: String interpolation
- **Arrow functions**: Full support with closures
- **Control flow**: `if`/`else`, loops, `try`/`catch` (basic)
- **Modules**: `import`/`export` statements

### Remaining Tasks

- **Integrate Official TypeScript Conformance Tests:**
  - Establish a process to regularly run the official TypeScript compiler test suite against the oatsc parser.
  - The remaining ~9.57% failures are primarily due to missing support for advanced ES6+ features.

- **Implement Remaining Syntax Features:**
  - Add robust parsing support for features not fully covered by the Deno test suite:
    - Decorators (Stage 3 proposal).
    - `namespace` and `module` blocks for organizing types.
    - `const enum` and other enum variants.
    - Full JSX/TSX parsing, including fragments and complex generic components.

- **Full .d.ts Support:**
  - Enhance the type system to correctly parse and load type information from ambient declaration files (.d.ts).
  - Implement support for declaration merging, allowing multiple interface declarations for the same object to be combined.

**Outcome:** oatsc can reliably parse any TypeScript file or project without syntax errors and can correctly interpret the type signatures from external .d.ts files, setting the stage for advanced type checking.

## Phase 2: Advanced Type System Implementation

**Goal:** Evolve the Oats type checker to correctly handle the full spectrum of TypeScript's powerful and complex type system features, achieving semantic equivalence with tsc.

### Currently Implemented ✅

- **Basic generics**: Generic functions and types
- **Unions**: Tagged unions with runtime boxing
- **Interfaces**: Interface declarations and basic checking
- **Enums**: Enum declarations (codegen partial)
- **Primitive types**: Full set including integers, floats, architecture types

### Key Tasks

- **Implement Control Flow Analysis (CFA):**
  - This is the most critical feature for intelligent type narrowing.
  - Implement analysis of `if`, `switch`, `try/catch` blocks, and other control flow statements to refine types within different code branches.

- **Support for Advanced Type Constructs:**
  - **Conditional Types:** Implement logic for `T extends U ? X : Y`.
  - **Mapped Types:** Implement support for creating new types by iterating over the keys of an existing type, e.g., `{ [K in keyof T]: T[K] }`.
  - **Template Literal Types:** Parse and evaluate types based on string template literals.
  - **infer Keyword:** Implement type inference within conditional types.

- **Refine Generic Type Inference:**
  - Improve the type inference engine to correctly deduce generic type arguments from function calls and object instantiations, matching tsc's behavior.

**Outcome:** The Oats type checker will be as powerful and reliable as the official TypeScript compiler, catching the same subtle type errors and providing the same level of safety and developer assistance.

## Phase 3: Comprehensive Runtime & Standard Library

**Goal:** Build out the Oats runtime to provide a complete, Deno-compatible environment, allowing existing TypeScript code to execute correctly.

### Key Tasks

- **Implement All JavaScript Built-in Objects:**
  - Using the Rust runtime, provide complete and spec-compliant implementations for all standard global objects: `Object`, `Array`, `String`, `Promise`, `Map`, `Set`, `RegExp`, `JSON`, `Math`, etc., including all their prototype methods.

- **Implement Web Platform APIs:**
  - Provide native, performant implementations for the core Web APIs that Deno exposes: `console`, `fetch`, `setTimeout`, `setInterval`, `URL`, `TextEncoder`, `EventTarget`, `AbortController`, `WebSocket`, etc.

- **Implement the Full Deno Namespace:**
  - Using the FFI system, build out the entire Deno namespace API, providing the file system, networking, and subprocess functionality that defines the Deno runtime.

- **Garbage Collector Hardening:**
  - Rigorously test the interaction between the GC and the new runtime objects, especially those that hold handles to native resources (like file descriptors or network sockets), to prevent memory leaks.

**Outcome:** Oats will be able to run a vast majority of existing TypeScript and Deno programs correctly, with a complete and performant standard library.

## Alternative Vision: Systems Language Evolution

For reference, there is also a vision to evolve Oats into a systems programming language with granular memory control, FFI, low-level types, and advanced concurrency. This would position Oats as a high-level alternative to Rust/C++ for systems development. See the archived `ROADMAP_SYSTEMS.md` for details on this alternative direction.
