# Project Plan: Achieving Deno FFI Compatibility in Oats

## Objective

To design and implement a Foreign Function Interface (FFI) for the Oats language that is functionally identical to Deno's, enabling seamless interoperability with native libraries written in C-compatible languages like Rust.

## Phase 1: Foundational Type Alignment & Runtime API

### Phase 1 Goal

Establish a formal, explicit contract for supported primitive types within the Oats runtime, mirroring Deno's FFI type table. This moves from an implicit ABI to an explicit, documented FFI foundation.

### Phase 1 Key Tasks

#### Define the OatsFfiType Enum

- In the runtime crate, create a public Rust enum named `OatsFfiType`.
- This enum will have variants for every primitive type supported by Deno's FFI: `I8`, `U8`, `I16`, `U16`, `I32`, `U32`, `I64`, `U64`, `F32`, `F64`, `Pointer`, `Buffer`.
- This creates a single source of truth for all FFI operations.

#### Create a Generic FFI Dispatcher

- Implement a central FFI dispatcher function in `runtime/src/ffi.rs`, e.g., `oats_ffi_call`.
- This function will take a function pointer, an array of argument types (`OatsFfiType`), an array of argument values, and a return type (`OatsFfiType`).
- It will be responsible for the unsafe logic of calling the native function with the correct argument types. This centralizes the unsafe code.

#### Refactor Existing FFI Functions

- Update the existing functions like `print_i32` and `print_f64` to use the new dispatcher. This validates the new system and cleans up the existing ABI.

#### Documentation

- Thoroughly document the `OatsFfiType` enum and the dispatcher function, clearly stating that the goal is 1:1 compatibility with Deno's FFI.

### Phase 1 Outcome

A well-defined and type-safe foundation for FFI within the runtime. The runtime will be ready to handle calls for all of Deno's supported primitive types.

## Phase 2: Compiler and Language Syntax

### Phase 2 Goal

Introduce syntax into the Oats language that allows developers to declare and type-check external native function signatures.

### Phase 2 Key Tasks

#### Design the FFI Declaration Syntax

- Introduce a new top-level statement in Oats, for example:

```typescript
declare external lib "path/to/lib.so" {
  function add(a: i32, b: i32): i32;
  function print_message(msg: pointer): void;
}
```

- The types used (`i32`, `pointer`, etc.) will correspond directly to the `OatsFfiType` enum variants.

#### Update the Parser

- Modify the parser in `oatsc/src/parser.rs` to recognize and parse this new `declare external` block.
- The parser should produce a new AST node representing the external library and its function signatures.

#### Integrate with the Type System

- Update the type checker in `oatsc/src/types.rs` to validate the FFI declarations.
- It should ensure that the types used in the signatures are valid FFI types.
- The type checker will add these function signatures to the global scope, making them available to the rest of the Oats code.

### Phase 2 Outcome

Oats code can now safely declare and type-check native function signatures, preventing type mismatch errors at compile time.

## Phase 3: Codegen and Dynamic Loading

### Phase 3 Goal

Generate the necessary code to dynamically load native libraries and call functions within them at runtime.

### Phase 3 Key Tasks

#### Implement Dynamic Library Loading

- Add a native library loading mechanism to the Oats runtime using a crate like `libloading`.
- The runtime will maintain a cache of loaded libraries to avoid redundant loading.

#### Update Code Generation

- Modify the `oatsc/src/codegen/` modules. When the codegen encounters a call to an external function, it should:
  - Emit a call to the runtime's FFI dispatcher (`oats_ffi_call`).
  - Pass the function name, library path, and the FFI types of the arguments and return value.

#### Build Process Integration

- Ensure that the `oatsc/src/builder.rs` correctly links the Oats executable with the necessary runtime components for FFI and dynamic loading.

### Phase 3 Outcome

The Oats compiler can now generate executables that dynamically load and call native functions, with the runtime handling the low-level details of the call.

## Phase 4: Advanced Features - Buffers and Pointers

### Phase 4 Goal

Implement support for complex data types like buffers and pointers, which are essential for any non-trivial FFI use case.

### Phase 4 Key Tasks

#### Buffer Support (TypedArray)

- Define the mechanism for passing Oats TypedArrays (like `Uint8Array`) to native code.
- This will involve passing a pointer to the start of the array's data and its length.
- Crucially, the Oats runtime must "pin" the array in memory during the FFI call to prevent the garbage collector from moving or freeing it while the native code is using it.

#### Opaque Pointer Support

- Implement a special `OpaquePointer` type in Oats that can hold a raw memory address from a native library.
- This is essential for interacting with C-style object-oriented APIs (e.g., `create_object() -> pointer`, `process_object(p: pointer)`).
- The Oats garbage collector will ignore these pointers, and it will be the developer's responsibility to manage the lifetime of the underlying native resource.

#### End-to-End Testing

- Create a simple C or Rust library with functions that use all supported FFI types, including pointers and buffers.
- Write a comprehensive suite of Oats tests that call these functions and validate the results, ensuring that the entire FFI system is working correctly and safely.

### Phase 4 Outcome

Oats will have a complete, Deno-compliant FFI that is powerful, safe, and ready for advanced systems programming tasks.
