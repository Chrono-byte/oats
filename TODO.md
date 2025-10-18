# Oats Compiler Enhancement TODO List

**Last Updated:** October 18, 2025

This TODO list outlines potential improvements and tasks for the Oats compiler, prioritized for **full TypeScript compatibility**. Focus is on implementing missing language features and type system capabilities before performance optimizations and tooling.

**Current Implementation Status:** As of October 18, 2025, Oats supports a substantial subset of TypeScript including classes, async/await, generics, unions, interfaces, enums, destructuring, and template literals. The parser achieves ~90.43% success rate on TypeScript conformance tests (4,547/5,028 files parsed successfully).

## 1. Complete TypeScript Feature Parity

This section tracks TypeScript features not yet implemented in Oats to achieve full language compatibility.

### 1.1 Advanced Type System Features

- **Advanced generics**:
  - Generic constraints (`T extends U`) - NOT IMPLEMENTED
  - Multiple type parameters (`<T, U>`) - NOT IMPLEMENTED
  - Generic classes and interfaces - PARTIALLY IMPLEMENTED (basic generics work)
- **Full interface implementation checking**: No enforcement of class implementation contracts.
- **Mapped types**: No `keyof`, `in`, or mapped type syntax (e.g., `{ [K in keyof T]: ... }`).
- **Conditional types**: No `T extends U ? X : Y` syntax.
- **Utility types**: No built-in types like `Partial<T>`, `Pick<T>`, `Record<K, V>`.
- **Template literal types**: No string manipulation at the type level.
- **Type guards and assertions**: No `is` operator, `as` assertions, or user-defined type guards.
- **Symbol and BigInt**: No `symbol` or `bigint` types.
- **More literal types**: Limited support beyond basic literals (e.g., no literal unions like `'a' | 'b'`).

### 1.2 Class and Object Features

- **Access modifiers**: Only `public` properties shown; no `private`, `protected`, or `#` private fields.
- **Static members**: No static properties or methods on classes.
- **Abstract classes**: No `abstract` keyword or abstract methods.
- **Getters/setters**: No `get`/`set` property accessors.
- **Method overloading**: No support for multiple method signatures.
- **Static blocks**: No `static { ... }` syntax for static initialization.
- **Index signatures**: No `[key: string]: Type` or `[key: number]: Type` for dynamic property access.
- **Constructor parameter properties**: No automatic property creation from constructor parameters.
- **Auto-accessors**: No `accessor` keyword for automatic getter/setter generation.

### 1.3 Language Constructs

- **Decorators**: No `@decorator` syntax for metadata/annotation.
- **Namespaces**: No `namespace` declarations for organizing code.
- **Generators**: No `function*` or `yield` for iterators.
- **JSX/TSX**: No support for XML-like syntax in templates.
- **Declaration merging**: No ability to merge interfaces or modules across declarations.
- **Module augmentation**: No `declare module` for extending external modules.
- **Ambient declarations**: No `declare` for describing external APIs.
- **Triple-slash directives**: No `/// <reference />` for compiler hints.
- **Exponentiation operator**: No `**` or `**=` operators.
- **For-await-of loops**: No `for await (const x of iterable)` syntax.
- **Numeric separators**: No underscore separators in numeric literals (e.g., `1_000`).
- **Using declarations**: No `using` or `await using` for resource management.

### 1.4 Advanced Control Flow and Concurrency

- **Concurrency beyond async/await**: No additional primitives like `Promise.all`, advanced scheduling, or concurrency models.
- **Error handling**: Basic try/catch may be limited (not explicitly shown in examples).
- **Advanced loops**: No `for...of`, `for...in`, `for await...of`, or labeled breaks/continues beyond basic `for`.

### 1.5 Runtime and Dynamic Features

- **Dynamic evaluation**: No `eval`, dynamic imports, or runtime code generation (intentionally omitted for AOT).
- **Reflection/introspection**: No runtime type information or `typeof` beyond basic checks.
- **Global objects**: Limited standard library; no full DOM, Node.js, or browser APIs.
- **Modules**: Static import/export supported, but no dynamic `import()`, import assertions/attributes, type-only imports (`import type`), re-exports, namespace imports, or verbatim module syntax.

### 1.6 Tooling and Ecosystem

- **Full module system**: Basic import/export, but no support for all module formats (e.g., no CommonJS, AMD).
- **TypeScript compiler options**: No `tsconfig.json` features like strict mode, path mapping, or advanced checks.
- **IDE support**: No language server mentioned yet (planned for mid-term).

## 2. Currently Implemented Features (Updated Status)

### 2.1 Core Language Features ✅ IMPLEMENTED

- **Async/await**: Full support for `async` functions and `await` expressions
- **Classes**: Class declarations, inheritance, constructors, methods, fields
- **Destructuring**: Array and object destructuring in variable declarations and assignments
- **Template literals**: String interpolation with `${}` syntax
- **Arrow functions**: Full support including captured variables
- **Generics**: Basic generic functions and types
- **Unions**: Tagged unions with runtime boxing/unboxing
- **Interfaces**: Interface declarations and basic type checking
- **Enums**: Enum declarations with type checking (codegen partial)
- **Tuples**: Fixed-size tuple types
- **Promises**: Promise type and async lowering to state machines

### 2.2 Type System ✅ IMPLEMENTED

- **Primitive types**: `number`, `boolean`, `string`, `void`
- **Integer types**: `i8`, `u8`, `i16`, `u16`, `i32`, `u32`, `i64`, `u64`
- **Float types**: `f32`, `f64`
- **Architecture types**: `isize`, `usize`
- **Character type**: `char`
- **Reference types**: `Weak<T>`, `Unowned<T>` (planned)
- **Optional types**: Nullable/undefined handling

### 2.3 Control Flow ✅ IMPLEMENTED

- **Conditionals**: `if`/`else` statements and ternary expressions
- **Loops**: `for`, `while`, `do-while` loops
- **Break/continue**: Loop control statements
- **Return**: Function return statements
- **Blocks**: Statement blocks and scoping

### 2.4 Expressions ✅ IMPLEMENTED

- **Binary operations**: All standard arithmetic, comparison, logical operators
- **Unary operations**: `-`, `+`, `!`, `~`, `typeof`
- **Update operations**: `++`, `--` (prefix and postfix)
- **Assignment**: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, etc.
- **Member access**: Dot notation and computed member access
- **Function calls**: Regular and method calls
- **Literals**: Numbers, strings, booleans, arrays, objects
- **New expressions**: Constructor calls
- **Parenthesized expressions**: Grouping

## 3. High-Priority Missing Features

### 3.1 Immediate Priorities (Next 1-2 weeks)

- **Enum codegen**: Complete enum member access and value emission
- **Generic constraints**: Support for `T extends U` syntax
- **Access modifiers**: `private`, `protected`, `#` private fields
- **Static members**: Static properties and methods
- **Abstract classes**: `abstract` keyword and abstract methods

### 3.2 Short-term Goals (Next 1-3 months)

- **Advanced generics**: Multiple type parameters, generic classes
- **Mapped types**: `keyof`, `in`, mapped type syntax
- **Conditional types**: `T extends U ? X : Y`
- **Decorators**: `@decorator` syntax
- **JSX/TSX**: XML-like syntax support

### 3.3 Long-term Vision (6+ months)

- **Full TypeScript compatibility**: 100% parser success rate
- **Advanced type system**: All TypeScript type features
- **Rich standard library**: Deno-compatible runtime APIs
- **FFI integration**: Seamless C/Rust interop
- **IDE support**: Language server and tooling

## 4. Critical Short-term Priorities (Next 2-4 Weeks)

### 4.1 RC Memory Management Audit

- **Audit all lowering sites that allocate heap temporaries**: template literals, string concatenation, `number_to_string`, `union_box_*`, `array`/`tuple`/object literal lowering, and `println`/printing helpers.
- **Emit missing `rc_dec` where temporaries are created and ownership is not transferred**.
- **Add focused tests (IR string-contains or insta snapshots) for each pattern to prevent regressions**.

### 4.2 Code Quality and Warnings

- Clean up any remaining compiler warnings (unused assignments) found in `stmt.rs` and other files.
- Run `cargo clippy --workspace` and fix critical lints.
- **Fix control flow error reporting**: Currently `continue` outside of loop is ignored; should emit diagnostic (see TODO in `stmt.rs`).

### 4.3 Testing and Integration

- Run `./scripts/run_fuzzing.sh` for fuzz targets and `./scripts/run_all_proper_tests.sh` to compile all proper_tests examples and catch regressions.
- Implement performance benchmarks for codegen speed.
- Expand snapshot tests for edge cases.

## 5. Developer Experience & Tooling

- Create a language server for IDE support.
- Add build scripts for easier setup.
- Generate API docs and usage guides.
- Improve error diagnostics with source spans and suggestions.
- Add debugging/logging features for memory tracking.

## 6. Documentation and Examples

- Add comprehensive documentation and examples.
- Update `docs/DEVELOPMENT.md` and `docs/ARCHITECTURE.md` with ownership/RC rules, object layout invariants, and runtime ABI contracts.
- Create more examples and tutorials.

## 7. Optimize Performance

- Profile and optimize LLVM IR generation (e.g., reduce redundant allocations).
- Implement generational GC with nursery and mature object spaces.
- Investigate redundant RC elimination and escape analysis opportunities.
- Improve the cycle collector's coverage and add stress tests.
- Add more built-in functions (e.g., advanced string/array ops).

## 8. Rapid Type Analysis (RTA) Implementation

This section tracks the phased implementation of Rapid Type Analysis for ahead-of-time optimizations.

### Phase 1: Analysis - Building the Foundation (Weeks 1-5)

- **Milestone 1.2: Call Graph Construction and Worklist Algorithm (Weeks 4-5)**
  - Implement call graph data structure to represent method calls.
  - Implement iterative RTA worklist algorithm starting from main, processing method calls to find potential targets.

### Phase 2: Optimization - Applying the Analysis (Weeks 6-11)

- **Milestone 2.1: Devirtualization of Method Calls (Weeks 6-9)**
  - Modify codegen to query RTA results for devirtualization of method calls.
  - Implement logic to emit direct static calls when only one possible method target.
  - Create micro-benchmarks to measure virtual vs direct call performance.

### Phase 3: Validation and Refinement (Weeks 12-14)

- **Milestone 3.1: Integration Testing (Weeks 12-13)**
  - Apply RTA to existing test suite including deno_tests to ensure no regressions.
  - Create larger test application with classes, inheritance, methods to validate optimizer.
- **Milestone 3.2: Final Report and Documentation (Week 14)**
  - Perform final performance and code size measurements on benchmarks.
  - Write internal documentation for RTA module, design, integration, diagnostics.

## 9. Refactor and Maintain Code

- Modularize large files (e.g., split `expr.rs` into submodules).
- Update dependencies (e.g., bump `inkwell` for newer LLVM versions).

## 10. Security and Safety Audits

- Audit for buffer overflows or unsafe operations.
- Harden recursion limits and add more safety checks.

## 11. Community and Ecosystem

- Add package management for dependencies.
- Integrate with existing TS tooling (e.g., via plugins).

## 12. Release Preparation

- Once the rc_dec audit, snapshot tests, and lints are green, prepare a release-candidate branch that includes:
  - A summary of ownership/RC guarantees and codegen invariants
  - Updated tests and any new snapshots
  - Short smoke-test log showing proper_tests/examples pass
