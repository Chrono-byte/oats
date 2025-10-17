# Oats Compiler Enhancement TODO List

**Last Updated:** October 17, 2025

This TODO list outlines potential improvements and tasks for the Oats compiler, prioritized for **full TypeScript compatibility**. Focus is on implementing missing language features and type system capabilities before performance optimizations and tooling.

**TypeScript Conformance Status:** As of October 17, 2025, the parser achieves ~90.43% success rate on TypeScript conformance tests (4,547/5,028 files parsed successfully). The remaining failures are primarily due to missing support for advanced ES6+ features listed below.

## 1. Complete TypeScript Feature Parity

This section tracks TypeScript features not yet implemented in Oats to achieve full language compatibility.

### 1.1 Advanced Type System Features

- **Enums (partial)**: Enum declarations are parsed and type-checked, but enum member access and value emission in codegen is not implemented.
- **Advanced generics**:
  - Generic constraints (`T extends U`)
  - Multiple type parameters (`<T, U>`)
  - Generic classes and interfaces
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
- **Async/await functions**: No `async` keyword on functions/methods or `await` expressions.
- **Destructuring**: No array/object destructuring in variable declarations, function parameters, or assignments.
- **Template literals**: No template strings with `${}` interpolation or tagged templates.
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

## 2. Expand Language Support

- Add more TypeScript features (e.g., decorators, namespaces, modules).
- Support for concurrency primitives beyond async/await.
- Add support for generators (`function*` / `yield`).
- Add support for JSX/TSX.
- Implement declaration merging and module augmentation.
- Add support for advanced type-level features (conditional types, mapped types, etc.).

## 3. Implement Missing Features

- Extend generics support (e.g., constraints, multiple type parameters).
- Add full interface implementation checking.

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
