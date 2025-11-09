# Oats Roadmap

Oats is a systems programming language with low-level control. Currently supports classes, async/await, generics, etc. Parser maintains ~90% syntax compatibility with TypeScript for migration purposes.

## Overall Compiler Assessment

**Overall Assessment:** ⭐⭐⭐⭐ (4/5)

**Strengths:**

- Clean architecture with well-separated concerns
- Excellent documentation
- Strong focus on safety and security
- Comprehensive test coverage (98+ test functions across 37 test files)
- Modern Rust practices

**Areas for Improvement:**

- Error handling could be more consistent in some areas
- Large files like `builder.rs` (1370+ lines) could be refactored
- Codegen implementation needed for newly added AST features

**Recent Improvements:**

- ✅ Compound assignment operators (`+=`, `-=`, `*=`, etc.)
- ✅ Array destructuring patterns (`let [a, b, c] = arr`)
- ✅ **Major Language Feature Completion (January 2025):**
  - ✅ Type aliases (`type Name<T> = Type`)
  - ✅ Interfaces with properties, methods, and index signatures
  - ✅ Enums with string/number values
  - ✅ Namespaces (`namespace Name { body }`)
  - ✅ Optional chaining (`?.`, `?.[]`)
  - ✅ Generators and yield (`function*`, `yield`, `yield*`)
  - ✅ Super expressions (`super`, `super.prop`, `super.method()`)
  - ✅ Type literal types (`{ prop: type }`)
  - ✅ Async function parsing (fully implemented)
  - ✅ Object destructuring patterns (AST and parser complete)

## Phase 1: Core Features

### Done

- Functions, classes, interfaces, enums, generics, unions
- Async/await, destructuring, template literals
- Arrow functions, loops, modules
- ✅ Array destructuring patterns (`let [a, b, c] = arr`)
- ✅ Object destructuring patterns (`{a, b}`, `{a: x, b: y}`, `{...rest}`)
- ✅ Compound assignment operators (`+=`, `-=`, `*=`, `/=`, `%=`, `<<=`, `>>=`, `>>>=`, `&=`, `|=`, `^=`, `**=`)
- ✅ Type aliases (`type Name<T> = Type`)
- ✅ Interfaces (`interface Name extends Base? { members }`)
- ✅ Enums (`enum Name { Variant = value? }`)
- ✅ Namespaces (`namespace Name { body }`)
- ✅ Optional chaining (`obj?.prop`, `arr?.[0]`, `func?.()`)
- ✅ Generators (`function*`, `async function*`)
- ✅ Yield expressions (`yield expr`, `yield* expr`)
- ✅ Super expressions (`super.prop`, `super.method()`, `super(...)`)
- ✅ Type literal types (`{ prop: type, method(): type }`)
- ✅ Import/Export statements (AST and parser complete)

### TODO

- Advanced generics
- Mapped/conditional types
- Utility types
- Type guards, Symbol, BigInt

## Missing Language Features

This section documents language features that are missing from the AST and/or parser implementations.

### Summary

- **AST Missing:** 0 features (all core features implemented!)
- **Parser Missing:** 0 features (all core features implemented!)
- **Both Missing:** 0 features

**Status:** ✅ **All core language features are now implemented in AST and parser!**

### Remaining Advanced Features

The following advanced TypeScript features are not yet implemented but are lower priority:

### ✅ Completed Features (January 2025)

All of the following features have been fully implemented in both AST and parser:

1. ✅ **Import/Export Statements** - Complete AST and parser support
2. ✅ **Object Destructuring** - Full support for `{a, b}`, `{a: x}`, `{...rest}` patterns
3. ✅ **Async Function Parsing** - Parser now correctly sets `is_async: true`
4. ✅ **Type Aliases** - `type Name<T extends U = V> = Type;`
5. ✅ **Interfaces** - Full support with properties, methods, index signatures, and extends
6. ✅ **Enums** - `enum Name { Variant = value? }` with string/number values
7. ✅ **Optional Chaining** - `obj?.prop`, `arr?.[0]` support
8. ✅ **Generators and Yield** - `function*`, `async function*`, `yield`, `yield*`
9. ✅ **Namespaces** - `namespace Name { body }`
10. ✅ **Type Literal Types** - `{ prop: type, method(): type }`
11. ✅ **Super Expressions** - `super`, `super.prop`, `super.method()`, `super(...)`
12. ✅ **Sequence Expressions** - Comma-separated expressions supported

### Remaining Advanced Type Features

The following advanced TypeScript type system features are not yet implemented:

#### Advanced Type Features

**Status:** ❌ Not yet implemented (lower priority)

- **Missing Types:**
  - Conditional types: `T extends U ? X : Y`
  - Mapped types: `{[K in keyof T]: T[K]}`
  - Utility types: `Readonly<T>`, `Partial<T>`, `Required<T>`, `Pick<T, K>`, etc.
- **Note:** Index signatures are supported in interfaces and type literals
- **AST Location:** `TsType` enum would need additional variants for these

## Phase 2: Systems Stuff

### Memory & Types

- Manual memory management
- Pointers, raw access
- Stack structs
- Unowned refs
- isize/usize, i8/u8/f32, char

### FFI

- Call C libs
- Structs by value
- Callbacks
- Unsafe pointers

### Concurrency

- OS threads
- Mutex, atomics
- Beyond async

### Classes

- Private/protected
- Static members
- Abstract classes
- Getters/setters

### Other

- ✅ Namespaces (implemented)
- ✅ Generators (implemented)
- Decorators
- Exponentiation, for-await

## Phase 3: Runtime & Tools

- Better GC/manual options
- FFI bindings
- Systems stdlib
- Package manager (Oats.toml)
- Cargo-like build tool
- Language server

## Priorities

### Next Weeks

- ✅ Enum AST/parser (complete)
- Enum codegen
- Generic constraints
- Access modifiers
- Memory primitives
- Codegen for newly added features (interfaces, type aliases, namespaces, etc.)

### Months

- FFI
- Concurrency
- Performance

### Long Term

- Full systems features
- IDE support

## Dev Stuff

- Fix RC bugs
- More tests
- Refactor code
- Security audits
