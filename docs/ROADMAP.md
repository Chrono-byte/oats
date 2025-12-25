# Oats Roadmap

## Current State

**Overall:** Pretty solid (4/5)

**What's good:**
- Clean architecture, well-separated concerns
- Good docs
- Safety-focused
- Lots of tests (98+ test functions across 37 files)
- Modern Rust

**What needs work:**
- Error handling could be more consistent
- Some files are too big (`builder.rs` is 1370+ lines)
- Some AST features need codegen implementations

**Recent stuff:**
- Compound assignment operators (`+=`, `-=`, `*=`, etc.)
- Array destructuring (`let [a, b, c] = arr`)
- Major language features (January 2025):
  - Type aliases (`type Name<T> = Type`)
  - Interfaces with properties, methods, index signatures
  - Enums with string/number values
  - Namespaces (`namespace Name { body }`)
  - Optional chaining (`?.`, `?.[]`)
  - Generators and yield (`function*`, `yield`, `yield*`)
  - Super expressions (`super`, `super.prop`, `super.method()`)
  - Type literal types (`{ prop: type }`)
  - Async function parsing
  - Object destructuring patterns

## Phase 1: Core Features

### Done

- Functions, classes, interfaces, enums, generics, unions
- Async/await, destructuring, template literals
- Arrow functions, loops, modules
- Array destructuring (`let [a, b, c] = arr`)
- Object destructuring (`{a, b}`, `{a: x, b: y}`, `{...rest}`)
- Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`, `<<=`, `>>>=`, `&=`, `|=`, `^=`, `**=`)
- Type aliases (`type Name<T> = Type`)
- Interfaces (`interface Name extends Base? { members }`)
- Enums (`enum Name { Variant = value? }`)
- Namespaces (`namespace Name { body }`)
- Optional chaining (`obj?.prop`, `arr?.[0]`, `func?.()`)
- Generators (`function*`, `async function*`)
- Yield expressions (`yield expr`, `yield* expr`)
- Super expressions (`super.prop`, `super.method()`, `super(...)`)
- Type literal types (`{ prop: type, method(): type }`)
- Import/Export statements (AST and parser done)
- Switch statements (`switch (expr) { case ... default }`)
- Try/Catch/Finally (`try { } catch (e) { } finally { }`)

### TODO

- Enum codegen (AST/parser done, needs runtime generation)
- Generic class monomorphization (test_generics fails - needs type param handling)
- Union unboxing for string concatenation (test_unions fails - needs union handling in binary_ops)
- Super property/method access (needs tests)
- Advanced generics
- Mapped/conditional types
- Utility types
- Type guards, Symbol, BigInt

## Missing Language Features

All core language features are implemented in AST and parser. The remaining stuff is advanced TypeScript features that are lower priority.

### Completed (January 2025)

1. Import/Export statements
2. Object destructuring (`{a, b}`, `{a: x}`, `{...rest}`)
3. Async function parsing
4. Type aliases (`type Name<T extends U = V> = Type;`)
5. Interfaces (properties, methods, index signatures, extends)
6. Enums (`enum Name { Variant = value? }`)
7. Optional chaining (`obj?.prop`, `arr?.[0]`)
8. Generators and yield (`function*`, `async function*`, `yield`, `yield*`)
9. Namespaces (`namespace Name { body }`)
10. Type literal types (`{ prop: type, method(): type }`)
11. Super expressions (`super`, `super.prop`, `super.method()`, `super(...)`)
12. Sequence expressions (comma-separated)

### Still Missing (Lower Priority)

- Conditional types: `T extends U ? X : Y`
- Mapped types: `{[K in keyof T]: T[K]}`
- Utility types: `Readonly<T>`, `Partial<T>`, `Required<T>`, `Pick<T, K>`, etc.

Note: Index signatures are supported in interfaces and type literals.

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
- Namespaces (done)
- Generators (done)
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

### Soon
- Enum codegen (AST/parser done)
- Generic constraints
- Access modifiers
- Memory primitives
- Codegen for super property/method access
- Generic class monomorphization (test_generics fails)
- Union unboxing for string concatenation (test_unions fails)

### Later
- FFI
- Concurrency
- Performance

### Eventually
- Full systems features
- IDE support

## Dev Tasks
- Fix RC bugs
- More tests
- Refactor code
- Security audits
