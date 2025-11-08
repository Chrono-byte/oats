# Oats Roadmap

Oats is a systems programming language with low-level control. Currently supports classes, async/await, generics, etc. Parser maintains ~90% syntax compatibility with TypeScript for migration purposes.

## Phase 1: Core Features

### Done

- Functions, classes, interfaces, enums, generics, unions
- Async/await, destructuring, template literals
- Arrow functions, loops, modules

### TODO

- Advanced generics
- Mapped/conditional types
- Utility types
- Type guards, Symbol, BigInt

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

- Decorators, namespaces, generators
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

- Enum codegen
- Generic constraints
- Access modifiers
- Memory primitives

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
