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
- Some missing language features (documented below)
- Error handling could be more consistent in some areas
- Some TODOs require AST changes
- Large files like `builder.rs` (1370+ lines) could be refactored

**Recent Improvements:**
- ✅ Compound assignment operators (`+=`, `-=`, `*=`, etc.)
- ✅ Array destructuring patterns (`let [a, b, c] = arr`)

## Phase 1: Core Features

### Done

- Functions, classes, interfaces, enums, generics, unions
- Async/await, destructuring, template literals
- Arrow functions, loops, modules
- ✅ Array destructuring patterns (`let [a, b, c] = arr`)
- ✅ Compound assignment operators (`+=`, `-=`, `*=`, `/=`, `%=`, `<<=`, `>>=`, `>>>=`, `&=`, `|=`, `^=`, `**=`)

### TODO

- Advanced generics
- Mapped/conditional types
- Utility types
- Type guards, Symbol, BigInt

## Missing Language Features

This section documents language features that are missing from the AST and/or parser implementations.

### Summary

- **AST Missing:** 11 features
- **Parser Missing:** 13 features
- **Both Missing:** 8 features

### High Priority (Blocks Common Use Cases)

#### 1. Import/Export Statements

**Status:** ❌ Missing from both AST and parser

- **Impact:** Critical for module system
- **AST Location:** `Stmt` enum needs `Import` variant
- **Required:** `ImportStmt` struct with source path and import specifiers
- **Parser Location:** `stmt.rs` - needs `import_stmt_parser()`
- **Required:** Parse `import { ... } from "...";` and `import * as X from "...";`

#### 2. Destructuring Patterns (Object)

**Status:** ⚠️ Partially implemented (array destructuring done, object destructuring missing)

- **Current:** Array destructuring `[a, b, c]` is implemented
- **Missing:** Object destructuring `{a, b}`, rest patterns `...rest`
- **AST Location:** `Pat` enum needs variants for `ObjectPat`, `RestPat`
- **Parser Location:** `common.rs` - `pat_parser()` needs object pattern support
- **Impact:** Blocks object destructuring in variable declarations, function parameters, assignments

#### 3. Async Function Parsing

**Status:** ⚠️ Partially implemented

- **Current:** AST has `Function.is_async` field, but parser hardcodes `is_async: false`
- **Parser Location:** `function.rs` - `fn_decl_parser()` and `fn_expr_parser()`
- **Required:** Parse `async function name() { }` and set `is_async: true`

### Medium Priority (Common TypeScript Features)

#### 4. Type Aliases

**Status:** ❌ Missing from both AST and parser

- **AST Location:** `Stmt` enum needs `TypeAlias` variant
- **Required:** `TypeAlias` struct with name, type parameters, and type definition
- **Parser Location:** `stmt.rs` - needs `type_alias_parser()`
- **Required:** Parse `type Name<T> = Type;`

#### 5. Interfaces

**Status:** ❌ Missing from both AST and parser

- **AST Location:** `Stmt` enum needs `InterfaceDecl` variant
- **Required:** `InterfaceDecl` struct with name, extends, and members
- **Parser Location:** `stmt.rs` - needs `interface_decl_parser()`
- **Required:** Parse `interface Name extends Base? { members }`

#### 6. Enums

**Status:** ❌ Missing from both AST and parser

- **AST Location:** `Stmt` enum needs `EnumDecl` variant
- **Required:** `EnumDecl` struct with name and variants (with optional values)
- **Parser Location:** `stmt.rs` - needs `enum_decl_parser()`
- **Required:** Parse `enum Name { Variant = value? }`

#### 7. Optional Chaining

**Status:** ❌ Missing from both AST and parser

- **Evidence:** Common TypeScript feature `obj?.prop`, `arr?.[0]`, `func?.()`
- **AST Location:** `MemberExpr` needs optional flag, or new `OptionalMemberExpr`
- **Parser Location:** `expr.rs` - member access parsing
- **Required:** Support for `?.` operator in member access and calls

### Lower Priority (Advanced Features)

#### 8. Generators and Yield

**Status:** ❌ Missing from both AST and parser

- **Evidence:** Tests show `async function* asyncGenerator() { yield ... }`
- **AST Location:** `Expr` enum needs `Yield` variant
- **Required:** `YieldExpr` struct with optional argument
- **Parser Location:** `function.rs` - needs generator support, `expr.rs` - needs `yield_expr_parser()`
- **Required:** Parse `function* name() { }`, `async function* name() { }`, `yield expr`, `yield* expr`
- **AST Note:** `Function` struct may need `is_generator: bool` field

#### 9. Namespaces

**Status:** ❌ Missing from both AST and parser

- **AST Location:** `Stmt` enum needs `NamespaceDecl` variant
- **Required:** `NamespaceDecl` struct with name and body
- **Parser Location:** `stmt.rs` - needs `namespace_decl_parser()`
- **Required:** Parse `namespace Name { body }`

#### 10. Type Literal Types

**Status:** ⚠️ Partially missing from AST

- **Evidence:** TODO_REVIEW.md mentions "Object literal types not yet supported in oats_ast"
- **Current:** `TsType` enum doesn't have `TsTypeLit` variant
- **AST Location:** `TsType` enum needs `TsTypeLit(TsTypeLit)` variant
- **Required:** `TsTypeLit` struct with properties

#### 11. Advanced Type Features

**Status:** ❌ Missing from AST

- **Missing Types:**
  - Conditional types: `T extends U ? X : Y`
  - Mapped types: `{[K in keyof T]: T[K]}`
  - Index signatures: `[key: string]: number`
  - Readonly/Partial/Required utility types
- **AST Location:** `TsType` enum needs additional variants

#### 12. Super Expressions

**Status:** ⚠️ Partially implemented

- **Current:** AST has `Super` struct and `Callee::Super(Super)` variant
- **Parser Location:** `expr.rs` - needs `super_expr_parser()`
- **Required:** Parse `super.prop`, `super.method()`, `super(...)`

#### 13. Sequence Expressions

**Status:** ⚠️ Unclear if implemented

- **Current:** AST has `SeqExpr` struct
- **Parser Location:** `expr.rs` - needs verification
- **Required:** Parse comma-separated expressions `a, b, c` (not just in function args)

### Implementation Priority

1. **High Priority:**
   - Import statements (critical for module system)
   - Complete object destructuring (needs class field metadata support)
   - Async function parsing (AST supports it, parser doesn't use it)

2. **Medium Priority:**
   - Type aliases
   - Interfaces
   - Enums
   - Optional chaining

3. **Lower Priority:**
   - Generators/yield
   - Namespaces
   - Advanced type features
   - Type literal types

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
