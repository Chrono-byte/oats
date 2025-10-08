# Roadmap to Real TypeScript Support

## Executive Summary

**Current State:** Oats can compile simple, self-contained TypeScript programs with basic classes, arrays, loops, and functions. However, it cannot run "off the shelf" TypeScript code from the internet.

**Compatibility Estimate:**
- **Today:** <1% of npm packages would work
- **After Phase 1:** ~10-15% of simple TypeScript code
- **After Phase 2:** ~40-50% of typical applications
- **Production Ready:** 3-5 years of development

---

## ✅ What Works Today (v0.1.0)

### Type System
- [x] Basic types: `number`, `boolean`, `string`, `void`
- [x] Arrays: `number[]`, `string[]` (homogeneous only)
- [x] Nominal classes: `class Foo { ... }`
- [x] Type annotations on parameters and returns

### Language Features
- [x] Named functions with strict type checking
- [x] Classes with constructors, fields, methods
- [x] `this` binding in methods
- [x] Control flow: `if/else`, `for`, `while`, `do-while`, `for-of`
- [x] Labeled `break` and `continue`
- [x] Binary operators: arithmetic, comparison, logical (&&, ||)
- [x] String concatenation
- [x] Array literals and indexing
- [x] Member access: `obj.field` (read/write)
- [x] Object construction: `new ClassName(args)`
- [x] Constructor parameters: `constructor(public x: number)`

### Module System
- [x] Export declarations: `export function`, `export class`
- [x] Import declarations parsed (types registered)
- [ ] **NOT SUPPORTED:** Cross-file compilation, module resolution

### Memory Management
- [x] Reference counting for heap objects
- [x] Automatic RC increment/decrement
- [x] Runtime bounds checking for arrays

### Tooling
- [x] LLVM IR generation
- [x] AOT compilation to native code
- [x] Basic diagnostics with source spans
- [x] Test suite (21 tests passing)

---

## ❌ Critical Missing Features

This section lists features **blocking** real-world TypeScript usage, ordered by impact.

---

## 🔴 Phase 1: Core Language Features (6-12 months)

**Goal:** Support simple npm packages and common TypeScript patterns  
**Target Compatibility:** 10-15% of TypeScript code

### 1. Arrow Functions (Non-closing) 🔴 **CRITICAL**
**Priority:** P0 | **Effort:** 1-2 weeks | **Complexity:** Low

**Current State:**
```typescript
// ❌ NOT SUPPORTED
const add = (x: number, y: number) => x + y;
const double = (x: number) => x * 2;
```

**What's Needed:**
- [ ] Parse `ast::Expr::Arrow` in expression lowering
- [ ] Generate function symbol for arrow expression
- [ ] Lower arrow body (both expression and block forms)
- [ ] Handle implicit returns for expression bodies
- [ ] Store arrow function as callable value (function pointer)

**Implementation Notes:**
- Start with non-capturing arrows (treat as regular functions)
- Arrow without captures can be lowered to static function + pointer
- Closures with capture deferred to Phase 2

**Tests:**
- [ ] Simple arrow: `const f = (x: number) => x + 1`
- [ ] Arrow with block: `const f = (x: number) => { return x + 1; }`
- [ ] Arrow as callback: `arr.forEach((x) => print_f64(x))`

---

### 2. Interfaces & Type Aliases 🔴 **CRITICAL**
**Priority:** P0 | **Effort:** 2-3 weeks | **Complexity:** Medium

**Current State:**
```typescript
// ❌ NOT SUPPORTED
interface User {
    name: string;
    age: number;
}
type Point = { x: number; y: number };
```

**What's Needed:**
- [ ] Parse `TsInterfaceDecl` and register as nominal type
- [ ] Parse `TsTypeAliasDecl` and expand to underlying type
- [ ] Extend `map_ts_type()` to handle interface references
- [ ] Support object type literals: `{ field: type }`
- [ ] Structural type checking (duck typing) for interfaces
- [ ] Generate struct layouts for interface types

**Implementation Notes:**
- Start with interfaces as nominal structs (nominal typing)
- Type aliases as transparent aliases (expand at use site)
- Structural typing (duck typing) is harder - defer to later

**Tests:**
- [ ] Interface declaration and usage
- [ ] Type alias for primitive types
- [ ] Type alias for object types
- [ ] Function parameter with interface type

---

### 3. Union Types (Basic) 🟡 **HIGH**
**Priority:** P1 | **Effort:** 2-3 weeks | **Complexity:** Medium

**Current State:**
```typescript
// ❌ NOT SUPPORTED
function process(value: string | number) { }
let result: string | null = null;
```

**What's Needed:**
- [ ] Extend `OatsType` enum: `Union(Vec<OatsType>)`
- [ ] Parse `TsUnionType` in `map_ts_type()`
- [ ] Tagged union representation (discriminator + payload)
- [ ] Runtime type guards: `typeof`, `instanceof`
- [ ] Type narrowing in conditionals
- [ ] Coercion rules for unions

**Implementation Notes:**
- Use tagged union: `{ tag: i32, data: union { ... } }`
- Store tag in high bits of pointer for pointer types
- Primitive unions need full discriminator word
- Start with simple unions (2-3 types max)

**Tests:**
- [ ] `string | number` parameter
- [ ] `T | null` nullable types
- [ ] Type guard: `if (typeof x === 'string')`
- [ ] Union array: `Array<string | number>`

---

### 4. Object Literals 🟡 **HIGH**
**Priority:** P1 | **Effort:** 1-2 weeks | **Complexity:** Low-Medium

**Current State:**
```typescript
// ❌ NOT SUPPORTED
const point = { x: 1, y: 2 };
const user = { name: "Alice", age: 30 };
```

**What's Needed:**
- [ ] Parse `ast::Expr::Object` (ObjectLit)
- [ ] Infer anonymous struct type from literal
- [ ] Allocate struct on heap
- [ ] Initialize fields from literal values
- [ ] Support shorthand properties: `{ x, y }`
- [ ] Support computed property names: `{ [key]: value }`

**Implementation Notes:**
- Generate anonymous struct type per literal shape
- Cache struct types by field signature for reuse
- All object literals are heap-allocated
- RC manage object lifetimes

**Tests:**
- [ ] Simple object literal: `{ x: 1, y: 2 }`
- [ ] Nested objects: `{ point: { x: 1, y: 2 } }`
- [ ] Object with methods: `{ getName() { return "Alice"; } }`
- [ ] Shorthand: `const x = 1; const obj = { x };`

---

### 5. Template Literals 🟢 **MEDIUM**
**Priority:** P2 | **Effort:** 1 week | **Complexity:** Low

**Current State:**
```typescript
// ❌ NOT SUPPORTED
const msg = `Hello ${name}!`;
const multiline = `Line 1
Line 2`;
```

**What's Needed:**
- [ ] Parse `ast::Expr::Tpl` (Template)
- [ ] Extract quasis (string parts) and expressions
- [ ] Convert expressions to strings (toString runtime helper)
- [ ] Concatenate parts using `str_concat`
- [ ] Handle escape sequences

**Implementation Notes:**
- Lower to series of `str_concat` calls
- Add runtime `to_string()` for number→string conversion
- Multiline strings already work in string literals

**Tests:**
- [ ] Simple interpolation: `` `x = ${x}` ``
- [ ] Multiple expressions: `` `${a} + ${b} = ${a+b}` ``
- [ ] Nested templates
- [ ] Tagged templates (advanced - defer)

---

### 6. Module Resolution & Multi-file Compilation 🔴 **CRITICAL**
**Priority:** P0 | **Effort:** 4-6 weeks | **Complexity:** High

**Current State:**
```typescript
// ❌ NOT SUPPORTED (parsed but not resolved)
import { readFile } from 'fs';
import utils from './utils';
```

**What's Needed:**
- [ ] Module resolver (Node.js algorithm)
- [ ] Find .ts/.tsx/.d.ts files on disk
- [ ] Handle relative imports: `./foo`, `../bar`
- [ ] Handle package imports: `lodash`, `@types/node`
- [ ] Parse imported modules recursively
- [ ] Build dependency graph (detect cycles)
- [ ] Topological sort for compilation order
- [ ] Cross-module symbol resolution
- [ ] Link-time symbol table merging
- [ ] Default imports: `import foo from './foo'`
- [ ] Namespace imports: `import * as foo from './foo'`
- [ ] Re-exports: `export { foo } from './bar'`

**Implementation Notes:**
- Start with relative imports only (./foo)
- Use simple file-system resolver (no node_modules yet)
- Build module graph before codegen phase
- Each module produces separate LLVM module
- Link modules at final compilation stage
- Defer node_modules resolution to later

**Tests:**
- [ ] Import function from another file
- [ ] Import class from another file
- [ ] Circular dependency detection
- [ ] Default vs named imports
- [ ] Re-exports

**Blockers:**
- This is the biggest architectural change
- May require refactoring compilation pipeline
- Consider incremental compilation strategy

---

### 7. Standard Library Basics 🟡 **HIGH**
**Priority:** P1 | **Effort:** 2-4 weeks | **Complexity:** Medium

**Current State:**
```typescript
// Only custom runtime functions work:
print_f64(42);
print_str("hello");

// ❌ NOT SUPPORTED:
console.log("hello");
Math.random();
Array.prototype.map();
```

**What's Needed:**
- [ ] `console.log()` - varargs, format any type
- [ ] `console.error()`, `console.warn()`
- [ ] `Math.random()`, `Math.floor()`, `Math.ceil()`, `Math.abs()`
- [ ] `Array.prototype.push()`, `pop()`, `shift()`, `unshift()`
- [ ] `Array.prototype.map()`, `filter()`, `reduce()` (needs closures)
- [ ] `String.prototype.length`, `charAt()`, `substring()`
- [ ] `Object.keys()`, `Object.values()`
- [ ] `JSON.stringify()`, `JSON.parse()` (hard - defer)

**Implementation Notes:**
- Start with intrinsics (compiler builtins)
- Math functions can link to libc `<math.h>`
- Array methods need runtime implementation
- Higher-order functions (map/filter) need closures (Phase 2)
- JSON is very complex - low priority

**Tests:**
- [ ] `console.log()` with multiple args
- [ ] Math functions: `Math.floor(3.7) === 3`
- [ ] Array mutation: `arr.push(42)`
- [ ] String methods: `"hello".length === 5`

---

## 🟡 Phase 2: Advanced Features (6-12 months)

**Goal:** Support typical application code, framework basics  
**Target Compatibility:** 40-50% of TypeScript code

### 8. Closures with Capture 🔴 **CRITICAL**
**Priority:** P0 (Phase 2) | **Effort:** 4-6 weeks | **Complexity:** High

**Current State:**
```typescript
// ❌ NOT SUPPORTED
function makeCounter() {
    let count = 0;
    return () => count++;  // captures 'count'
}
```

**What's Needed:**
- [ ] Escape analysis: detect captured variables
- [ ] Closure environment struct allocation
- [ ] Box captured variables on heap
- [ ] Generate closure thunk (trampoline)
- [ ] Pass environment pointer to closure body
- [ ] Lifetime analysis for closure validity
- [ ] Nested closures (closures capturing closures)

**Implementation Notes:**
- Closure = { fn_ptr, env_ptr }
- Captured vars stored in heap-allocated struct
- Mutable captures need RefCell-like indirection
- Consider Rust-style borrow checker for safety
- This is a major feature - allocate significant time

**Tests:**
- [ ] Simple counter closure
- [ ] Multiple captured variables
- [ ] Mutable capture: `() => count++`
- [ ] Closure returned from function
- [ ] Nested closures

---

### 9. Generics (Monomorphization) 🔴 **CRITICAL**
**Priority:** P0 (Phase 2) | **Effort:** 6-8 weeks | **Complexity:** Very High

**Current State:**
```typescript
// ❌ NOT SUPPORTED
function identity<T>(x: T): T { return x; }
class Box<T> { constructor(public value: T) {} }
```

**What's Needed:**
- [ ] Parse type parameters: `<T, U extends Base>`
- [ ] Track generic types in `OatsType`
- [ ] Monomorphization: generate specialized code per concrete type
- [ ] Cache monomorphized functions (avoid duplicates)
- [ ] Generic constraints: `T extends Base`
- [ ] Generic classes and methods
- [ ] Type parameter inference

**Implementation Notes:**
- Start with simple unconstrained generics
- Monomorphization = template instantiation (C++ style)
- Alternative: type erasure (Java style) - simpler but slower
- Generic arrays already work (`Array<T>` is nominal)
- This is PhD-level compiler work

**Tests:**
- [ ] Generic function: `identity<number>(42)`
- [ ] Generic class: `new Box<string>("hello")`
- [ ] Type inference: `identity(42)` infers `T=number`
- [ ] Generic constraints

---

### 10. Async/Await & Promises 🔴 **CRITICAL**
**Priority:** P0 (Phase 2) | **Effort:** 8-12 weeks | **Complexity:** Very High

**Current State:**
```typescript
// ❌ NOT SUPPORTED
async function fetchData(): Promise<string> {
    const response = await fetch(url);
    return response.text();
}
```

**What's Needed:**
- [ ] `Promise<T>` type representation
- [ ] Async function state machine lowering
- [ ] `await` point identification (suspension points)
- [ ] Runtime scheduler/executor
- [ ] Event loop integration
- [ ] Promise chaining: `.then()`, `.catch()`
- [ ] `async`/`await` syntax sugar

**Implementation Notes:**
- Lower `async fn` to state machine (Rust/C# style)
- Each `await` becomes a yield point
- Need runtime with:
  - Task queue
  - Event loop
  - Future/Promise abstraction
- Consider using existing runtime (Tokio-style)
- This is a massive undertaking - consider deferring

**Tests:**
- [ ] Simple async function
- [ ] Sequential awaits
- [ ] Parallel promises: `Promise.all()`
- [ ] Error handling in async

**Blockers:**
- Requires runtime scheduler (not just LLVM IR)
- May need OS integration (epoll, kqueue, IOCP)
- Consider WebAssembly target for browser async

---

### 11. Destructuring 🟢 **MEDIUM**
**Priority:** P2 | **Effort:** 3-4 weeks | **Complexity:** Medium

**Current State:**
```typescript
// ❌ NOT SUPPORTED
const { x, y } = point;
const [first, second] = array;
function fn({ name, age }: User) { }
```

**What's Needed:**
- [ ] Parse destructuring patterns: `Pat::Object`, `Pat::Array`
- [ ] Lower to multiple assignments
- [ ] Handle nested destructuring
- [ ] Rest patterns: `const { x, ...rest } = obj`
- [ ] Default values: `const { x = 0 } = obj`
- [ ] Destructuring in function parameters

**Tests:**
- [ ] Object destructuring
- [ ] Array destructuring
- [ ] Nested destructuring
- [ ] Rest operator
- [ ] Parameter destructuring

---

### 12. Optional & Default Parameters 🟢 **MEDIUM**
**Priority:** P2 | **Effort:** 1-2 weeks | **Complexity:** Low

**Current State:**
```typescript
// ❌ NOT SUPPORTED
function greet(name?: string, age: number = 18) { }
```

**What's Needed:**
- [ ] Parse optional parameters: `name?: string`
- [ ] Parse default values: `age = 18`
- [ ] Generate initialization code for defaults
- [ ] Handle undefined/null for optionals
- [ ] Overload resolution (multiple signatures)

**Tests:**
- [ ] Optional parameter omitted
- [ ] Default parameter used
- [ ] Mixed optional and default

---

### 13. Try/Catch/Finally 🟠 **MEDIUM-HIGH**
**Priority:** P1 | **Effort:** 4-6 weeks | **Complexity:** High

**Current State:**
```typescript
// ❌ NOT SUPPORTED
try {
    riskyOperation();
} catch (e) {
    console.log(e);
} finally {
    cleanup();
}
```

**What's Needed:**
- [ ] LLVM exception handling (landing pads, invoke)
- [ ] Error object representation
- [ ] Stack unwinding
- [ ] Catch clause with error binding
- [ ] Finally block guarantee
- [ ] Nested try blocks
- [ ] Exception type matching

**Implementation Notes:**
- Use LLVM's `invoke` + `landingpad` instructions
- Personality function for unwinding
- Integrate with C++ exception ABI (`__cxa_throw`)
- Alternative: Result<T, E> pattern (Rust-style)

**Tests:**
- [ ] Basic try/catch
- [ ] Finally always runs
- [ ] Nested try blocks
- [ ] Re-throw: `catch (e) { throw e; }`

---

### 14. Tuples 🟢 **MEDIUM**
**Priority:** P2 | **Effort:** 1-2 weeks | **Complexity:** Low

**Current State:**
```typescript
// ❌ NOT SUPPORTED
const pair: [string, number] = ["age", 42];
```

**What's Needed:**
- [ ] Parse `TsTupleType`
- [ ] Extend `OatsType`: `Tuple(Vec<OatsType>)`
- [ ] Lower to struct with fixed size
- [ ] Index access: `pair[0]`, `pair[1]`
- [ ] Tuple destructuring

**Tests:**
- [ ] Tuple declaration
- [ ] Tuple indexing
- [ ] Tuple destructuring

---

### 15. Spread & Rest Operators 🟡 **HIGH**
**Priority:** P1 | **Effort:** 2-3 weeks | **Complexity:** Medium

**Current State:**
```typescript
// ❌ NOT SUPPORTED
const arr = [1, 2, ...otherArr];
const obj = { x: 1, ...otherObj };
function sum(...nums: number[]) { }
```

**What's Needed:**
- [ ] Array spread: `[...arr]`
- [ ] Object spread: `{ ...obj }`
- [ ] Rest parameters: `...args`
- [ ] Runtime array/object copying

**Tests:**
- [ ] Array spread in literal
- [ ] Object spread in literal
- [ ] Rest parameters
- [ ] Spread in function call

---

## 🟣 Phase 3: Ecosystem & Tooling (12+ months)

**Goal:** Production-ready, npm-compatible, ecosystem integration  
**Target Compatibility:** 80%+ of TypeScript code

### 16. Advanced Generics
- [ ] Higher-kinded types
- [ ] Conditional types: `T extends U ? X : Y`
- [ ] Mapped types: `{ [K in keyof T]: ... }`
- [ ] Template literal types

### 17. Decorators
- [ ] Class decorators: `@Component`
- [ ] Method decorators
- [ ] Property decorators
- [ ] Parameter decorators

### 18. Advanced Type System
- [ ] Intersection types: `A & B`
- [ ] Literal types: `type One = 1`
- [ ] Discriminated unions
- [ ] Type guards: `is`, `asserts`
- [ ] Index signatures: `[key: string]: any`

### 19. Module System (Advanced)
- [ ] `node_modules` resolution
- [ ] Package.json parsing
- [ ] `@types/*` definitions
- [ ] CommonJS interop: `require()`
- [ ] ESM: `import.meta`, dynamic `import()`

### 20. Standard Library (Complete)
- [ ] Full `Array` prototype
- [ ] Full `String` prototype
- [ ] `Map`, `Set`, `WeakMap`, `WeakSet`
- [ ] `RegExp`
- [ ] `Date`, `JSON`
- [ ] `Promise`, `async iterators`
- [ ] `Proxy`, `Reflect`

### 21. FFI & Native Interop
- [ ] C ABI calling convention
- [ ] Extern declarations: `declare function malloc(size: number): Pointer`
- [ ] Buffer/ArrayBuffer for binary data
- [ ] WebAssembly interop

### 22. Build System Integration
- [ ] `tsconfig.json` support
- [ ] Source maps
- [ ] Incremental compilation
- [ ] Watch mode
- [ ] Module bundling

### 23. Debugging & Profiling
- [ ] DWARF debug info
- [ ] GDB/LLDB integration
- [ ] Performance profiling hooks
- [ ] Memory leak detection

### 24. Optimization
- [ ] Inline functions
- [ ] Dead code elimination
- [ ] Constant folding
- [ ] Loop optimizations
- [ ] Escape analysis (stack allocation)

---

## 📊 Effort & Timeline Estimates

### Phase 1 Breakdown (6-12 months, 1-2 developers)
| Feature | Effort | Complexity | Priority |
|---------|--------|------------|----------|
| Arrow functions (non-closing) | 1-2 weeks | Low | P0 |
| Interfaces & type aliases | 2-3 weeks | Medium | P0 |
| Union types (basic) | 2-3 weeks | Medium | P1 |
| Object literals | 1-2 weeks | Low-Medium | P1 |
| Template literals | 1 week | Low | P2 |
| Module resolution | 4-6 weeks | High | P0 |
| Standard library basics | 2-4 weeks | Medium | P1 |
| **Total** | **14-22 weeks** | | |

### Phase 2 Breakdown (6-12 months, 2-3 developers)
| Feature | Effort | Complexity | Priority |
|---------|--------|------------|----------|
| Closures | 4-6 weeks | High | P0 |
| Generics | 6-8 weeks | Very High | P0 |
| Async/await | 8-12 weeks | Very High | P0 |
| Destructuring | 3-4 weeks | Medium | P2 |
| Optional/default params | 1-2 weeks | Low | P2 |
| Try/catch | 4-6 weeks | High | P1 |
| Tuples | 1-2 weeks | Low | P2 |
| Spread/rest | 2-3 weeks | Medium | P1 |
| **Total** | **30-48 weeks** | | |

### Phase 3 Breakdown (12+ months, 3-5 developers)
- Advanced language features: 6-12 months
- Ecosystem integration: 6-12 months
- Production hardening: 6-12 months

---

## 🎯 Recommended Next Steps

### Immediate (This Sprint)
1. ✅ **Arrow functions (non-closing)** - Quick win, high impact
2. ✅ **Object literals** - Unblocks many patterns
3. ✅ **Template literals** - Nice quality-of-life improvement

**Expected Outcome:** Jump from <1% to ~10% compatibility

### Next Quarter (3 months)
4. ✅ **Interfaces & type aliases** - Critical for any real code
5. ✅ **Union types (basic)** - Nullable types, option types
6. ✅ **Module resolution** - Multi-file compilation

**Expected Outcome:** ~15-20% compatibility, can compile small libraries

### Next 6 Months
7. ✅ **Standard library basics** - Console, Math, Array methods
8. ✅ **Closures** - Unlocks functional programming
9. ✅ **Try/catch** - Error handling

**Expected Outcome:** ~30-40% compatibility, can compile real applications

---

## 📈 Success Metrics

### Compatibility Milestones
- [ ] **10% milestone:** Can compile simple utility libraries (lodash basics)
- [ ] **25% milestone:** Can compile small frameworks (basic Express routes)
- [ ] **50% milestone:** Can compile medium applications (React components)
- [ ] **75% milestone:** Can compile large frameworks (full Express, React)
- [ ] **90% milestone:** Can compile most npm packages

### Performance Targets
- [ ] Compilation speed: <100ms per 1000 LOC
- [ ] Generated code: Within 2x of V8/JIT performance
- [ ] Memory overhead: <10MB baseline

### Quality Targets
- [ ] Test coverage: >80%
- [ ] Zero crashes on valid TypeScript
- [ ] Graceful errors on invalid code
- [ ] Documentation for all public APIs

---

## 🚧 Known Limitations & Future Work

### Architecture Decisions
1. **Monomorphization vs Type Erasure:** Currently planning monomorphization (C++ style). May switch to type erasure (Java style) for faster compilation.
2. **Memory Model:** Current RC model is simple but has cycle issues. May need:
   - Cycle detection (reference counting + cycle collector)
   - Tracing GC (more complex runtime)
   - Rust-style ownership (borrow checker)
3. **Runtime:** Currently minimal runtime. May need:
   - Full standard library implementation
   - Or link to V8/Node.js runtime
   - Or WebAssembly target

### Research Questions
- How to handle npm's 2+ million packages?
- TypeScript's type system is Turing-complete - what subset do we support?
- Interop with JavaScript? Transpile to JS? Compile to WASM?

---

## 📚 References & Resources

### Similar Projects
- **TypeScript Compiler (tsc):** Reference implementation
- **Deno:** Rust-based TypeScript runtime
- **SWC:** Fast TypeScript/JavaScript compiler in Rust
- **Rome/Biome:** TypeScript toolchain in Rust
- **QuickJS:** Lightweight JavaScript engine
- **Hermes:** AOT compiler for React Native

### Reading Material
- TypeScript Handbook: https://www.typescriptlang.org/docs/handbook/
- LLVM Language Reference: https://llvm.org/docs/LangRef.html
- Crafting Interpreters: https://craftinginterpreters.com/
- Engineering a Compiler (Cooper & Torczon)

---

## 💬 Contributing

Want to help? Pick a feature from Phase 1, check the effort estimate, and:

1. Open an issue: "Implement [feature name]"
2. Get feedback on approach
3. Submit incremental PRs (small, tested changes)
4. Document your work

**Good First Issues:**
- Arrow functions (non-closing)
- Template literals
- Tuples
- Object literals

**Harder Issues (Need Mentoring):**
- Module resolution
- Closures
- Generics
- Async/await

---

## 📝 Changelog

- **2025-10-08:** Initial roadmap created based on gap analysis
- Next update: After Phase 1 features complete

---

*This roadmap is a living document. Priorities may shift based on user feedback, technical discoveries, and resource availability.*
