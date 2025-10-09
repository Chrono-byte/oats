# Oats Compiler Roadmap

This document provides a comprehensive roadmap for the Oats TypeScript-to-LLVM
AOT compiler, covering short-term goals, long-term vision, and detailed
implementation plans for major features.

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Current Status](#current-status)
3. [Short-term Roadmap (Phase 1)](#short-term-roadmap-phase-1)
4. [Medium-term Roadmap (Phase 2)](#medium-term-roadmap-phase-2)
5. [Long-term Vision (Phase 3)](#long-term-vision-phase-3)
6. [Async/Await Implementation Plan](#asyncawait-implementation-plan)
7. [Consolidated Priorities](#consolidated-priorities)
8. [Timeline & Effort Estimates](#timeline--effort-estimates)

---

## Executive Summary

**Current State:** Oats can compile simple, self-contained TypeScript programs
with basic classes, arrays, loops, and functions. However, it cannot run "off
the shelf" TypeScript code from the internet.

**Compatibility Estimate:**

- **Today:** <1% of npm packages would work
- **After Phase 1:** ~10-15% of simple TypeScript code
- **After Phase 2:** ~40-50% of typical applications
- **Production Ready:** 3-5 years of development

**Development Focus:**

- **Phase 1 (6-12 months):** Core language features for basic compatibility
- **Phase 2 (6-12 months):** Advanced features for real applications
- **Phase 3 (12+ months):** Ecosystem integration and production readiness

---

## Current Status

### ✅ What Works Today (v0.1.0)

#### Type System

- [x] Basic types: `number`, `boolean`, `string`, `void`
- [x] Arrays: `number[]`, `string[]` (homogeneous only)
- [x] Nominal classes: `class Foo { ... }`
- [x] Type annotations on parameters and returns

#### Language Features

- [x] Named functions with strict type checking
- [x] Classes with constructors, fields, methods
- [x] `this` binding in methods
- [x] Control flow: `if/else`, `for`, `while`, `do-while`, `for-of`
- [x] Labeled `break` and `continue`
- [x] Binary operators: arithmetic, comparison, logical (&&, ||)
- [x] All unary operators: `!`, `-`, `+`, `~`, `++`, `--`
- [x] String concatenation and template literals
- [x] Array literals and indexing
- [x] Member access: `obj.field` (read/write)
- [x] Object construction: `new ClassName(args)`
- [x] Constructor parameters: `constructor(public x: number)`

#### Module System

- [x] Export declarations: `export function`, `export class`
- [x] Import declarations parsed (types registered)
- [ ] **NOT SUPPORTED:** Cross-file compilation, module resolution

#### Memory Management

- [x] Reference counting for heap objects
- [x] Automatic RC increment/decrement
- [x] Runtime bounds checking for arrays
- [x] Unified 64-bit headers for all heap objects
- [x] Static string literals (immortal, no RC overhead)

#### Tooling

- [x] LLVM IR generation
- [x] AOT compilation to native code
- [x] Basic diagnostics with source spans
- [x] Test suite (51 tests passing)

### ❌ Critical Missing Features

**Blocking Real-world Usage:**

### Status: implemented / partially-implemented / outstanding

The project has made progress on several items previously listed as missing.
Below is the updated status with short notes.

- ✅ Arrow functions (non-capturing) — implemented (non-capturing arrows lowered
  to static functions; capturing/closures remain to be implemented)
- ✅ Object literals (basic) — implemented: object literals are lowered to
  heap-allocated structs with header + fields; shorthand props supported.
  Computed keys and spread remain unsupported.
- ⚠️ Interface types and type aliases — partially implemented: nominal type
  mapping (`TsTypeRef` -> `NominalStruct`) and Promise/Array generic handling
  exist; full structural interface/type-alias support is still incomplete.
- ❌ Module resolution and multi-file compilation — still outstanding
  (architectural work: resolver, graph, cross-file symbol resolution).
- ⚠️ Standard library — partially implemented: runtime helpers exist for
  `math_random`, string/print helpers, and array helper functions
  (`array_alloc`, `array_get_*`, `array_push_*`); full stdlib API surface is
  still missing.
- ✅ Union types (basic) — implemented: `OatsType::Union` support in the type
  mapper, runtime boxing/unboxing helpers, and discriminant helper are present.
  Generics beyond simple Promise/Array support remain outstanding.
- ❌ Closures with capture — still outstanding (escape analysis and closure
  environment lowering planned in Phase 2)

### Phase A Complete

We have completed the Phase A stabilization tasks that ensure a consistent
object layout, metadata format, and initial collector scaffolding. Key items
completed:

- Consistent object layout (header/meta/fields) across constructors and object literals
- Packed per-class metadata globals and runtime validation
- Trial-deletion collector scaffold and diagnostic logging (opt-in via env var)
- Integration and unit tests verifying the above (codegen snapshots, runtime tests)

This completes the critical foundation required before implementing module
resolution and closures in Phase B/C.

---

## Short-term Roadmap (Phase 1)

**Goal:** Support simple npm packages and common TypeScript patterns\
**Target Compatibility:** 10-15% of TypeScript code\
**Timeline:** 6-12 months, 1-2 developers

### Priority 0 (Critical Blockers)

#### 1. Arrow Functions (non-capturing) 🔴

**Effort:** 1-2 weeks | **Complexity:** Low

```typescript
// Target support:
const add = (x: number, y: number) => x + y;
const double = (x: number) => x * 2;
arr.forEach((x) => print_f64(x));
```

**Implementation:**

- Parse `ast::Expr::Arrow` in expression lowering
- Generate function symbol for arrow expression
- Handle implicit returns for expression bodies
- Store arrow function as callable value (function pointer)
- Start with non-capturing arrows (defer closures to Phase 2)

#### 2. Module Resolution and Multi-file Compilation 🔴

**Effort:** 4-6 weeks | **Complexity:** High

```typescript
// Target support:
import { readFile } from "fs";
import utils from "./utils";
export { helper } from "./lib";
```

**Implementation:**

- Module resolver (Node.js algorithm for relative imports)
- Find .ts/.tsx files on disk (no node_modules initially)
- Parse imported modules recursively
- Build dependency graph with cycle detection
- Topological sort for compilation order
- Cross-module symbol resolution
- This is the biggest architectural change

#### 3. Interfaces and Type Aliases 🔴

**Effort:** 2-3 weeks | **Complexity:** Medium

```typescript
// Target support:
interface User {
    name: string;
    age: number;
}
type Point = { x: number; y: number };
```

**Implementation:**

- Parse `TsInterfaceDecl` and register as nominal type
- Parse `TsTypeAliasDecl` and expand to underlying type
- Extend `map_ts_type()` to handle interface references
- Support object type literals: `{ field: type }`
- Start with nominal typing (structural typing later)

### Priority 1 (High Impact)

#### 4. Unions, Object Literals, and typeof (Critical consolidation) 🟡

Work completed so far has introduced a runtime discriminant helper and basic
boxed-union layout. The next step is to finish end-to-end support and test
coverage.

Effort: 1-3 weeks | Complexity: Medium

Goals & acceptance criteria:

- Representation: unions are boxed where necessary (pointer ABI) and expose a
  64-bit discriminant word (runtime helper `union_get_discriminant`).
- Lowering: locals, parameters, field stores, array elements, and object
  literals use boxed unions when type requires. Compiler emits `union_box_*` /
  `union_unbox_*` and `rc_inc`/`rc_dec` appropriately.
- typeof semantics: unary `typeof` and binary `typeof <expr> === "..."` use the
  discriminant for boxed unions and return interned string pointers that behave
  like static string literals.
- Tests: IR-level tests assert presence of `union_get_discriminant` and interned
  string globals; integration smoke test executes an AOT output exercising
  typeof on boxed unions.

Implementation steps (short):

1. Finish codegen lowering for union-typed locals/params/fields (box on store,
   unbox on numeric coercion). Ensure `rc_inc`/`rc_dec` invariants. (1 week)
2. Finalize unary and binary `typeof` lowering to consult discriminant when
   operand is pointer-like and fall back correctly. (2-3 days)
3. Add IR-level and runtime tests covering boxed number/string/boolean unions,
   field writes, param passing, and destructor behaviour. (3-5 days)
4. Small optimizations: hoist cached helper FunctionValue lookups and consider
   inlining discriminant checks in hot paths. (optional)

Rationale: finishing unions and object-literal boxing is high-impact — it
enables many real-world patterns (heterogeneous containers, optional values) and
stabilizes runtime/RC semantics.

### Expected Phase 1 Outcome

- **Compatibility:** Jump from <1% to ~15-20%
- **Can Compile:** Simple utility libraries, basic frameworks
- **Examples:** Basic Express routes, simple React components

---

## Medium-term Roadmap (Phase 2)

**Goal:** Support typical application code, framework basics\
**Target Compatibility:** 40-50% of TypeScript code\
**Timeline:** 6-12 months, 2-3 developers

### Advanced Language Features

#### 7. Closures with Capture 🔴

**Effort:** 4-6 weeks | **Complexity:** High

```typescript
// Target support:
function makeCounter() {
    let count = 0;
    return () => count++; // captures 'count'
}
```

**Implementation:**

- Escape analysis: detect captured variables
- Closure environment struct allocation
- Box captured variables on heap
- Generate closure thunk (trampoline)
- Pass environment pointer to closure body
- This unlocks functional programming patterns

#### 8. Generics (Monomorphization) 🔴

**Effort:** 6-8 weeks | **Complexity:** Very High

```typescript
// Target support:
function identity<T>(x: T): T {
    return x;
}
class Box<T> {
    constructor(public value: T) {}
}
```

**Implementation:**

- Parse type parameters: `<T, U extends Base>`
- Track generic types in `OatsType`
- Monomorphization: generate specialized code per concrete type
- Cache monomorphized functions (avoid duplicates)
- Alternative: type erasure (Java style) - simpler but slower
- This is PhD-level compiler work

#### 9. Try/Catch/Finally 🟠

**Effort:** 4-6 weeks | **Complexity:** High

```typescript
// Target support:
try {
    riskyOperation();
} catch (e) {
    console.log(e);
} finally {
    cleanup();
}
```

**Implementation:**

- LLVM exception handling (landing pads, invoke)
- Error object representation
- Stack unwinding with proper cleanup
- Alternative: Result<T, E> pattern (Rust-style)

### Enhanced Type System

#### 10. Destructuring 🟢

**Effort:** 3-4 weeks | **Complexity:** Medium

```typescript
// Target support:
const { x, y } = point;
const [first, second] = array;
function fn({ name, age }: User) {}
```

#### 11. Tuples 🟢

**Effort:** 1-2 weeks | **Complexity:** Low

```typescript
// Target support:
const pair: [string, number] = ["age", 42];
```

#### 12. Spread & Rest Operators 🟡

**Effort:** 2-3 weeks | **Complexity:** Medium

```typescript
// Target support:
const arr = [1, 2, ...otherArr];
const obj = { x: 1, ...otherObj };
function sum(...nums: number[]) {}
```

### Expected Phase 2 Outcome

- **Compatibility:** ~40-50% of typical applications
- **Can Compile:** Medium frameworks, React applications
- **Examples:** Full Express servers, complex React apps

---

## Long-term Vision (Phase 3)

**Goal:** Production-ready, npm-compatible, ecosystem integration\
**Target Compatibility:** 80%+ of TypeScript code\
**Timeline:** 12+ months, 3-5 developers

### Advanced Type System

- [ ] Higher-kinded types
- [ ] Conditional types: `T extends U ? X : Y`
- [ ] Mapped types: `{ [K in keyof T]: ... }`
- [ ] Template literal types
- [ ] Intersection types: `A & B`
- [ ] Discriminated unions
- [ ] Index signatures: `[key: string]: any`

### Module System (Advanced)

- [ ] `node_modules` resolution
- [ ] Package.json parsing
- [ ] `@types/*` definitions
- [ ] CommonJS interop: `require()`
- [ ] ESM: `import.meta`, dynamic `import()`

### Standard Library (Complete)

- [ ] Full `Array` prototype methods
- [ ] Full `String` prototype methods
- [ ] `Map`, `Set`, `WeakMap`, `WeakSet`
- [ ] `RegExp`, `Date`, `JSON`
- [ ] `Promise`, async iterators
- [ ] `Proxy`, `Reflect`

### Ecosystem Integration

- [ ] `tsconfig.json` support
- [ ] Source maps and debugging
- [ ] Incremental compilation
- [ ] Module bundling
- [ ] FFI & native interop

### Performance & Optimization

- [ ] Inline functions
- [ ] Dead code elimination
- [ ] Loop optimizations
- [ ] Escape analysis (stack allocation)

---

## Async/Await Implementation Plan

### Overview

Async/await is one of the most complex features to implement, requiring deep
integration between compiler and runtime. This section provides a comprehensive
implementation plan.

### Architecture: State Machines + Runtime Scheduler

#### Compiler-Side Transformation

**1. Create State Struct** For each async function, create a struct containing:

- All local variables (persist when function pauses)
- State field (integer) to track execution point
- Saved intermediate values from expressions

```typescript
// Source:
async function fetchUser(id: number): Promise<User> {
    let response = await fetch(`/api/users/${id}`);
    let user = await response.json();
    return user;
}

// Transformed to state struct:
struct FetchUserState {
    state: i32,           // Current execution point
    id: f64,              // Parameter
    response: ptr,        // Local variable
    user: ptr,            // Local variable
    temp_result: ptr,     // Temporary for await results
}
```

**2. Rewrite Function Body** Transform the async function into a resume function
with switch-based dispatch:

```rust
fn fetchUser_resume(state_ptr: *mut FetchUserState) -> TaskStatus {
    match state_ptr.state {
        0 => {
            // Initial state: call fetch()
            let url = string_concat("/api/users/", state_ptr.id);
            let fetch_promise = fetch(url);
            state_ptr.temp_result = fetch_promise;
            state_ptr.state = 1;
            return TaskStatus::Pending;
        }
        1 => {
            // Resume after fetch completes
            state_ptr.response = state_ptr.temp_result;
            // Continue to next await...
        }
        // ... more states
    }
}
```

**3. Lower `await` Expressions** Each `await` becomes:

1. Execute the awaited expression
2. Save current state
3. Return "pending" status to runtime
4. Create continuation point for resume

#### Runtime-Side Execution

**1. Task Queue** Runtime maintains queue of ready-to-run tasks:

```c
typedef struct Task {
    void* state_ptr;           // Pointer to state machine struct
    TaskResumeFn resume_fn;    // Function to resume execution
    struct Task* next;         // Next task in queue
} Task;
```

**2. Event Loop** Core scheduler loop:

```c
void runtime_run_event_loop() {
    while (has_pending_tasks()) {
        Task* task = dequeue_ready_task();
        TaskStatus status = task->resume_fn(task->state_ptr);
        
        if (status == TASK_COMPLETE) {
            free_task(task);
        } else if (status == TASK_PENDING) {
            // Will be rescheduled by promise completion
        }
    }
}
```

**3. Promise Object** Link tasks together and enable scheduler to resume waiting
tasks:

```c
typedef struct Promise {
    i64 header;               // Standard RC header
    uint8_t status;           // Pending/Resolved/Rejected
    void* result;             // Result value (when resolved)
    Task** waiting_tasks;     // Array of tasks waiting on this promise
    size_t waiting_count;     // Number of waiting tasks
} Promise;
```

### Implementation Phases

#### Phase 0: Foundation (2-4 hours)

- [ ] Add `Promise` object layout to heap design
- [ ] Add `OatsType::Promise(Box<OatsType>)` to type system
- [ ] Write unit tests for Promise representation

#### Phase 1: State Machine Transformation (1-2 weeks)

- [ ] Add `is_async` detection in function parsing
- [ ] Implement `gen_async_function_ir()` for simple case
- [ ] Transform single `await` expression
- [ ] Generate switch-based resume function
- [ ] Test: compile async function to IR

#### Phase 2: Minimal Scheduler (1-2 weeks)

- [ ] Implement task queue in runtime
- [ ] Add `runtime_schedule_task()` function
- [ ] Add `runtime_run_event_loop()` function
- [ ] Implement Promise with status/result fields
- [ ] Test: manually schedule tasks

#### Phase 3: Integration (1-2 weeks)

- [ ] Codegen for `await` calls runtime functions
- [ ] Async function wrapper schedules tasks
- [ ] Main function calls event loop
- [ ] Test: end-to-end async function execution

#### Phase 4: Real I/O (3-4 weeks)

- [ ] Integrate with OS event systems (epoll/kqueue/IOCP)
- [ ] Implement `setTimeout()` using timer wheel
- [ ] Implement async file I/O
- [ ] Implement async network I/O
- [ ] Test: real async operations

**Total Estimated Effort:** 2-3 months focused development

---

## Consolidated Priorities

### High-level Goals

- **Remove panic sites** in compiler by migrating to `Result<_, Diagnostic>`
- **Modularize** large `codegen/mod.rs` into smaller modules
- **Harden runtime** with bounds checks and error handling
- **Add CI** that ensures correctness across platforms

### Short-term (1-3 days)

1. **Finish error handling migration** (incremental)
   - Strategy: Add `lower_*_result` adapters, convert callers gradually
   - Safety: Small commits, run `cargo build -p oats` after each
   - Tests: Add tests that ensure diagnostics instead of panics

2. **Member-write lowering** (0.5-1 day)
   - Implement `obj.field = expr` with RC semantics
   - Add test asserting correct IR (getelementptr + store + rc_inc/rc_dec)

3. **Add CI for LLVM-18** (1 day)
   - Use `scripts/setup_env.sh` as reference
   - Install `llvm-18` on Ubuntu runners and set env variables

### Medium-term (weeks)

1. **Modularize CodeGen** (batched approach)
   - Batch A: Create `expr.rs` and `stmt.rs`, move small helpers
   - Batch B: Move function-level emission to `gen.rs`
   - Batch C: Common utilities to `helpers.rs`, tidy imports

2. **Core language features**
   - Arrow functions (non-capturing)
   - Object literals
   - Module resolution (relative imports first)
   - Standard library basics

### Long-term (months)

- **Closures** with boxed environments and capture semantics
- **Memory management** improvements (cycle detection or tracing GC)
- **Async/await** lowering and runtime scheduler

---

## Timeline & Effort Estimates

### Phase 1 Breakdown (6-12 months, 1-2 developers)

| Feature                       | Effort          | Complexity | Priority |
| ----------------------------- | --------------- | ---------- | -------- |
| Arrow functions (non-closing) | 1-2 weeks       | Low        | P0       |
| Interfaces & type aliases     | 2-3 weeks       | Medium     | P0       |
| Union types (basic)           | 2-3 weeks       | Medium     | P1       |
| Object literals               | 1-2 weeks       | Low-Medium | P1       |
| Template literals             | 1 week          | Low        | P2       |
| Module resolution             | 4-6 weeks       | High       | P0       |
| Standard library basics       | 2-4 weeks       | Medium     | P1       |
| **Total Phase 1**             | **14-22 weeks** |            |          |

### Phase 2 Breakdown (6-12 months, 2-3 developers)

| Feature                 | Effort          | Complexity | Priority |
| ----------------------- | --------------- | ---------- | -------- |
| Closures                | 4-6 weeks       | High       | P0       |
| Generics                | 6-8 weeks       | Very High  | P0       |
| Async/await             | 8-12 weeks      | Very High  | P0       |
| Destructuring           | 3-4 weeks       | Medium     | P2       |
| Optional/default params | 1-2 weeks       | Low        | P2       |
| Try/catch               | 4-6 weeks       | High       | P1       |
| Tuples                  | 1-2 weeks       | Low        | P2       |
| Spread/rest             | 2-3 weeks       | Medium     | P1       |
| **Total Phase 2**       | **30-48 weeks** |            |          |

### Phase 3 Breakdown (12+ months, 3-5 developers)

- **Advanced language features:** 6-12 months
- **Ecosystem integration:** 6-12 months
- **Production hardening:** 6-12 months

### Success Metrics

#### Compatibility Milestones

- [ ] **10% milestone:** Simple utility libraries (lodash basics)
- [ ] **25% milestone:** Small frameworks (basic Express routes)
- [ ] **50% milestone:** Medium applications (React components)
- [ ] **75% milestone:** Large frameworks (full Express, React)
- [ ] **90% milestone:** Most npm packages

#### Performance Targets

- [ ] Compilation speed: <100ms per 1000 LOC
- [ ] Generated code: Within 2x of V8/JIT performance
- [ ] Memory overhead: <10MB baseline

#### Quality Targets

- [ ] Test coverage: >80%
- [ ] Zero crashes on valid TypeScript
- [ ] Graceful errors on invalid code
- [ ] Documentation for all public APIs

---

## Next Steps

### Immediate (This Sprint)

1. ✅ **Arrow functions (non-closing)** - Quick win, high impact
2. ✅ **Object literals** - Unblocks many patterns
3. ✅ **Template literals** - Quality-of-life improvement

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

## Contributing

### Good First Issues

- Arrow functions (non-closing)
- Template literals
- Tuples
- Object literals

### Harder Issues (Need Mentoring)

- Module resolution
- Closures
- Generics
- Async/await

### How to Help

1. **Pick a feature** from Phase 1, check effort estimate
2. **Open an issue:** "Implement [feature name]"
3. **Get feedback** on approach
4. **Submit incremental PRs** (small, tested changes)
5. **Document your work**

---

This roadmap represents a living document that evolves based on user feedback,
technical discoveries, and resource availability. The focus remains on building
a production-quality TypeScript compiler that can handle real-world codebases
while maintaining the performance benefits of AOT compilation.
