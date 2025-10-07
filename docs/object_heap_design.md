# Object / Heap / Class design for Oats

This design doc describes a pragmatic, incremental runtime/object model for the
Oats AOT prototype. It focuses on minimal complexity (work with the existing
LLVM/inkwell setup), easy codegen, and a clear migration path from the current
prototype to more advanced features (classes, arrays, objects, closures, GC).

Goals and constraints

- Keep the runtime small and explicit: add a few runtime helpers (malloc/free,
  array alloc, strlen, concat, print) rather than a full GC initially.
- Work with the existing type narrowing in `crates/oats/src/types.rs`
  (Number/Boolean/String/NominalStruct) and the current `CodeGen` which already
  treats `String`/`NominalStruct` as pointer types.
- Avoid complex runtime tagging schemes (e.g., NaN-boxing) in the short term.
  Prefer a simple split representation: numbers as native f64, other values as
  pointers (i8* / opaque struct pointers).
- Make it easy to evolve to a GC or refcounting later.

High-level representation

- Primitive: Number -> LLVM f64 (immediate, passed/returned as f64)
- Boolean -> small integer type (i1 / i8 / i64 as needed), but boxed locally as
  LLVM integer where required.
- Reference types (String, NominalStruct, Array, Object, Function closures) ->
  opaque pointer type (i8* / ptr to a named LLVM struct) at the ABI level.

Why this split?

- The current codebase already maps `OatsType::Number` to f64 and
  `String`/`NominalStruct` to pointer types. Extending that model keeps
  compatibility and keeps lowering simple.
- It avoids immediate need for boxing numbers and the complexity of
  unboxing/boxing at call boundaries.

Heap object layout (recommended initial format) We use a small, simple object
header followed by payload. All heap objects are pointer-aligned and
returned/captured as `i8*` (or typed LLVM pointer for nominal structs).

Common header (first bytes)

- 0..7: u64 header word: low bits reserved for flags; remaining bits for a
  runtime type id / size / refcount tag depending on configuration.

Suggested simple header encoding (32/64-bit):

- u64: [63..32] = type tag / type id (32 bits) ; [31..0] = size_or_refcount (32
  bits) or other metadata

Interpretation options (pick one for initial impl):

- Option A (no refcount): store type id in high bits and payload size in low
  bits. Free must be explicit or at program end.
- Option B (refcount): store refcount in low 32 bits and type id in high 32
  bits; require atomic ops only if multi-threaded (we'll implement
  single-threaded first).

Concrete layouts

- String
  - header (typeid=STR_TYPE)
  - length (u64 or u32)
  - bytes (length bytes) + null terminator (optional)
  - Existing code emits string globals for literals; runtime `strdup`/`malloc`
    can create heap-allocated strings when needed or `str_concat` will allocate.

- Array
  - header (typeid=ARRAY_TYPE)
  - length (u64)
  - element type id (optional) or element size (u32)
  - element storage (contiguous): for homogeneous numeric arrays, store f64
    elements directly; otherwise store pointers (i8*) per element.

- NominalStruct / Class instance
  - header (typeid = nominal type id)
  - fields in declared order (each field either f64 or pointer) — layout
    determined at compile time for nominal structs
  - Methods are emitted as plain functions; instance methods receive a typed
    pointer (or i8*) as first parameter (like `this`).

NominalStruct vs dynamic object

- If a type is declared in source (a `class` or an explicit TS type), prefer
  emitting a nominal LLVM struct type and use concrete GEP-based access to
  fields (faster, static layout). This leverages the existing
  `OatsType::NominalStruct(String)`.
- For dynamic / object-literal use-cases where the shape is not known at compile
  time, lower to a heap `Object` representation with a hashtable-like runtime
  (or an array of (key,value) pairs) and runtime helpers `obj_get(obj, key)` /
  `obj_set(obj, key)`.

Method dispatch and constructors

- Methods for nominal structs map to either:
  - non-virtual functions: emitted as
    `func_<Type>_<method>(<struct_ptr>, args...)` and resolved at compile/link
    time; or
  - virtual dispatch (later): if inheritance is added, emit a vtable pointer in
    the header and perform an indirect call.
- Constructor: lower `constructor(...)` to a function that allocates the
  instance (`malloc(sizeof(struct))`), initializes fields, and returns the typed
  pointer.

Arrays and indexing

- Array allocation is performed via runtime helper
  `array_alloc(len, elem_size, elem_typeid?) -> i8*` which allocates
  header+length+data and returns an i8* (or typed pointer for homogeneous
  arrays).
- Indexing lowers to either direct GEP (for nominal homogeneous arrays) or calls
  to runtime helpers `array_get(ptr, idx)` / `array_set(ptr, idx, value)` when
  element types are dynamic.

Strings

- String literals (compile-time) may remain in read-only global memory (current
  code already emits const global arrays). When a runtime-owned string is
  required (concatenation), return a heap-allocated string with
  header+len+bytes.
- Provide runtime helpers: `strlen(i8* s) -> i64`,
  `str_concat(i8* a, i8* b) -> i8*`.

Memory management strategy Start simple and practical:

- Phase 0 (initial): manual `malloc` + `free` via runtime; avoid freeing where
  not strictly necessary. This is simplest for prototyping.
- Phase 1: add single-threaded reference counting. Runtime helpers
  increment/decrement refcounts; on zero call `free` and recursively DEC fields
  that are pointers.
- Phase 2: optional tracing GC (harder): integrate a simple stop-the-world
  tracing GC if refcounting proves insufficient (cycles) or performance requires
  it.

Rust-like ownership model (preferred where possible) The project goal you stated
is to "recreate Rust behavior wherever possible" for object heap behavior. The
following describes a pragmatic way to map Rust semantics onto the prototype
while balancing TypeScript's dynamic needs.

Principles

- Ownership by default: each heap value has a single owning handle. Moving a
  value transfers ownership (no implicit copy). This avoids hidden shared
  aliases and enables deterministic destruction semantics similar to Rust's
  `Drop`.
- Borrowing (compile-time checked): support read-only borrows and, optionally,
  mutable borrows that the checker enforces (no two mutable borrows, no
  mutable + immutable aliasing). For the prototype, implement a conservative
  checker that forbids obvious aliasing patterns; progressively relax with more
  advanced borrow analysis.
- Explicit sharing via Rc: when shared ownership is required, the source must
  opt-in (for example, via an explicit `Rc<T>` annotation or by compiler
  lowering for certain APIs). Shared ownership is implemented with
  single-threaded reference counting and deterministic `Drop` when the count
  reaches zero.
- Deterministic destructors: lower language-level destructors (if present) or
  emitted `Drop` glue to runtime calls executed when an owned value is freed
  (refcount reaches zero or owner goes out of scope).

How this maps to TS-ish source code

- By default, lowering treats local object/array values as moved on assignment
  and function call (call-by-value moves ownership). This differs from JS
  semantics; for TS-like convenience, we provide explicit helpers to
  clone/borrow when sharing is needed.
- Example source-to-lowering rules:
  - `let a = obj; let b = a;` -> move `a` into `b`; `a` is considered invalid
    after move unless the code clones explicitly: `let b = clone(a);`
  - Function calls take ownership of arguments unless the parameter is annotated
    as a borrow/reference (e.g., `fn foo(&T)` or an annotation we choose).
  - Return values transfer ownership to the caller.

Runtime support (helpers)

- `rc_inc(ptr: i8*)` — increment reference count (for shared values)
- `rc_dec(ptr: i8*)` — decrement reference count and, if zero, call
  `drop_fill(ptr)` then `free(ptr)`
  - Note: in the current implementation `rc_inc`/`rc_dec` are implemented using
    atomic 64-bit compare-exchange on the header word so the refcount operations
    are thread-safe. The runtime uses a CAS loop on the full u64 header so the
    high 32 bits (type tag) are preserved while the low 32 bits (refcount) are
    updated atomically.
- `drop_fill(ptr: i8*)` — call the runtime-compiled destructor/cleanup for
  fields (recursively `rc_dec` pointer fields)
- `malloc(size: i64)`, `free(ptr: i8*)`, `memcpy` as before

Codegen changes to enforce model

- `crates/oats/src/types.rs`
  - Optionally add ownership qualifiers to `OatsType` or a parallel `Ownership`
    enum (Owned, Borrowed, Shared).
  - Map annotated/shared types to `OatsType::NominalStruct` + ownership
    metadata.

- `crates/oats/src/codegen/mod.rs`
  - On local variable declaration: allocate storage for the owned handle; at
    scope exit, emit `rc_dec` (or `free` if unique) for pointer-like values.
  - On assignment and parameter passing: implement move semantics by emitting no
    runtime op for plain moves (just copy pointer and invalidate source at the
    IR level by preventing further use via the type checker). For shared values
    or explicit clones call `rc_inc`.
  - On returns: transfer ownership to caller by not calling `rc_dec` in callee;
    emitting `rc_inc` only when needed by caller semantics.
  - Add lowering for `clone(expr)` intrinsic (or source-level `...`) that emits
    explicit allocation/copy or `rc_inc` depending on representation.
  - Emit `drop` calls for types that require destructor behavior (call a
    generated destructor function for the nominal struct which runs `rc_dec` on
    pointer fields and any other cleanup).

Type checker / diagnostics changes

- Because Rust's safety comes from compile-time checks, we should add basic
  checks in `types.rs` / a new `borrow_check.rs`:
  - Track ownership state of locals: valid, moved, borrowed.
  - Reject use-after-move or invalid concurrent mutable borrows.
  - Provide helpful diagnostics using `diagnostics::report_error_span`.

Limitations and differences from Rust (practical concessions)

- JavaScript/TypeScript source code is historically permissive and implicitly
  shares objects; enforcing Rust-like moves will be a semantic break. To make
  the transition workable:
  - Opt-in shared/reference types: only treat values as moved by default in
    lowered code; provide a small set of idioms (or compiler transforms) to make
    common JS patterns ergonomic (e.g., implicitly wrap top-level exported
    objects in `Rc` during module lowering).
  - Borrow-checker scope: implement a conservative, local-dataflow borrow
    checker first; full borrow checker to cover all JS patterns is expensive.
  - Interop with dynamic reflective operations (e.g., `Object.keys`, `[]`
    indexing) will retain runtime helpers and dynamic semantics; Rust guarantees
    won't apply there.

Tests to add for ownership model

- `crates/oats/tests/ownership_move.rs` — test that moving a struct invalidates
  the original; expect compiler error on use-after-move.
- `crates/oats/tests/ownership_rc.rs` — test for explicit shared ownership:
  increment/decrement behavior, destructor runs on final drop; assert generated
  IR contains `rc_inc`/`rc_dec` calls for shared paths.
- `crates/oats/tests/borrow_check.rs` — simple mutable/immutable borrow tests to
  ensure the checker rejects invalid borrow patterns.

Incremental adoption plan

1. Implement `rc_inc`/`rc_dec` runtime helpers and lowering for explicit
   `clone`/`share` operations (no borrow-check yet). Add tests for shared
   behavior.
2. Add move semantics to codegen (source-level use-after-move diagnostics) with
   simple validity tracking (locals only). Tests: `ownership_move.rs`.
3. Implement a conservative borrow checker for local borrows (read-only and
   mutable) and block obvious misuse. Tests: `borrow_check.rs`.
4. Expand checker coverage and consider ergonomics: e.g., automatic `Rc`
   wrapping for exported modules or for values crossing module boundaries.

Runtime helpers (initial set)

- malloc(size: i64) -> i8*
- free(ptr: i8*) -> void
- memcpy(dst: i8*, src: i8*, size: i64) -> void
- strlen(ptr: i8*) -> i64
- str_concat(a: i8*, b: i8*) -> i8*
- print_f64(d: f64) -> void
- print_str(s: i8*) -> void
- array_alloc(len: i64, elem_size: i32, elem_is_number: i32) -> i8*
- array_get_i64/array_get_f64/array_get_ptr helpers (or a single typed helper)
- array_set_* helpers
- (Optional) inc_ref(ptr), dec_ref(ptr)

## Current implementation status (snapshot)

This repository already contains an initial Phase‑1 implementation for arrays
and heap helpers. The notes below summarize what is implemented in the workspace
today and what remains to do.

- Runtime helpers (implemented)
  - `array_alloc(len: i64, elem_size: i32, elem_is_number: i32) -> i8*` —
    allocates header+length+data and stores the element-kind flag in the header.
  - `array_get_f64(arr: i8*, idx: i64) -> f64` — typed fast path to read numeric
    array elements.
  - `array_get_ptr(arr: i8*, idx: i64) -> i8*` — typed helper to read pointer
    elements.
  - `array_set_f64` / `array_set_ptr` — typed stores for numeric/pointer arrays.
  - `rc_inc(ptr: i8*)` and `rc_dec(ptr: i8*)` — single-threaded refcount
    increment/decrement helpers (dec frees when count reaches zero).

- Header layout (current choice)
  - The u64 header word stores metadata with a small, stable encoding: high 32
    bits are a type tag / flags field and low 32 bits are used for size/refcount
    depending on configuration. The runtime currently uses a header bit to
    record whether the array payload contains numbers (f64) so codegen/runtime
    helpers can choose the correct access path.

- CodeGen changes (implemented)
  - `crates/oats/src/codegen/mod.rs` now declares and caches runtime function
    declarations (getters) for the new helpers (`array_alloc`, `array_get_f64`,
    `array_get_ptr`, `array_set_*`, `rc_inc`, `rc_dec`).
  - Array literal lowering: when all elements are compile-time numeric values,
    codegen allocates a numeric array and stores contiguous f64 payloads;
    otherwise it emits a pointer array (i8* elements).
  - Computed `obj[idx]` lowering (Member/computed): if the index expression
    lowers to an integer or float the lowering calls `array_get_f64(arr, idx)`
    to return an unboxed f64; otherwise it falls back to the pointer-array
    GEP+load path. This gives a fast, typed path for numeric arrays while
    preserving a simple pointer fallback for dynamic/mixed cases.

- Tests & build
  - The existing workspace tests build and pass after these changes. Some
    internal codegen getters are currently unused which produces harmless
    dead-code warnings (for example the pointer-getter or rc helpers that are
    declared but not yet used at all call-sites).

- Remaining / next work (high priority)
  - Wire `array_get_ptr` / typed pointer helpers into places that currently use
    GEP+load so pointer arrays also use the runtime helper if desired.
  - Add focused unit tests under `crates/oats/tests/` to validate numeric-array
    indexing, pointer-array indexing, and a numeric-array loop sum (to exercise
    the numeric helper path).
  - Decide and implement the ownership/refcount conventions for loads/stores
    (whether loads auto `rc_inc`, whether stores `rc_dec` the old value) and
    wire `rc_inc`/`rc_dec` into lowering as required by the ownership model.
  - Implement the dynamic runtime dispatch fallback that reads the header at
    runtime and chooses `array_get_f64` vs `array_get_ptr` when element-kind is
    unknown at compile time.

Status mapping to files

- `crates/runtime/src/lib.rs`: contains the new runtime helpers (alloc, typed
  get/set, rc_inc/rc_dec).
- `crates/oats/src/codegen/mod.rs`: contains getter declarations and the
  array-literal and computed-index lowering that calls `array_get_f64` for
  numeric indices and falls back to pointer GEP otherwise.

If you want, I can now add the focused tests (numeric array + loop sum and
pointer-array indexing), wire `array_set_*` callsites, and implement the
runtime-checked dynamic dispatch fallback. Pick which to do next and I'll
implement and validate it.

Codegen changes required

- `crates/oats/src/types.rs`
  - extend `map_ts_type` to support `TsArrayType`, `TsTupleType`, `TsTypeLit`
    and produce an `OatsType::Array(Box<OatsType>)` or extended `NominalStruct`
    representation.

- `crates/oats/src/codegen/helpers.rs`
  - extend `map_type_to_llvm` to handle arrays and nominal structs with field
    layouts; add helpers to compute field GEPs for nominal structs.

- `crates/oats/src/codegen/mod.rs`
  - Lower `MemberExpr`:
    - If left is `NominalStruct` instance: compute GEP and load/store a field.
    - If left is dynamic object: call runtime `obj_get/obj_set` helpers.
  - Lower `ArrayLit` and `IndexExpr`:
    - For element type known and numeric: allocate contiguous f64 buffer and
      emit stores.
    - For dynamic or mixed types: allocate array of pointers and emit boxed
      values.
  - Add lowering for `For`, `While`, and `DoWhile` by emitting appropriate basic
    blocks and branches.
  - Emit method bindings: when lowering a `class` method, emit a function with
    an explicit receiver parameter.

Tests to add

- `crates/oats/tests/arrays_and_loops.rs`:
  - Source: create a numeric array literal, loop over elements (for/while), sum
    them, call `println(sum)`; assert generated IR contains a loop label and
    array access lowers to either GEP or a runtime helper call.
- `crates/oats/tests/class_basic.rs`:
  - Source: class Point with `x,y` fields, constructor, method `dist`, create
    instance, call method; assert emitted IR contains constructor symbol and
    `Point_dist` method signature.
- `crates/oats/tests/module_import.rs`:
  - Two-file example: `a.oats` exports a function, `b.oats` imports and calls
    it; assert both functions are present in final IR.

Example lowering snippets

- Method signature (nominal struct Point with two doubles):

  define double @Point_dist(%Point* %this, double %otherx, double %othery) { ...
  }

- Constructor (returns pointer):

  define %Point* @Point_constructor(double %x, double %y) { %p = call i8*
  @malloc(i64 <size>) ; cast to %Point* ; store fields, return %Point* }

- Array helper usage (high-level):

  call i8* @array_alloc(i64 %len, i32 <elem_size>, i32 <is_number>) ; returns
  pointer to header->data pointer

Incremental implementation plan

1. Arrays + loops + member access (Short, high value)
   - Extend `types.rs` to recognize `TsArrayType` and map to
     `OatsType::Array(Box<OatsType>)`.
