# Oats Compiler Architecture

This document provides comprehensive technical documentation for the Oats TypeScript-to-LLVM AOT compiler, covering memory management, language feature implementation, and system design.

## Table of Contents

1. [Memory Management & Heap Object System](#memory-management--heap-object-system)
2. [Class System Implementation](#class-system-implementation)
3. [Unary Operators Implementation](#unary-operators-implementation)
4. [Object Layout & Type Representation](#object-layout--type-representation)
5. [Runtime Integration](#runtime-integration)

---

## Memory Management & Heap Object System

### Overview

The Oats compiler uses a **unified 64-bit header** for all heap-allocated objects (strings, arrays, classes). This system provides deterministic memory management through reference counting while maintaining TypeScript's share-by-reference semantics.

### Header Layout (8 bytes at offset 0)

```
Bits 0-31:   Reference count (atomic, u32)
Bit 32:      Static flag (1 = immortal/don't modify RC, 0 = heap-allocated)
Bits 33-63:  Reserved for future type tags/flags
```

**Constants in `crates/runtime/src/lib.rs`:**
- `HEADER_RC_MASK = 0xffffffff` (low 32 bits)
- `HEADER_STATIC_BIT = 1u64 << 32` (bit 32)
- `HEADER_FLAGS_MASK = 0xffffffff00000000` (high 32 bits)

### Object Types and Memory Layouts

#### 1. Static String Literals (embedded in .rodata section)
```
Offset 0:  i64 header (value: 0x100000000 = static bit set, RC=0)
Offset 8:  i64 length (number of bytes, not including null terminator)
Offset 16: [N x i8] data (UTF-8 bytes, null-terminated for C compatibility)
```

Codegen returns pointer to offset 16 (data field) for C string compatibility.
Generated in `crates/oats/src/codegen/expr.rs` for `Lit::Str` and template literals.

#### 2. Heap-Allocated Strings (from str_concat, number_to_string, etc.)
```
Offset 0:  i64 header (RC initialized to 1, static bit = 0)
Offset 8:  i64 length
Offset 16: [N x i8] data (null-terminated)
```

Allocated by `heap_str_alloc()`, `heap_str_from_cstr()`, `str_concat()`, `number_to_string()`.
Runtime functions return pointer to offset 16 (data).

#### 3. Arrays
```
Offset 0:  i64 header (RC initialized to 1, static bit = 0)
Offset 8:  i64 length (number of elements)
Offset 16: data (element size * length bytes)
```

Allocated by `array_alloc(len, elem_size, elem_kind)` in `crates/runtime/src/lib.rs`.
Returns pointer to offset 0 (base of object, not data).

#### 4. Classes/Objects
```
Offset 0:  i64 header (RC initialized to 1, static bit = 0)
Offset 8:  field 0 (8 bytes)
Offset 16: field 1 (8 bytes)
...
```

Allocated in constructor codegen (`crates/oats/src/codegen/mod.rs`).
Returns pointer to offset 0 (base of object).

### Reference Counting - The Pointer Problem

The RC system must handle **two kinds of pointers**:
1. **Object base pointers** (point to offset 0 where header lives)
2. **String data pointers** (point to offset 16, after header+length)

**Runtime function `get_object_base(p)`** (`crates/runtime/src/lib.rs`, lines 497-535):
- Heuristically determines if pointer is object base or string data pointer
- Checks header validity at offset 0 (if RC looks reasonable, it's an object base)
- If not, checks offset -16 (if valid header there, it's a string data pointer)
- Returns actual object base pointer for RC operations

**RC Operations** (`rc_inc`, `rc_dec` in `crates/runtime/src/lib.rs`):
1. Call `get_object_base(p)` to find actual object base
2. Read header at offset 0
3. Check static bit - if set, return early (don't modify immortal objects)
4. Atomically increment/decrement RC in low 32 bits
5. For `rc_dec`, if RC reaches 0, call destructor (if present) and free

### Critical Rules for Codegen

1. **String literals**: Always emit with static bit set (`0x100000000`), return pointer to data field (index 2)
2. **Heap strings**: Runtime allocates with RC=1, returns pointer to data field
3. **Arrays/Classes**: Runtime allocates with RC=1, returns pointer to base (offset 0)
4. **Parameters/stores**: Call `rc_inc()` when storing pointer to local/field
5. **Returns**: Call `emit_rc_dec_for_locals()` before return to clean up function locals
6. **All pointers**: Safe to pass to `rc_inc`/`rc_dec` - they handle both base and data pointers

### Memory Management Strategy: Rust-Inspired Hybrid Model

The long-term vision includes multiple phases of optimization:

#### Phase 0: Automatic Reference Counting (Current Implementation)
- Foundation: Automatic RC with deterministic destruction
- Compiler injects `rc_inc`/`rc_dec` calls into generated LLVM IR
- Immediate cleanup when reference count reaches zero
- Predictable memory usage without GC pauses

#### Phase 1: Compiler-Managed Stack Allocation (Future)
- **Escape Analysis**: Detect objects that never escape function scope
- Stack allocation for local-only objects using `alloca`
- No RC overhead for stack-allocated objects
- Massive performance boost for temporary objects

#### Phase 2: Cycle-Collecting RC (Future)
- Address primary weakness of RC: reference cycles
- Detect and break cycles between heap objects
- Maintain deterministic destruction for acyclic cases
- Optional fallback to tracing GC for complex cycles

### Promise Object Layout (Future Async Support)

For future async/await implementation:

```c
typedef struct Promise {
    i64 header;                // Standard RC header
    uint8_t status;            // Pending/Resolved/Rejected
    void* result;              // Result value (when resolved)
    Task** waiting_tasks;      // Array of tasks waiting on this promise
    size_t waiting_count;      // Number of waiting tasks
} Promise;
```

---

## Class System Implementation

### Overview

Complete class support including constructors with parameters, field initialization, and method lowering with proper `this` binding.

### Key Components

#### 1. Type System Integration (`crates/oats/src/types.rs`)

**Public `map_ts_type` Function:**
- Moved from local function to public standalone
- Reusable across codebase (main.rs, aot_run.rs, codegen/mod.rs)
- Supports: Number, Boolean, String, Void, Arrays, NominalStruct (class types)

#### 2. Constructor Generation (`crates/oats/src/codegen/mod.rs`)

**`gen_constructor_ir` Method:**
- Unified constructor generation for all classes
- Parses constructor parameters (regular + `public` property parameters)
- Calculates object size: header (8 bytes) + fields (8 bytes each)
- Memory allocation using `malloc`
- Header initialization with refcount=1
- Parameter/local handling with proper mapping
- Constructor body statement lowering
- Returns allocated object pointer

**Memory Layout Calculation:**
```rust
let header_size = 8u64; // RC header
let field_size = fields.len() as u64 * 8; // 8 bytes per field
let total_size = header_size + field_size;
```

#### 3. Parameter Property Handling

**Auto-Assignment Logic:**
```rust
// Auto-assign constructor parameters to matching fields
// (TypeScript shorthand: constructor(public name: string) creates and assigns a field)
for (field_idx, (field_name, _field_type)) in fields.iter().enumerate() {
    if let Some(param_idx) = param_names.iter().position(|pn| pn == field_name) {
        // This field matches a constructor parameter - auto-assign it
        let param_val = f.get_nth_param(param_idx as u32).expect("param missing");
        let field_offset = header_size + (field_idx as u64 * 8);
        
        // Calculate field address and store the value
        let field_ptr = self.builder.build_struct_gep(
            self.context.i8_type(),
            obj_ptr,
            field_offset as u32,
            "field_ptr"
        )?;
        
        let _ = self.builder.build_store(field_ptr_cast, param_val);
        
        // If pointer type, increment RC for the stored reference
        if param_val.get_type().is_pointer_type() {
            let rc_inc_fn = self.get_rc_inc();
            let _ = self.builder.build_call(rc_inc_fn, &[param_val.into()], "rc_inc_field");
        }
    }
}
```

### Constructor Parameter Properties Fix

#### Problem
TypeScript constructor parameter properties with accessibility modifiers (`public`, `private`, `protected`) should automatically create and initialize class fields:

```typescript
export class Person {
    constructor(public name: string) {}
    // Equivalent to:
    // name: string;
    // constructor(name: string) {
    //     this.name = name;
    // }
}
```

#### Solution
The auto-assignment logic in `gen_constructor_ir()` handles this by:
1. Detecting fields that match constructor parameters by name
2. Automatically generating field assignment code
3. Proper RC management for pointer-type fields
4. Memory layout calculation including the auto-created fields

#### Example Generated IR Pattern
```llvm
define ptr @Person_constructor(ptr %this, ptr %name) {
    ; Store parameter to field
    %field_ptr = getelementptr i8, ptr %this, i64 8  ; skip header
    store ptr %name, ptr %field_ptr
    
    ; Increment RC for stored pointer
    call void @rc_inc(ptr %name)
    
    ret ptr %this
}
```

---

## Unary Operators Implementation

### Overview

Successfully implemented all unary operators for TypeScript-to-LLVM compilation including arithmetic, logical, bitwise, and update operators.

### Implemented Operators

#### 1. Arithmetic Unary Operators

**Unary Minus (`-`)**: Negates numeric values
- LLVM: `fneg` instruction for floating-point negation
- Example: `-x` where x=5 yields -5

**Unary Plus (`+`)**: Type coercion to number (no-op for numbers)
- Implemented as identity operation (returns value as-is)
- Example: `+x` where x=5 yields 5

#### 2. Logical Operators

**Logical NOT (`!`)**: Boolean negation
- LLVM: `xor i1 %val, true` instruction
- Converts operand to boolean, then inverts
- Example: `!true` yields false

#### 3. Bitwise Operators

**Bitwise NOT (`~`)**: Two's complement bitwise inversion
- LLVM: Convert to i32, apply `xor`, convert back to f64
- Operations: `fptosi` → `xor` → `sitofp`
- Example: `~5` yields -6 (two's complement)

#### 4. Update Operators

**Postfix Increment (`x++`)**: Returns old value, increments variable
- LLVM: Load → Add 1.0 → Store → Return old value
- Example: `x++` where x=5 returns 5, sets x to 6

**Postfix Decrement (`x--`)**: Returns old value, decrements variable
- LLVM: Load → Subtract 1.0 → Store → Return old value
- Example: `x--` where x=5 returns 5, sets x to 4

**Prefix Increment (`++x`)**: Increments variable, returns new value
- LLVM: Load → Add 1.0 → Store → Return new value
- Example: `++x` where x=5 returns 6, sets x to 6

**Prefix Decrement (`--x`)**: Decrements variable, returns new value
- LLVM: Load → Subtract 1.0 → Store → Return new value
- Example: `--x` where x=5 returns 4, sets x to 4

### Implementation Details

#### AST Structure (deno_ast)
```rust
// Unary operators: -, +, !, ~
UnaryExpr {
    op: UnaryOp,
    arg: Box<Expr>,
}

// Update operators: ++, --
UpdateExpr {
    op: UpdateOp,      // PlusPlus, MinusMinus
    prefix: bool,      // true for ++x, false for x++
    arg: Box<Expr>,
}
```

#### LLVM IR Generation Patterns

**Logical NOT:**
```rust
fn lower_logical_not(&self, operand: BasicValueEnum) -> Result<BasicValueEnum, Diagnostic> {
    let condition_val = self.to_condition_i1(operand)?;
    let negated = self.builder.build_not(condition_val, "logical_not")?;
    Ok(negated.into())
}
```

**Bitwise NOT:**
```rust
fn lower_bitwise_not(&self, operand: BasicValueEnum) -> Result<BasicValueEnum, Diagnostic> {
    let int_val = self.builder.build_float_to_signed_int(
        operand.into_float_value(),
        self.context.i32_type(),
        "to_i32"
    )?;
    let inverted = self.builder.build_not(int_val, "bitwise_not")?;
    let result = self.builder.build_signed_int_to_float(
        inverted,
        self.context.f64_type(),
        "to_f64"
    )?;
    Ok(result.into())
}
```

**Update Operators:**
```rust
fn lower_update_expr(&self, update: &UpdateExpr) -> Result<BasicValueEnum, Diagnostic> {
    let target_ptr = self.lower_expr_as_lvalue(&update.arg)?;
    let current_val = self.builder.build_load(f64_type, target_ptr, "current")?;
    
    let new_val = match update.op {
        UpdateOp::PlusPlus => self.builder.build_float_add(
            current_val.into_float_value(),
            f64_type.const_float(1.0),
            "increment"
        )?,
        UpdateOp::MinusMinus => self.builder.build_float_sub(
            current_val.into_float_value(),
            f64_type.const_float(1.0),
            "decrement"
        )?,
    };
    
    self.builder.build_store(target_ptr, new_val)?;
    
    if update.prefix {
        Ok(new_val.into()) // Return new value for prefix
    } else {
        Ok(current_val)    // Return old value for postfix
    }
}
```

### Error Handling and Type Safety

**Type Checking:**
- Arithmetic operators require numeric operands
- Logical operators convert operands to boolean
- Update operators require mutable lvalue expressions
- Bitwise operators work on integers (with float conversion)

**Edge Cases:**
- NaN handling in floating-point operations
- Overflow behavior in integer conversions
- Proper lvalue resolution for update operators

---

## Object Layout & Type Representation

### Type System (`crates/oats/src/types.rs`)

#### Core Types
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum OatsType {
    Number,
    Boolean,
    String,
    Void,
    Array(Box<OatsType>),
    NominalStruct {
        name: String,
        fields: Vec<(String, OatsType)>,
    },
    // Future: Promise(Box<OatsType>), Union(Vec<OatsType>), Tuple(Vec<OatsType>)
}
```

#### LLVM Type Mapping
- **Primitives**: `number` → `f64`, `boolean` → `i1`/`i8`, `string` → `i8*`
- **Arrays**: All arrays → `i8*` (opaque pointer to heap object)
- **Classes**: All classes → `i8*` (opaque pointer to heap object)
- **Void**: `void` (no LLVM representation)

### Field Layout Strategy

#### Classes (Nominal Structs)
```rust
// TypeScript class:
class Point {
    constructor(public x: number, public y: number) {}
}

// Memory layout:
// Offset 0:  RC header (8 bytes)
// Offset 8:  x field (8 bytes, f64)
// Offset 16: y field (8 bytes, f64)
// Total: 24 bytes
```

#### Arrays (Homogeneous Elements)
```rust
// TypeScript array:
let numbers: number[] = [1, 2, 3];

// Memory layout:
// Offset 0:  RC header (8 bytes)
// Offset 8:  length = 3 (8 bytes, i64)
// Offset 16: element data (3 * 8 = 24 bytes)
// Total: 40 bytes
```

### Coercion and Conversion Rules

#### Numeric Coercion
```rust
pub fn coerce_to_f64(&self, val: BasicValueEnum) -> Result<FloatValue, Diagnostic> {
    match val {
        BasicValueEnum::FloatValue(f) => Ok(f),
        BasicValueEnum::IntValue(i) => {
            Ok(self.builder.build_signed_int_to_float(i, self.context.f64_type(), "coerce")?)
        }
        _ => Err(Diagnostic::simple("Cannot coerce to number"))
    }
}
```

#### Boolean Conversion
```rust
pub fn to_condition_i1(&self, val: BasicValueEnum) -> Result<IntValue, Diagnostic> {
    match val {
        BasicValueEnum::IntValue(i) if i.get_type().get_bit_width() == 1 => Ok(i),
        BasicValueEnum::FloatValue(f) => {
            let zero = f.get_type().const_zero();
            Ok(self.builder.build_float_compare(FloatPredicate::ONE, f, zero, "to_bool")?)
        }
        BasicValueEnum::PointerValue(p) => {
            let null = p.get_type().const_null();
            Ok(self.builder.build_ptr_diff(p, null, "ptr_to_bool")?)
        }
        _ => Err(Diagnostic::simple("Cannot convert to boolean"))
    }
}
```

---

## Runtime Integration

### Runtime Function Declarations

The codegen declares and links to runtime functions in `crates/runtime/src/lib.rs`:

#### Memory Management
- `malloc(size: i64) -> i8*` - Heap allocation
- `free(ptr: i8*)` - Memory deallocation
- `rc_inc(ptr: i8*)` - Increment reference count
- `rc_dec(ptr: i8*)` - Decrement reference count

#### String Operations
- `str_concat(left: i8*, right: i8*) -> i8*` - String concatenation
- `heap_str_alloc(len: i64) -> i8*` - Allocate heap string
- `number_to_string(val: f64) -> i8*` - Number to string conversion

#### Array Operations
- `array_alloc(len: i64, elem_size: i64, elem_kind: i32) -> i8*` - Array allocation
- `array_get_f64(arr: i8*, index: i64) -> f64` - Get numeric element
- `array_get_ptr(arr: i8*, index: i64) -> i8*` - Get pointer element
- `array_set_ptr(arr: i8*, index: i64, val: i8*)` - Set pointer element

#### I/O Operations
- `print_f64(val: f64)` - Print number
- `print_str(str: i8*)` - Print string

### Runtime Function Usage Patterns

#### String Literal Creation
```rust
// Create static string literal
let header = context.i64_type().const_int(HEADER_STATIC_BIT, false);
let struct_type = context.struct_type(&[i64_type, i64_type, array_type], false);
let global = module.add_global(struct_type, None, &name);
global.set_constant(true);
global.set_initializer(&const_struct);

// Return pointer to data field (offset 16)
builder.build_struct_gep(struct_type, global.as_pointer_value(), 2, "str_data")
```

#### Object Construction
```rust
// Allocate object memory
let size_val = context.i64_type().const_int(total_size, false);
let malloc_fn = self.get_malloc();
let obj_ptr = builder.build_call(malloc_fn, &[size_val.into()], "alloc")?
    .try_as_basic_value().left().unwrap().into_pointer_value();

// Initialize RC header
let header_val = context.i64_type().const_int(1, false); // RC = 1
let header_ptr = builder.build_struct_gep(i8_type, obj_ptr, 0, "header_ptr")?;
builder.build_store(header_ptr, header_val)?;
```

#### Reference Counting
```rust
// Increment RC when storing pointer
builder.build_store(alloca, value_ptr);
let rc_inc_fn = self.get_rc_inc();
builder.build_call(rc_inc_fn, &[value_ptr.into()], "rc_inc");

// Decrement RC for function locals before return
self.emit_rc_dec_for_locals(local_vars);
```

### Testing and Verification

#### Test Coverage
- **Unit Tests**: 40+ tests covering all heap object features
- **Integration Tests**: End-to-end compilation and execution
- **Memory Safety**: No corruption, proper RC semantics
- **Performance**: Efficient allocation and deallocation patterns

#### Test Categories
- `crates/oats/tests/template_literals.rs` - String interpolation with RC
- `crates/oats/tests/arrays_and_loops.rs` - Array operations with RC
- `crates/oats/tests/class_lowering.rs` - Class construction and methods
- `crates/oats/tests/field_write.rs` - Field access and mutation

### Future Runtime Extensions

#### Async/Await Support (Planned)
- Task queue and scheduler implementation
- Promise object management
- Event loop integration with OS I/O primitives
- State machine execution engine

#### Advanced Memory Management (Planned)
- Cycle detection for reference counting
- Optional tracing GC fallback
- Escape analysis for stack allocation
- Memory pool optimization

#### Standard Library (Planned)
- Complete Array/String prototype methods
- Math library integration with libc
- Console API with formatted output
- JSON parsing and serialization

---

## Summary

The Oats compiler architecture is built around a unified memory management system using reference counting with specialized object layouts. The system provides:

1. **Deterministic Memory Management**: Reference counting ensures immediate cleanup
2. **TypeScript Compatibility**: Maintains share-by-reference semantics transparently
3. **Performance**: Optimized object layouts and minimal runtime overhead
4. **Safety**: Comprehensive testing and proper RC semantics
5. **Extensibility**: Clean runtime interface for future language features

This architecture provides a solid foundation for implementing advanced TypeScript features like async/await, generics, and a comprehensive standard library while maintaining the performance characteristics expected from an AOT compiler.