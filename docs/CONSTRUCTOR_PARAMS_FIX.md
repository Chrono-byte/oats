# Constructor Parameter Properties Fix

## Problem
TypeScript allows a shorthand syntax where constructor parameters with accessibility modifiers (`public`, `private`, `protected`) automatically create and initialize class fields:

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

The compiler was correctly collecting these parameter properties as fields (in `aot_run.rs`), but the constructor codegen (`codegen/mod.rs`) was NOT initializing them - it only stored parameters into local variables but never wrote them to the object's field memory.

## Symptoms
- Class constructed successfully
- Methods could access `this` pointer
- But field values were uninitialized (garbage or zero)
- Example: `person.printName()` would print nothing or crash

## Solution
Added auto-assignment logic in `gen_constructor_ir()` after parameter locals are created:

```rust
// Auto-assign constructor parameters to matching fields
// (TypeScript shorthand: constructor(public name: string) creates and assigns a field)
for (field_idx, (field_name, _field_type)) in fields.iter().enumerate() {
    if let Some(param_idx) = param_names.iter().position(|pn| pn == field_name) {
        // This field matches a constructor parameter - auto-assign it
        let param_val = f.get_nth_param(param_idx as u32).expect("param missing");
        let field_offset = header_size + (field_idx as u64 * 8);
        
        // Calculate field address and store the value
        let field_ptr = ... // pointer arithmetic to field location
        let _ = self.builder.build_store(field_ptr_cast, param_val);
        
        // If pointer type, increment RC for the stored reference
        if param_val.get_type().is_pointer_type() {
            let rc_inc_fn = self.get_rc_inc();
            let _ = self.builder.build_call(rc_inc_fn, &[param_val.into()], "rc_inc_field");
        }
    }
}
```

## Memory Layout
For `class Person { constructor(public name: string) {} }`:

```
Offset 0:  i64 header (RC=1, flags)
Offset 8:  ptr name (field 0)
```

Constructor receives `name` parameter, stores it at offset 8, and increments its RC.

## Testing
- `examples/test_class_simple.oats` - Prints "Alice" correctly
- `examples/comprehensive_test.oats` - Full integration test with classes
- `cargo test -p oats` - All 43 tests passing

## Files Modified
- `crates/oats/src/codegen/mod.rs` - Added field initialization in `gen_constructor_ir()`
- `.github/copilot-instructions.md` - Documented heap object system and constructor behavior

## Related
- Heap Object System docs in copilot-instructions.md
- Constructor params test: `crates/oats/tests/constructor_params.rs`
- Field access tests: `crates/oats/tests/field_write.rs`
