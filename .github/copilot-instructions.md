### **Oats Compiler — AI Contributor Guide**

This guide provides the critical patterns, conventions, and architectural
details required to contribute to the **Oats** compiler codebase. Your primary
goal is to write safe, efficient, and idiomatic Rust code that adheres to these
principles.

---

### \#\# Project Architecture

Oats is an experimental Ahead-of-Time (AOT) compiler that transforms a subset of
TypeScript into native code using LLVM.

- **Workspace Crates**:

  - `crates/oats`: The core compiler logic (parsing, type checking, codegen).
  - `crates/runtime`: The C-callable runtime library that the compiled code
    links against (provides memory management, string/array operations, etc.).

- **Compilation Pipeline**:

  1. **Parsing**: `deno_ast` parses TypeScript source into an Abstract Syntax
     Tree (AST). (`parser.rs`)
  2. **Type Checking**: The AST is traversed to map TypeScript types to the
     internal `OatsType` representation. (`types.rs`)
  3. **Code Generation**: The typed AST is lowered into LLVM IR using the
     `inkwell` crate. (`codegen/*.rs`)
  4. **Linking**: The final object file is linked with the Oats runtime library
     to produce a standalone executable.

---

### \#\# CRITICAL: Heap Object Memory Layout

**This is the most important contract in the compiler.** All heap-allocated
objects in Oats share a unified 64-bit header for reference counting and type
information. Mismanaging this structure will lead to memory corruption.

- **Unified 64-bit Header**: Located at offset `0` for **all** heap objects.

  - **Bits 0-31 (u32)**: Strong reference count (atomic).
  - **Bit 32 (bool)**: Static flag. If `1`, the object is immortal (e.g., a
    string literal) and RC operations are no-ops.
  - **Bits 33-48 (u16)**: Weak reference count.
  - **Bits 49-63**: Reserved for type tags and flags.

- **Object Layouts (Byte Offsets)**:

  - **String**: `[header][i64 length][char data ... NUL]`
    - Codegen functions work with a pointer to the **data** (offset `+16`).
  - **Array**: `[header][i64 length][element data ...]`
    - Codegen functions work with a **base pointer** (offset `+0`).
  - **Class/Object**: `[header][i64 meta_ptr][field data ...]`
    - Codegen functions work with a **base pointer** (offset `+0`).
    - The field at offset `+8` is the **meta-slot**, reserved for internal use.
      **User fields always start at offset `+16`.**

---

### \#\# Reference Counting (RC) Protocol

Manual reference counting is used for all heap objects. The runtime provides
helper functions to manage counts.

- **When to Increment**: Call `rc_inc` **before** storing a pointer into a local
  variable, struct field, or array element.
- **When to Decrement**: Call `rc_dec` on the **old value** before overwriting a
  pointer. Also, call `rc_dec` on all locals before exiting a scope (e.g.,
  before a `return`, `break`, or `continue`).
- **Cleanup**: Use `emit_rc_dec_for_locals(&locals_stack)` to automatically
  clean up variables before exiting a block.
- **Weak Pointers**: For `Weak<T>` types, use the dedicated `rc_weak_inc`,
  `rc_weak_dec`, and `rc_weak_upgrade` functions.
- **Local Variable Tracking**: The `LocalEntry` tuple is used extensively in
  codegen to track variables for proper RC management:
  ```rust
  // (LLVM ptr, LLVM type, is_initialized, is_const, is_weak, nominal_type_name, oats_type)
  type LocalEntry<'a> = (
      PointerValue<'a>,               // alloca ptr
      BasicTypeEnum<'a>,              // LLVM type  
      bool,                           // is_initialized
      bool,                           // is_const
      bool,                           // is_weak
      Option<String>,                 // nominal type name
      Option<crate::types::OatsType>, // OatsType for unions
  );
  ```

---

### \#\# Error Handling: The `Diagnostic` Pattern

**This pattern is non-negotiable.** The compiler must never panic. All functions
involved in parsing, type checking, or codegen must return a `Result` to
gracefully handle errors and report them to the user.

- **Signature**: All fallible functions must return `Result<T, Diagnostic>`.
- **NO `unwrap()` / `expect()`**: These methods are forbidden within the
  `crates/oats/src` directory.
- **Propagation**: Use the `?` operator to propagate errors up the call stack.
- **Creation**: Create new errors using `Diagnostic::simple("Error message")` or
  `Diagnostic::simple_with_span("Error message", byte_span)`.

**Correct Implementation:**

```rust
// Propagate an error from a nested call
pub fn lower_expr(...) -> Result<BasicValueEnum<'a>, Diagnostic> {
    let value = some_fallible_operation()?; // CORRECT: Propagate with `?`
    Ok(value)
}
```

---

### \#\# Development Workflows

- **Setup (Run Once per Session)**: You must source the environment script to
  configure LLVM paths.

  ```bash
  source ./scripts/setup_env.sh
  ```

- **Build and Test**:

  ```bash
  # Build the entire workspace
  cargo build --workspace

  # Run all unit and integration tests
  cargo test --workspace
  ```

- **Compile and Run a File**: Use the `toasty` binary to compile an `.oats`
  file.

  ```bash
  # Compile the example file
  cargo run -p oats --bin toasty -- examples/hello.oats

  # Run the compiled native executable
  ./hello
  ```

- **Snapshot Testing**: The compiler's IR output is verified with `insta`.

  - LLVM IR snapshots are stored in `crates/oats/tests/codegen/snapshots/`.
  - To review changes: `cargo insta review`
  - To accept changes: `cargo insta accept`
  - **Requirement**: Always provide a clear explanation for any snapshot changes
    in your pull request.

- **Running Example Tests**: Use the script to test all examples:

  ```bash
  ./scripts/run_all_proper_tests.sh
  ```

- **Fuzzing**: The project includes fuzzing for parser robustness:

  ```bash
  ./scripts/run_fuzzing.sh
  ```

- **Runtime Diagnostics**: Enable logging for debugging memory management:

  ```bash
  export OATS_RUNTIME_LOG=1      # General runtime logging
  export OATS_COLLECTOR_LOG=1    # Cycle collector logging
  ```

---

### \#\# Testing Structure

The project uses multiple testing approaches that reflect different compiler phases:

- **Unit Tests**: Individual module tests for parsing, type checking, and codegen helpers
- **Integration Tests**: Full compilation pipeline tests in `crates/oats/tests/`
  - `parsing/`: AST parsing validation  
  - `codegen/`: LLVM IR generation with snapshot testing
  - `end_to_end/`: Complete `.oats` → executable compilation
- **Example Tests**: Real programs in `examples/` compiled by `run_all_proper_tests.sh`
- **Snapshot Tests**: LLVM IR output validated with `insta` crate
- **Fuzzing**: Parser robustness testing in `fuzz/` directory

**Oats Program Structure**: All `.oats` files must export a `main()` function:
```typescript
export function main(): void {
    println("Hello from Oats!");
}
```

---

### \#\# Adding a Runtime Function

Adding a new C-callable function to the runtime for the compiler to use is a
**two-step process**. Both are required.

1. **Implement in Runtime (`crates/runtime/src/lib.rs`)**: Add the `extern "C"`
   function to the runtime library.

   ```rust
   #[unsafe(no_mangle)]
   pub extern "C" fn my_new_runtime_helper(arg: i64) -> i64 {
       // ... implementation ...
   }
   ```

2. **Declare in Compiler (`crates/oats/src/codegen/mod.rs`)**: Declare the
   function signature in the `CodeGen` struct so the compiler knows how to call
   it.

   ```rust
   // In the CodeGen struct definition
   pub struct CodeGen<'a> {
       // ...
       pub fn_my_new_helper: RefCell<Option<FunctionValue<'a>>>,
   }

   // In the CodeGen impl block
   impl<'a> CodeGen<'a> {
       fn get_my_new_helper(&self) -> FunctionValue<'a> {
           // Standard lazy-initialization pattern
           if let Some(f) = *self.fn_my_new_helper.borrow() { return f; }
           let fn_type = self.i64_t.fn_type(&[self.i64_t.into()], false);
           let f = self.module.add_function("my_new_runtime_helper", fn_type, None);
           *self.fn_my_new_helper.borrow_mut() = Some(f);
           f
       }
   }
   ```

---

### \#\# Pre-Commit Checklist

1. [ ] Does `cargo test --workspace` pass cleanly?
2. [ ] Does `cargo clippy --workspace` pass cleanly?
3. [ ] Have you added a new test case for your feature or bug fix?
4. [ ] If you changed LLVM IR output, have you run `cargo insta accept` and
       explained the changes?
5. [ ] Have you removed **all** instances of `.unwrap()` or `.expect()` from the
       compiler crate?
6. [ ] If you added a runtime function, did you complete **both** steps
       (implementation and declaration)?
