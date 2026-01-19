# AGENTS.md - Guidelines for Agentic Coding in Oats

This file provides guidelines for AI coding assistants working on the Oats compiler.

## Build Commands

```bash
# Build entire workspace
cargo build --workspace

# Build specific crate
cargo build --package oatsc
cargo build --package toasty

# Build with optimizations
cargo build --release --workspace

# Run oatsc tests
cargo test --package oatsc

# Run single oatsc test
cargo test --package oatsc --test builder_tests test_builder_single_file
cargo test --package oatsc --test codegen_emit_tests test_emit_arrow_function

# Run toasty e2e tests
cargo test --package toasty --test e2e_tests
cargo test --package toasty --test e2e_tests test_basic_types -- --nocapture

# Run all tests
cargo test --workspace
```

## Project Structure

- **crates/oatsc**: TypeScript-to-LLVM compiler (parsing, type checking, codegen)
- **crates/runtime**: Runtime library (ARC, allocators, cycle collector)
- **crates/toasty**: CLI tool with `build` and `run` commands
- **crates/std**: Standard library (console, fs, net, etc.)
- **examples/**: Example Oats programs
- **scripts/**: Development helper scripts

## Code Style Guidelines

### Imports

- Use `crate::` for internal imports
- Use `use inkwell::...` for LLVM bindings
- Group imports: std → external → internal
- Example:
  ```rust
  use std::collections::HashMap;
  use anyhow::Result;
  use crate::diagnostics::Diagnostic;
  ```

### Formatting

- Use `cargo fmt` before committing
- 4-space indentation
- Max line length: ~100 characters (soft)
- Module documentation comments (`//!`) for public APIs

### Types

- Use `OatsType` enum from `crate::types` for type representation
- Use `DiagnosticResult<T>` = `Result<T, Diagnostic>` for fallible operations
- Use `Option` and `Result` explicitly; never use `.unwrap()`
- Clippy lints are enforced (see Cargo.toml lints section)

### Naming Conventions

- **Snake_case** for functions, variables, modules: `gen_function_ir`, `emit_host_main`
- **PascalCase** for types: `CodeGen`, `OatsType`, `LinkingConfig`
- **SCREAMING_SNAKE_CASE** for constants: `RC_INC`, `OATS_MAIN`
- Prefix internal symbols with `_` if unused: `_unused_var`

### Error Handling

- Use `?` operator for propagating `DiagnosticResult` and `Result`
- Use `map_err` or `inspect_err` to attach context
- Use `anyhow::bail!` for fatal CLI errors
- Never silently ignore errors; always handle or propagate

### Code Patterns

- **Lowering**: Transform AST to LLVM IR via `impl CodeGen`
- **Lazy initialization**: Cache runtime helpers (e.g., `fn_rc_inc`)
- **Escape analysis**: Opt-in RC elision for locals that don't escape scope
- **Monomorphization**: Generic functions specialize at compile-time

### Comments

- Use `//` for implementation comments
- Use `//!` for module-level docs
- Use `///` for public function docs with `# Arguments`, `# Returns` sections
- Comment non-obvious codegen decisions (ABI, alignment, etc.)

### Testing

- Tests use `insta` for snapshot testing
- Use `assert_compiles()` helper from `test_utils`
- Debug output via `eprintln!("DEBUG: ...")` for development

### Things to Avoid

- **Never** use `git stash` - other agents may have uncommitted work
- **Never** use `git checkout` on modified files - it discards others' changes
- **Never** commit without running tests first
- Avoid `unwrap()` in production code (clippy: `unwrap_used = "deny"`)
- Don't modify files you haven't read first

### Working with Other Agents

- Read files before editing them
- Check `git status` before making changes
- Don't use destructive git commands without explicit permission
- Communicate what files you're modifying and why
