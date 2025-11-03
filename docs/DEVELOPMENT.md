# Development Guide

**Last Updated:** November 3, 2025

Quick guide for contributors. This replaces the old docs.

## Current Status

Oats is an active AOT TypeScript compiler with good language support. We have tests, snapshots, and fuzzing. Parser passes ~90% of TypeScript tests.

## Setup

- Get Rust (rustup) and LLVM 18
- Run `source ./scripts/setup_env.sh`
- Build: `cargo build --workspace`
- Tests: `cargo test --workspace`

## Coding Standards

- Codegen functions return `Result<T, Diagnostic>`, no panics.
- Use `Diagnostic::simple` for errors.

## Common Tasks

- Add runtime helper: in `crates/runtime/src/lib.rs`, declare in `crates/oats/src/codegen/mod.rs`
- Add codegen test: use `crates/oats/tests/` with insta snapshots.

## Quality Gates

- Build: `cargo build --workspace` (must pass)
- Tests: `cargo test --workspace` (must pass)
- Lint: `cargo clippy --workspace` (fix warnings)

## Architecture Quick Reference

### Project Structure

```md
├── crates/
│   ├── oats/          # Compiler (parser, types, codegen)
│   ├── runtime/       # Runtime library
│   ├── oats_ast/      # wip home built AST library
│   ├── oats_parser/   # wip home built parser
│   ├── toasty/        # Test runner
│   └── std/           # Standard library
├── examples/          # Test programs
└── docs/              # Docs
```

### Compilation Pipeline

TypeScript → AST → Typed AST → LLVM IR → Object File → Executable

### Key Abstractions

- `OatsType`: Represents types
- `CodeGen`: IR generation context
- `Diagnostic`: Error reporting

## Error Handling

All codegen returns `Result<T, Diagnostic>`.

Example:

```rust
pub fn lower_expr(...) -> Result<BasicValueEnum<'a>, Diagnostic> {
    let val = some_operation()
        .map_err(|_| Diagnostic::simple("Failed"))?;
    Ok(val)
}
```

For LLVM calls:

```rust
let call_site = self.builder.build_call(fn_val, &args, "name")
    .map_err(|_| Diagnostic::simple("Call failed"))?;
```

Diagnostics:

```rust
Diagnostic::simple("Error message")
```

## Testing

- Unit tests for helpers
- Integration tests with `toasty`
- Snapshots with `insta`
