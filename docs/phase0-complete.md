# Phase 0: Separation of Concerns - Complete

This document summarizes the completion of Phase 0 of the Oats toolchain evolution.

## Objectives

Phase 0 aimed to achieve a clean architectural separation between the build orchestrator (toasty) and the compiler (oatsc), following the cargo/rustc model.

## Completed Tasks

✅ **Redefined oatsc API**: Created a `CompileOptions` struct that provides a clean, explicit API for compilation configuration, eliminating the need for environment variable manipulation at the API level.

✅ **Clean Compiler Entry Point**: Added a `compile()` function that accepts `CompileOptions`, making oatsc invokable programmatically with clear contracts.

✅ **Separated Concerns**: Updated toasty to construct `CompileOptions` objects rather than directly setting environment variables, establishing clear boundaries between build orchestration and compilation.

✅ **Module Resolution in Toasty**: Integrated the existing module resolution logic into toasty's build command, making toasty responsible for:

- Discovering the entry point file
- Walking the import graph to find all source files
- Validating that all modules can be parsed

✅ **Testing**: Added comprehensive integration tests for:

- Single-file module resolution
- Multi-file module resolution with imports
- Verified all existing tests still pass

✅ **Documentation**: Created multi-file examples demonstrating the import discovery capability.

## Architecture

### Before Phase 0

```
toasty (CLI) 
  └─> directly calls oatsc::builder::run_from_args()
      └─> reads env vars and command-line args
      └─> compiles single file
```

### After Phase 0

```
toasty (Build Orchestrator)
  ├─> discovers all modules via import graph
  ├─> validates all modules parse correctly
  └─> constructs CompileOptions
      └─> calls oatsc::compile(options)
          └─> pure compilation with explicit configuration
```

## API Example

```rust
// New clean API
let options = oatsc::CompileOptions::new("src/main.oats".to_string());
options.out_dir = Some("target".to_string());
options.opt_level = Some("aggressive".to_string());

let result = oatsc::compile(options)?;
```

## Future Phases

Phase 0 establishes the foundation for:

- **Phase 1**: `Oats.toml` project manifest
  - Now that toasty handles project structure, we can add formal project definitions
  - The CompileOptions API is ready to accept additional flags like `--extern` for dependencies

- **Phase 2**: Dependency Management
  - Toasty will resolve dependencies and invoke oatsc multiple times
  - The module resolution infrastructure is in place

- **Phase 3**: Additional Commands
  - `toasty new`, `toasty check`, `toasty test`
  - Build on the clean separation between orchestration and compilation

## Testing

All existing tests pass (1 pre-existing failure in deno_compat unrelated to these changes).

New tests verify:

- Module resolution discovers all transitive dependencies
- Single-file and multi-file scenarios both work
- Import graph traversal functions correctly

## Notes

- Multi-module compilation (compiling multiple files into one binary) is deferred to a future phase
- Currently, only the entry point is compiled, but all dependencies are discovered and validated
- This provides the architectural foundation without requiring major changes to the compilation pipeline
