# Oats Package System

## Overview

The Oats package system provides a Cargo-inspired, package-based compilation model for managing multi-package projects. It replaces file-based dependency management with a structured, manifest-driven approach.

## Architecture

### Key Components

1. **Oats.toml Manifest**: Defines package metadata and dependencies
2. **Package Graph**: Directed graph of package dependencies
3. **Build Orchestrator**: Manages compilation order and linking
4. **oatsc Compiler**: Package-aware compiler with sandbox restrictions

### Design Principles

- **Package-Based Compilation**: Compile packages as units, not individual files
- **Process Isolation**: Each package compilation is independent
- **Explicit Dependencies**: All external dependencies provided via CLI flags
- **Dependency-First Build Order**: Topological sorting ensures dependencies build first

## Manifest Format

### Basic Structure

```toml
[package]
name = "my-package"
version = "0.1.0"
authors = ["Your Name <email@example.com>"]
description = "Package description"
license = "MIT"
entry = "src/main.oats"  # Entry point (default: src/main.oats)

[dependencies]
# Path-based local dependencies
my-lib = { path = "../my-lib" }
utils = { path = "./utils" }

# Future: Version-based dependencies (not yet implemented)
# serde = "1.0"
```

### Package Metadata

- `name`: Package identifier (alphanumeric, hyphens, underscores)
- `version`: Semantic version string
- `entry`: Relative path to package entry point
- `authors`, `description`, `license`: Optional metadata

### Dependencies

Currently supports path-based dependencies only:

```toml
[dependencies]
package-name = { path = "relative/path/to/package" }
```

Each dependency must:
- Have its own `Oats.toml` manifest
- Use the same name in both manifest and dependency reference
- Be located at the specified path relative to the current package

## Build Process

### 1. Manifest Discovery

```bash
cd my-project
toasty build  # Automatically discovers Oats.toml
```

The build system searches for `Oats.toml` in current directory and parent directories.

### 2. Package Graph Construction

```
app (root)
 ├─> common-lib
 └─> utils
      └─> common-lib
```

The graph builder:
- Recursively discovers dependencies
- Detects circular dependencies
- Validates package names and paths
- Creates nodes for each unique package

### 3. Topological Sort

```
Build order: [common-lib, utils, app]
```

Packages are sorted in dependency-first order:
- Dependencies always compile before dependents
- Circular dependencies cause build failure
- Parallel compilation can be added in future

### 4. Package Compilation

For each package in order:

```bash
oatsc --package-root /path/to/package \
      --extern-pkg common-lib=/path/to/common-lib.oats.meta \
      -o target/package_pkg.o
```

The compiler:
- Loads package entry point from manifest
- Receives dependency metadata via `--extern-pkg` flags
- Emits object file and metadata
- Restricts file access to package directory (future)

### 5. Final Linking

```bash
clang -o myapp \
      target/common_lib_pkg.o \
      target/utils_pkg.o \
      target/app_pkg.o \
      target/release/libruntime.a
```

All package object files are linked with the Oats runtime to produce the final executable.

## Usage Examples

### Creating a New Package

```bash
# Create a new executable package
toasty new myapp

# Create a new library package
toasty new mylib --lib
```

This generates:
```
myapp/
├── Oats.toml
└── src/
    └── main.oats
```

### Multi-Package Project

```
project/
├── app/
│   ├── Oats.toml          # depends on: common, utils
│   └── src/main.oats
├── common/
│   ├── Oats.toml          # no dependencies
│   └── src/lib.oats
└── utils/
    ├── Oats.toml          # depends on: common
    └── src/lib.oats
```

**app/Oats.toml**:
```toml
[package]
name = "app"
version = "0.1.0"
entry = "src/main.oats"

[dependencies]
common = { path = "../common" }
utils = { path = "../utils" }
```

**app/src/main.oats**:
```typescript
import { Logger } from "common";
import { format } from "utils";

export function main(): number {
    const logger = new Logger();
    const msg = format("Hello, World!");
    logger.log(msg);
    return 0;
}
```

Build the app:
```bash
cd app
toasty build
```

Build order will be: `common → utils → app`

## CLI Reference

### toasty Commands

```bash
# Build package or file
toasty build [OPTIONS] [SRC]

# Build and run
toasty run [OPTIONS] [SRC]

# Create new package
toasty new <NAME> [--lib]
```

### Build Options

- `--release`: Build in release mode with optimizations
- `--out-dir <DIR>`: Output directory (default: current directory)
- `--out-name <NAME>`: Override executable name
- `--linker <LINKER>`: Specify linker to use
- `--opt-level <LEVEL>`: Optimization level (none, default, aggressive)
- `--lto <MODE>`: LTO mode (none, thin, full)

### oatsc Flags (Advanced)

```bash
# Legacy single-file mode
oatsc main.oats

# Package mode (used by toasty)
oatsc --package-root /path/to/pkg \
      --extern-pkg dep=/path/to/dep.oats.meta \
      -o output.o
```

## Implementation Details

### Package Graph Data Structure

```rust
pub struct PackageNode {
    pub name: String,
    pub root_dir: PathBuf,
    pub manifest: Manifest,
}

pub type PackageGraph = petgraph::Graph<PackageNode, (), Directed>;
```

Edges represent dependencies: `A → B` means "A depends on B"

### Circular Dependency Detection

```rust
match petgraph::algo::toposort(&graph, None) {
    Ok(sorted) => /* Build in sorted order */,
    Err(cycle) => /* Report cycle and fail */,
}
```

### Build Output

```
target/
├── common_pkg.o           # Object file
├── common_pkg.oats.meta   # Metadata (exports, types)
├── utils_pkg.o
├── utils_pkg.oats.meta
├── app_pkg.o
├── app_pkg.oats.meta
└── app                    # Final executable
```

## Migration Guide

### From File-Based to Package-Based

**Before (single file)**:
```bash
toasty build main.oats
```

**After (package-based)**:
```bash
# Create Oats.toml
cat > Oats.toml << EOF
[package]
name = "myapp"
version = "0.1.0"
entry = "main.oats"

[dependencies]
EOF

# Build automatically detects manifest
toasty build
```

### Migrating Multi-File Projects

1. Create package structure:
   ```bash
   mkdir -p src
   mv *.oats src/
   ```

2. Create manifest:
   ```bash
   toasty new myproject
   ```

3. Split into packages if needed:
   - Identify logical modules
   - Create separate package for each module
   - Add path dependencies in manifests

## Future Enhancements

### Planned Features

- **Parallel Compilation**: Build independent packages concurrently
- **External Dependencies**: Support for Git and registry dependencies
- **Version Resolution**: Semantic versioning and conflict resolution
- **Workspace Support**: Multi-package repositories with shared configuration
- **Build Cache**: Incremental builds based on source changes
- **Cross-Compilation**: Target-specific builds
- **Package Publishing**: Registry for sharing packages

### Package Metadata Format

Future `.oats.meta` files will include:

```json
{
  "package": "mylib",
  "version": "0.1.0",
  "exports": {
    "add": { "signature": "(number, number) -> number" },
    "multiply": { "signature": "(number, number) -> number" }
  },
  "object_file": "mylib_pkg.o",
  "dependencies": ["common"]
}
```

## Troubleshooting

### Common Issues

**Issue**: `No Oats.toml found, using single-file mode`
- **Cause**: Working directory doesn't contain package manifest
- **Fix**: Create `Oats.toml` or use `toasty new`

**Issue**: `Circular package dependency detected`
- **Cause**: Package A depends on B, which depends on A
- **Fix**: Refactor to break cycle (extract common code, invert dependency)

**Issue**: `Dependency path does not exist`
- **Cause**: Path in manifest is incorrect
- **Fix**: Verify relative path from manifest location to dependency

**Issue**: `Package name mismatch`
- **Cause**: Package name in `Oats.toml` doesn't match dependency reference
- **Fix**: Ensure names are identical in both places

### Debugging

Enable verbose output:
```bash
toasty --verbose build
```

This shows:
- Manifest discovery
- Package graph construction
- Build order
- Compilation commands
- Link command

## Examples

See `examples/pkg_test/` for working examples:
- `mylib`: Simple library package
- `myapp`: Application depending on mylib

## Contributing

When adding package system features:
1. Update manifest schema in `crates/toasty/src/manifest.rs`
2. Enhance graph builder in `crates/toasty/src/package_graph.rs`
3. Update build orchestrator in `crates/toasty/src/build.rs`
4. Add tests with example packages
5. Document in this file

## References

- **Issue**: [Implement Package-Based Module System](https://github.com/Chrono-byte/oats/issues/XXX)
- **Design Doc**: Project Plan: A Rust-Inspired Module System for Oats
- **Cargo Book**: [Cargo's package management](https://doc.rust-lang.org/cargo/)
