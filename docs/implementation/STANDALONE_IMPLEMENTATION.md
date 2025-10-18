# Standalone Toasty Implementation

This document describes the implementation of standalone toasty functionality, allowing the compiler to work without requiring the full Oats repository.

## Overview

The implementation consists of three main components:

### 1. GitHub Actions Workflow (`.github/workflows/build-runtime.yml`)

Automatically builds the runtime library for multiple platforms:
- Linux x86_64
- Linux aarch64 (ARM64)
- macOS x86_64 (Intel)
- macOS aarch64 (Apple Silicon)

**Triggers:**
- Push to main branch (when runtime crate changes)
- Manual workflow dispatch
- Release events

**Outputs:**
- Creates a GitHub release tagged as `runtime-<commit-sha>`
- Uploads platform-specific static libraries as release assets

### 2. Runtime Fetcher (`crates/oatsc/src/runtime_fetch.rs`)

Provides automatic runtime library fetching:

**Features:**
- Detects current platform and fetches appropriate runtime
- Caches downloaded libraries in `~/.cache/oats/runtime/`
- Falls back to local build if fetch fails or platform is unsupported
- Respects environment variables for configuration

**Environment Variables:**
- `OATS_NO_REMOTE_RUNTIME`: Disable remote fetching, always build locally
- `OATS_RUNTIME_CACHE`: Override default cache directory

### 3. Builder Integration (`crates/oatsc/src/builder.rs`)

Modified the build pipeline to:
1. First attempt to fetch pre-built runtime
2. Use cached runtime if available
3. Fall back to local cargo build if needed

**Benefits:**
- Faster compilation (no runtime rebuild needed)
- Standalone toasty works without full repository
- Backward compatible (local build still works)

## Usage Examples

### As a standalone tool (once published):

```bash
# Install toasty
cargo install --git https://github.com/Chrono-byte/oats toasty

# Create and run a program
echo 'export function main(): number { return 42; }' > test.oats
toasty run test.oats
```

### With repository (development):

```bash
# Use remote runtime
cargo run -p toasty -- build examples/add.oats

# Force local build
OATS_NO_REMOTE_RUNTIME=1 cargo run -p toasty -- build examples/add.oats

# Custom cache location
OATS_RUNTIME_CACHE=/tmp/oats_cache cargo run -p toasty -- build examples/add.oats
```

## Testing

The implementation includes a test script (`scripts/test_standalone.sh`) that verifies:
- Local runtime build functionality
- Remote runtime fetch functionality (when available)
- Correct execution of compiled programs

## Dependencies Added

To `crates/oatsc/Cargo.toml`:
- `ureq` (v2.9): HTTP client for downloading runtime
- `serde_json` (v1.0): JSON parsing for GitHub API
- `dirs` (v5.0): Cross-platform directory paths

## Future Improvements

1. Add checksums/signatures for runtime artifacts
2. Support Windows platforms
3. Add runtime version compatibility checking
4. Implement incremental runtime updates
5. Add telemetry for fetch success/failure rates

## Implementation Notes

- The runtime fetcher is designed to fail gracefully
- All operations print warnings rather than errors when remote fetch fails
- The system automatically handles platform detection
- Caching ensures repeated builds are fast
