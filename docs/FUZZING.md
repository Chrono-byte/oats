# Fuzzing Guide for Oats

**Last Updated:** October 16, 2025

This document describes the fuzzing infrastructure for the Oats compiler and how
to use it to discover bugs and crashes.

## Overview

Oats uses `cargo-fuzz` (libFuzzer) to automatically generate test inputs and
discover:

- Parser crashes or panics
- Compiler crashes or infinite loops
- Undefined behavior in unsafe code
- Integer overflows or memory corruption

## Setup

### Prerequisites

Fuzzing requires the Rust nightly toolchain (for sanitizer support):

```bash
# Install nightly toolchain
rustup toolchain install nightly
```

### Install cargo-fuzz

```bash
cargo install cargo-fuzz
```

### Build Fuzzing Targets

```bash
# Build the configured fuzz target
cargo fuzz build fuzz_parser
```

## Available Fuzz Targets

### `fuzz_parser`

**Target:** Parser (`oatsc::parser::parse_oats_module`)

**What it tests:**

- Parsing arbitrary TypeScript/Oats source code
- Handling malformed syntax
- Unicode handling
- Edge cases in AST construction

**Seed corpus:** `fuzz/corpus/fuzz_parser/`

- Contains valid `.oats` files from `examples/`

**Run:**

```bash
cargo +nightly fuzz run fuzz_parser
```

> **Note:** `fuzz/fuzz_targets/fuzz_target_1.rs` is a placeholder. Add a new
> entry to `Cargo.toml` before using it.

## Running Fuzzers

### Quick Test (60 seconds per target)

```bash
./scripts/run_fuzzing.sh
```

The script currently runs `fuzz_parser` (the only configured target) and
respects `FUZZ_TIME` / `FUZZ_JOBS` overrides.

### Continuous Fuzzing (24 hours)

```bash
FUZZ_TIME=86400 ./scripts/run_fuzzing.sh
```

### Single Target with Custom Options

```bash
# Run parser fuzzer for 300 seconds with 8 parallel jobs
cargo +nightly fuzz run fuzz_parser -j 8 -- -max_total_time=300

# Run with memory limit (4GB per job)
cargo +nightly fuzz run fuzz_parser -- -rss_limit_mb=4096

# Run with dictionary (if you create one)
cargo +nightly fuzz run fuzz_parser -- -dict=fuzz/dict/oats.dict
```

### Reproduce a Crash

When fuzzing finds a crash, it saves the input to `fuzz/artifacts/{target}/`.

```bash
# Reproduce the crash
cargo +nightly fuzz run fuzz_parser fuzz/artifacts/fuzz_parser/crash-abc123...

# Run in debugger
cargo +nightly fuzz run fuzz_parser fuzz/artifacts/fuzz_parser/crash-abc123... -- -runs=1
rust-lldb target/x86_64-unknown-linux-gnu/release/fuzz_parser
```

## Understanding Results

### Crash Artifacts

Crashes are saved to `fuzz/artifacts/{target}/`:

- `crash-*` - Input that caused a crash
- `leak-*` - Input that caused a memory leak
- `timeout-*` - Input that caused a timeout
- `oom-*` - Input that caused out-of-memory

### Minimizing Crash Inputs

LibFuzzer can reduce crash inputs to minimal test cases:

```bash
# Minimize a crash input
cargo +nightly fuzz cmin fuzz_parser

# Triage multiple crashes (find unique bugs)
cargo +nightly fuzz tmin fuzz_parser fuzz/artifacts/fuzz_parser/crash-abc123...
```

### Coverage Reporting

See what code paths are being exercised:

```bash
# Generate coverage report
cargo +nightly fuzz coverage fuzz_parser

# View HTML coverage report
cd fuzz/coverage/fuzz_parser/
python3 -m http.server 8000
# Open http://localhost:8000 in browser
```

## Adding New Fuzz Targets

1. **Create target file:** `fuzz/fuzz_targets/fuzz_my_feature.rs`

    ```rust
    #![no_main]
    use libfuzzer_sys::fuzz_target;

    fuzz_target!(|data: &[u8]| {
        // Your fuzzing code here
        // Should NEVER panic or crash
    });
    ```

2. **Update `fuzz/Cargo.toml`:**

    ```toml
    [[bin]]
    name = "fuzz_my_feature"
    path = "fuzz_targets/fuzz_my_feature.rs"
    test = false
    doc = false
    bench = false
    ```

3. **Create seed corpus:** `fuzz/corpus/fuzz_my_feature/`

4. **Add to runner:** Update `scripts/run_fuzzing.sh` TARGETS array

## Best Practices

### DO

✅ Run fuzzing for at least 24 hours before release ✅ Minimize crash inputs
before filing bug reports ✅ Add fixed crashes to regression test suite ✅ Keep
seed corpus up-to-date with new examples ✅ Use
`_guard = oatsc::diagnostics::suppress()` to silence stderr ✅ Check coverage to
find untested code paths

### DON'T

❌ Panic or crash in fuzz targets (defeats the purpose) ❌ Perform I/O
operations (slow, non-deterministic) ❌ Use global mutable state (causes flaky
failures) ❌ Ignore timeouts (might indicate infinite loops) ❌ Fuzz in debug
mode (too slow, use release)

## Continuous Integration

### GitHub Actions Example

```yaml
name: Fuzzing
on: [push, pull_request]

jobs:
  fuzz:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Install Rust nightly
        run: rustup toolchain install nightly
      - name: Install cargo-fuzz
        run: cargo install cargo-fuzz
      - name: Run fuzzing (5 minutes per target)
        run: FUZZ_TIME=300 ./scripts/run_fuzzing.sh
```

## Troubleshooting

### "Fuzzer died before producing any outputs"

- Check that fuzz target compiles: `cargo +nightly fuzz build {target}`
- Verify seed corpus exists and is valid
- Check for panics in target initialization code

### "Slow unit: N seconds"

- Indicates input that takes long to process
- May reveal algorithmic complexity issues (DoS risk)
- Consider adding timeout limits in target code

### No crashes found

- ✅ Good sign! But doesn't guarantee bug-free code
- Increase fuzzing time or parallel jobs
- Review coverage to find untested code paths
- Consider adding structure-aware fuzzing (custom mutators)

## Resources

- [libFuzzer Documentation](https://llvm.org/docs/LibFuzzer.html)
- [cargo-fuzz Book](https://rust-fuzz.github.io/book/cargo-fuzz.html)
- [Fuzzing Rust with AFL](https://rust-fuzz.github.io/book/afl.html)
- [Fuzzing Best Practices](https://github.com/google/fuzzing/blob/master/docs/good-fuzz-target.md)

## Known Limitations

- Fuzzing does not test runtime behavior (compiled binary execution)
- LLVM IR generation is tested but not native code generation
- Resource limits (memory, CPU) must be set explicitly
- Some code paths require specific input structure (consider dictionaries)

## Future Enhancements

- [ ] Add dictionary for TypeScript keywords and operators
- [ ] Structure-aware fuzzing for AST generation
- [ ] Differential testing (compare with TypeScript compiler)
- [ ] Runtime fuzzing (fuzz compiled binaries)
- [ ] AFL++ integration for comparison
