# Security Policy

## Supported Versions

Oats is currently in experimental/alpha stage. Security updates will be provided for the latest commit on the `master` branch.

| Version | Supported          |
| ------- | ------------------ |
| master  | :white_check_mark: |
| < 0.1.0 | :x:                |

## Security Model

### Threat Model

Oats is an ahead-of-time (AOT) compiler that transforms TypeScript code to native executables via LLVM. The security model considers the following threat vectors:

#### Compiler Threats
- **Malicious Input**: Crafted TypeScript source files designed to cause compiler crashes, infinite loops, or resource exhaustion
- **Resource Exhaustion**: Programs that cause excessive memory allocation or compilation time
- **Parser Exploits**: Malformed syntax that triggers undefined behavior in the parser

#### Runtime Threats
- **Memory Safety**: Buffer overflows, use-after-free, double-free in the reference-counted runtime
- **Integer Overflow**: Arithmetic overflow in size calculations leading to under-allocation
- **Resource Exhaustion**: Heap exhaustion, infinite loops, or excessive memory allocation at runtime
- **Concurrency Issues**: Race conditions in reference counting or cycle collector

### Security Boundaries

**What Oats DOES protect against:**
- ✅ Memory safety violations in safe Rust code
- ✅ Integer overflows in allocation size calculations (via checked arithmetic)
- ✅ Resource exhaustion via configurable heap limits
- ✅ Parser crashes via fuzz testing and validation
- ✅ Reference counting bugs via atomic operations

**What Oats DOES NOT protect against:**
- ❌ Malicious code in the generated executable (Oats compiles whatever you give it)
- ❌ Supply chain attacks (dependencies, LLVM, system libraries)
- ❌ Side-channel attacks (timing, cache, speculative execution)
- ❌ Sandbox escapes (Oats does not provide sandboxing)

### Known Limitations

1. **Extensive `unsafe` Code**: The runtime (`crates/runtime/src/lib.rs`) uses extensive `unsafe` blocks for FFI and low-level memory management. While these have been audited and documented, they remain a potential source of vulnerabilities.

2. **Heuristic Pointer Validation**: The `is_plausible_addr()` function provides heuristic validation but not cryptographic guarantees. Crafted pointers might pass these checks.

3. **Cycle Collection**: The cycle collector is experimental and may have edge cases where cycles are not properly detected.

4. **LLVM Trust**: We trust LLVM to generate safe code. LLVM bugs could lead to miscompilation or undefined behavior.

## Reporting a Vulnerability

**Please do NOT report security vulnerabilities through public GitHub issues.**

### For Critical Vulnerabilities

If you discover a critical vulnerability (e.g., arbitrary code execution, memory corruption, privilege escalation):

1. **Email**: [TBD - Add security contact email]
2. **Subject**: Include "[SECURITY]" in the subject line
3. **Details**: Provide:
   - Description of the vulnerability
   - Steps to reproduce
   - Proof-of-concept code (if available)
   - Affected versions/commits
   - Potential impact assessment

### For Non-Critical Issues

For lower-severity issues (e.g., resource exhaustion, DoS, information disclosure):

1. Open a GitHub issue with the `security` label
2. Provide the same details as above
3. We'll triage within 7 days

### Response Timeline

| Severity | Initial Response | Fix Target | Disclosure |
|----------|-----------------|------------|------------|
| Critical | 24 hours | 7 days | 30 days after fix |
| High | 3 days | 14 days | 45 days after fix |
| Medium | 7 days | 30 days | 60 days after fix |
| Low | 14 days | Best effort | Immediate |

## Security Hardening Features

### Implemented (Phase 1)

#### 1. Integer Overflow Protection
All allocation size calculations use checked arithmetic:
```rust
let total_size = match 16usize.checked_add(str_len) {
    Some(s) => match s.checked_add(1) {
        Some(total) => total,
        None => return ptr::null_mut(), // Graceful failure
    },
    None => return ptr::null_mut(),
};
```

**Functions hardened:**
- `heap_str_alloc()` - String allocation
- `str_concat()` - String concatenation  
- `array_alloc()` - Array allocation
- `runtime_malloc()` - General allocation

#### 2. Resource Limits
Configurable limits prevent resource exhaustion:

```bash
# Limit total heap to 512 MB
export OATS_MAX_HEAP_BYTES=536870912

# Limit single allocation to 64 MB
export OATS_MAX_ALLOC_BYTES=67108864
```

**Defaults:**
- `OATS_MAX_HEAP_BYTES`: 1 GB total heap
- `OATS_MAX_ALLOC_BYTES`: 256 MB per allocation

Limits are enforced atomically before every allocation in `runtime_malloc()`.

#### 3. Fuzz Testing
Comprehensive fuzz testing infrastructure using cargo-fuzz:

```bash
# Run fuzzing (default: 60 seconds)
./scripts/run_fuzzing.sh

# Run for 24 hours
FUZZ_TIME=86400 ./scripts/run_fuzzing.sh
```

**Targets:**
- `fuzz_parser`: Tests parser with arbitrary UTF-8 input
- `fuzz_compiler`: Tests full compilation pipeline

**Results:** Already discovered and fixed 2 DoS vulnerabilities (see `docs/FUZZING_RESULTS.md`)

#### 4. Safety Documentation
Comprehensive safety documentation for all `unsafe` code:
- `docs/RUNTIME_SAFETY.md`: Safety contracts for every unsafe function
- `docs/ARCHITECTURE.md`: Memory layouts and invariants
- Inline `SAFETY:` comments explaining why unsafe code is safe

### Planned (Phase 2)

#### 1. Parser Limits
- **Max AST depth**: Prevent stack overflow from deeply nested structures
- **Compilation timeout**: Prevent infinite loops in compiler
- **Max source size**: Reject excessively large input files

#### 2. Debug Mode Protections
Enable with `OATS_DEBUG_ALLOC=1`:
- Canary values in freed blocks to detect use-after-free
- Allocation tracking to detect memory leaks
- Validate object headers on every RC operation

#### 3. CI/CD Integration
- 5-10 minute fuzz runs on every PR
- 24-hour nightly fuzzing jobs
- Fail CI on new crash artifacts

## Security Best Practices

### For Oats Users

1. **Validate Input**: Don't compile untrusted TypeScript from unknown sources
2. **Set Resource Limits**: Use `OATS_MAX_HEAP_BYTES` and `OATS_MAX_ALLOC_BYTES` in production
3. **Keep Updated**: Use the latest version from `master` for security fixes
4. **Enable Logging**: Use `OATS_RUNTIME_LOG=1` to debug allocation issues

### For Oats Contributors

1. **Minimize `unsafe`**: Only use `unsafe` when absolutely necessary
2. **Document Safety**: Every `unsafe` block must have a `SAFETY:` comment explaining why it's safe
3. **Use Checked Arithmetic**: Always use `checked_add`, `checked_mul` for size calculations
4. **Test Thoroughly**: Add unit tests, fuzz tests, and end-to-end tests for new features
5. **Review Carefully**: Pay extra attention to PRs touching `crates/runtime/src/lib.rs`

## Security Audit History

| Date | Type | Auditor | Findings | Status |
|------|------|---------|----------|--------|
| 2025-10-10 | External Review | Anonymous | 2 critical, 4 high | Fixed |
| TBD | Formal Audit | TBD | Pending | Planned |

## Acknowledgments

We thank the following people for responsibly disclosing security issues:
- Anonymous reviewer (October 2025): Comprehensive security review leading to Phase 1 hardening

## References

- [RUNTIME_SAFETY.md](docs/RUNTIME_SAFETY.md) - Comprehensive safety documentation
- [FUZZING.md](docs/FUZZING.md) - Fuzzing guide and best practices
- [SECURITY_HARDENING.md](docs/SECURITY_HARDENING.md) - Security improvement summary
- [ARCHITECTURE.md](docs/ARCHITECTURE.md) - Memory layouts and contracts

---

**Last Updated:** October 10, 2025  
**Next Review:** January 2026
