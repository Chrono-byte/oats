# Runtime Safety Documentation

This document provides comprehensive safety documentation for the `unsafe` code blocks in `crates/runtime/src/lib.rs`. Every use of `unsafe` is documented with its invariants, preconditions, and rationale.

## Table of Contents

1. [Memory Safety Model](#memory-safety-model)
2. [Safety Contracts by Function](#safety-contracts-by-function)
3. [Known Limitations and Mitigation Strategies](#known-limitations-and-mitigation-strategies)
4. [Security Hardening Checklist](#security-hardening-checklist)

## Memory Safety Model

### Object Layout Contract

All heap objects follow a unified memory layout with a 64-bit atomic header:

```
Offset 0:  [u64 header] - Atomic, containing:
           - Bits 0-31:  Strong refcount (u32)
           - Bit 32:     Static/immortal flag (1 = immortal, no RC ops)
           - Bits 33-48: Weak refcount (u16)
           - Bits 49-63: Type tag (for discriminated types)

Strings:   [header][i64 len][char data + NUL]
           - Codegen receives pointer to data (offset +16)
           
Arrays:    [header][i64 len][elements...]
           - Returns base pointer (offset 0)
           
Classes:   [header][i64 meta_ptr][fields...]
           - Offset +8 is meta-slot (reserved, never overwritten)
           - Fields start at offset +16

Unions:    [header][dtor_ptr][discriminant][payload]
```

### Pointer Canonicalization

The runtime accepts two types of pointers:
1. **Base pointers**: Point to offset 0 (the header)
2. **String data pointers**: Point to offset +16 (the string data)

The function `get_object_base()` canonicalizes both to base pointers using heuristics:
- Read header at pointer location
- Check if refcount is "reasonable" (< 10000) or static bit is set
- If not, try reading header at `pointer - 16`
- This heuristic is **conservative but not foolproof**

### Thread Safety

- All refcount operations use atomic CAS (Compare-And-Swap) on the full 64-bit header
- `Acquire/Release` ordering ensures destructors observe prior writes
- The cycle collector uses a mutex-protected queue for root candidates
- Race conditions during deallocation are mitigated by the CAS claim pattern

## Safety Contracts by Function

### Reference Counting Functions

#### `rc_inc(p: *mut c_void)`

**Safety Preconditions:**
- `p` must be either:
  - A base pointer to a runtime-allocated object (points to header)
  - A string data pointer (offset +16 from a runtime string object)
  - `null` (handled gracefully, becomes no-op)
- The object must not have been freed
- If `p` points to offset +16, there must be a valid header at offset 0

**Invariants Upheld:**
- Static objects (HEADER_STATIC_BIT set) are never modified (early return)
- Refcount increments are atomic (CAS loop prevents races)
- Only operates on addresses that pass `is_plausible_addr()` check

**Why This is Safe:**
- Null check prevents null dereference
- Plausibility check filters out obvious garbage pointers
- `get_object_base()` canonicalizes string vs base pointers
- Atomic operations prevent data races
- Static objects are never mutated

**Known Risks:**
- Heuristic pointer resolution can be fooled by crafted values
- If object is freed between check and increment, UB may occur (mitigated by application-level ownership)
- Pointer passed from generated code is trusted to be valid

---

#### `rc_dec(p: *mut c_void)`

**Safety Preconditions:**
- `p` must be an **owning** pointer (caller is releasing ownership)
- Object must not have been freed already
- If strong count is 1, caller must not access object after this call

**Invariants Upheld:**
- Atomic CAS ensures only one thread claims destruction (strong count → 0)
- Destructor runs only after successful claim
- Weak count is decremented after destructor completes
- Control block is freed only when both strong and weak counts reach zero

**Why This is Safe:**
- CAS loop ensures exactly one thread performs destruction
- `Acquire` ordering ensures destructor sees all prior writes
- Type tag determines destructor behavior (union dtor vs class field traversal)
- Weak count prevents premature control block deallocation

**Known Risks:**
- If metadata pointer is corrupted, `validate_meta_block()` may fail, leaving pointers un-decremented (memory leak, but not UB)
- Destructor may be called with partially-initialized objects if refcount drops to zero during construction (application logic bug)

---

#### `rc_weak_inc(p: *mut c_void)` / `rc_weak_dec(p: *mut c_void)`

**Safety Preconditions:**
- `p` must be a base pointer to a runtime object (not a string data pointer)
- Object control block must still exist (strong or weak count > 0)

**Invariants Upheld:**
- Weak count occupies bits 33-48 (u16, max 65535)
- Weak increments/decrements use atomic CAS on full header
- Weak count prevents control block deallocation

**Why This is Safe:**
- Operates on same atomic header as strong count
- CAS ensures no races during weak count updates
- Control block is only freed when both strong and weak are zero

**Known Risks:**
- Weak count overflow (> 65535) would corrupt adjacent bits (unlikely in practice)
- If application creates excessive weak references, overflow could occur

---

### Memory Allocation Functions

#### `runtime_malloc(size: size_t)`

**Safety Preconditions:**
- `size` must not cause integer overflow when adding bookkeeping overhead

**Invariants Upheld:**
- Uses `checked_add` to prevent overflow
- Stores allocation size at offset -8 for `runtime_free` to use
- Returns pointer to usable area (after bookkeeping)

**Why This is Safe:**
- `checked_add` prevents overflow, returns null on failure
- Uses `std::alloc` with correct layout
- Null check prevents using invalid allocation

**Known Risks:**
- Large allocations could exhaust heap (no resource limits currently)
- Caller must not exceed allocation size (buffer overflow, not checked here)

---

#### `runtime_free(p: *mut c_void)`

**Safety Preconditions:**
- `p` must have been allocated by `runtime_malloc`
- `p` must not have been freed already (double-free protection not implemented)
- Caller must not access `p` after calling this function

**Invariants Upheld:**
- Reads size from offset -8 (stored by `runtime_malloc`)
- Uses `std::alloc::dealloc` with correct layout

**Why This is Safe:**
- Only frees memory allocated by `runtime_malloc` (size stored at known offset)
- Uses correct layout for deallocation

**Known Risks:**
- **No double-free protection** - calling twice on same pointer is UB
- If `p` was not allocated by `runtime_malloc`, reading size at -8 is UB
- Trusts stored size value (corruption would cause incorrect deallocation)

---

### String Functions

#### `heap_str_alloc(str_len: size_t)`

**Safety Preconditions:**
- `str_len` should not be excessively large (DoS risk)

**Invariants Upheld:**
- Allocates header + length + data + NUL
- Initializes refcount to 1
- Stores length at offset +8

**Why This is Safe:**
- Uses `runtime_malloc` which checks overflow
- Initializes all control data before returning pointer

**Known Risks:**
- **No maximum size check** - attacker could request huge allocation
- Caller must initialize string data before use

---

#### `heap_str_from_cstr(s: *const c_char)`

**Safety Preconditions:**
- `s` must point to a valid NUL-terminated C string
- Memory at `s` must remain valid for duration of call

**Invariants Upheld:**
- Uses `CStr::from_ptr` which trusts NUL termination
- Copies data including NUL byte
- Returns data pointer (offset +16) with refcount = 1

**Why This is Safe:**
- `CStr` finds NUL terminator safely (assumes valid C string)
- Copies exact byte count including NUL
- Returns owned pointer with refcount = 1

**Known Risks:**
- **Trusts NUL termination** - if `s` is not NUL-terminated, `CStr::from_ptr` is UB
- No bounds checking on input length

---

### Cycle Collector Functions

#### `validate_meta_block(meta: *mut u64, max_len: usize)`

**Safety Preconditions:**
- `meta` may be any pointer (function validates it)
- If `meta` passes checks, it must point to readable memory

**Invariants Upheld:**
- Checks pointer is non-null, aligned, and plausible
- Validates magic value (META_MAGIC = 0x4F415453)
- Validates field count is reasonable (1..=max_len)
- Validates each offset is positive, aligned, and within bounds

**Why This is Safe:**
- Conservative checks reduce UB risk
- All pointer derefs are inside `unsafe` block with precondition comments
- Returns `false` on any suspicious value

**Known Risks:**
- **Heuristic validation only** - cannot guarantee memory is valid
- Concurrently freed memory could pass checks but become invalid
- Used for best-effort cycle collection, not correctness-critical path

---

#### `gather_neighbors(obj: *mut c_void)`

**Safety Preconditions:**
- `obj` must be a base pointer to a runtime object
- Object must not have been freed
- Metadata pointers must be valid if present

**Invariants Upheld:**
- Uses type tag to determine object layout
- Validates metadata before dereferencing
- Returns vector of raw pointers (may be null or invalid)

**Why This is Safe:**
- Type tag determines layout (union vs class)
- Uses `validate_meta_block` before reading offsets
- Conservative checks on discriminant values
- Null checks before adding pointers to result

**Known Risks:**
- **Best-effort only** - cannot guarantee all neighbors are found
- Corrupted type tags could cause incorrect layout interpretation
- Caller must null-check and validate returned pointers

---

#### `collector_thread(col: Arc<Collector>)`

**Safety Preconditions:**
- Runs in dedicated background thread
- Objects in queue may have been freed by application threads

**Invariants Upheld:**
- Uses atomic CAS to claim objects before destruction
- Only destroys objects whose strong count can be atomically set to zero
- Validates metadata before dereferencing

**Why This is Safe:**
- CAS claim prevents double-free (only one thread succeeds)
- Conservative checks reduce UB from stale pointers
- Metadata validation before field traversal
- Atomic operations for all refcount reads

**Known Risks:**
- **Stale pointers** - object may be freed between queue insertion and processing
- Heuristic checks not foolproof against all invalid pointers
- Best-effort cycle collection, not guaranteed to be sound

---

## Known Limitations and Mitigation Strategies

### 1. Heuristic Pointer Resolution

**Risk:** `get_object_base()` uses heuristics (refcount < 10000) to distinguish base vs data pointers. Crafted values could fool this check.

**Mitigation:**
- Use magic values in headers (future enhancement)
- Increase heuristic strictness (check type tag, weak count, etc.)
- Add debug mode that tracks all allocated objects

**Current Status:** Relies on generated code producing only valid pointers. Real-world risk is low for non-malicious code.

---

### 2. Integer Overflow in Size Calculations

**Risk:** Array/string allocation size calculations could overflow, leading to under-allocation and buffer overflows.

**Mitigation:**
- ✅ `runtime_malloc` uses `checked_add` (DONE)
- ❌ `heap_str_alloc` does not check `str_len` overflow (TODO)
- ❌ Array allocation functions should use checked arithmetic (TODO)

**Action Items:**
- Audit all size calculations for `checked_mul`, `checked_add`
- Add maximum allocation size limits (e.g., 1 GB)

---

### 3. Double-Free Vulnerabilities

**Risk:** `runtime_free` does not detect double-free. Application bugs or codegen errors could cause crashes or exploits.

**Mitigation Strategies:**
1. **Canary values** (medium overhead): Write magic canary to header on free, check on subsequent free
2. **Tracking allocator** (high overhead, debug only): Maintain hash set of active allocations
3. **Operating system protection** (zero overhead): Rely on modern allocators detecting double-free

**Current Status:** Relies on correct codegen and application logic. Consider adding debug-mode tracking.

---

### 4. Resource Exhaustion (DoS)

**Risk:** Malicious or buggy code could exhaust memory by allocating huge strings/arrays or creating allocation loops.

**Mitigation:**
- Implement maximum heap size limit (configurable)
- Add allocation rate limiting
- Implement timeout for compilation
- Limit maximum AST depth during parsing

**Action Items:** Add resource limits in next milestone.

---

### 5. Cycle Collector Races

**Risk:** Collector may attempt to claim objects being concurrently modified by application threads.

**Mitigation:**
- CAS claim pattern ensures only one thread destroys object
- Conservative checks reduce stale pointer derefs
- Failed CAS retries or aborts claim

**Current Status:** Best-effort collector, not guaranteed to catch all cycles. Tuning and testing needed.

---

## Security Hardening Checklist

### High Priority (Milestone 2)

- [ ] **Add integer overflow checks** to all size calculations
  - [ ] `heap_str_alloc` - check `str_len` overflow before allocation
  - [ ] Array allocation - use `checked_mul` for element count × size
  - [ ] String concat - check total length overflow
  
- [ ] **Implement resource limits**
  - [ ] Maximum heap size (environment variable `OATS_MAX_HEAP_MB`)
  - [ ] Maximum allocation size (e.g., 1 GB per allocation)
  - [ ] Compilation timeout (environment variable `OATS_COMPILE_TIMEOUT_SEC`)
  - [ ] Maximum AST depth during parsing (prevent stack overflow)

- [ ] **Add fuzz testing infrastructure**
  - [ ] Set up `cargo-fuzz` targets for parser
  - [ ] Create fuzzing corpus from `examples/` and `proper_tests/`
  - [ ] Run continuously in CI

### Medium Priority (Milestone 3)

- [ ] **Enhanced header validation**
  - [ ] Add magic value to headers (e.g., 0xBADC0FFE in unused bits)
  - [ ] Validate magic on every RC operation
  - [ ] Detect common corruption patterns

- [ ] **Debug-mode double-free detection**
  - [ ] Add canary values to freed blocks (debug build only)
  - [ ] Optional allocation tracking (hash set of active allocations)
  - [ ] Environment variable `OATS_DEBUG_ALLOC=1` to enable

- [ ] **Improved cycle collector safety**
  - [ ] Add generation counter to headers (detect stale pointers)
  - [ ] Implement tri-color marking instead of trial deletion
  - [ ] Add collector statistics and diagnostics

### Low Priority (Future)

- [ ] **Memory tagging** (ARM MTE, x86 LAM when available)
- [ ] **Sanitizer integration** (AddressSanitizer, MemorySanitizer in debug builds)
- [ ] **Formal verification** of refcount logic using Kani or similar tools

---

## Unsafe Block Audit Summary

| File | Unsafe Blocks | Documented | Reviewed |
|------|---------------|------------|----------|
| `crates/runtime/src/lib.rs` | ~50 | ✅ (this doc) | ✅ |
| `crates/oats/src/codegen/*.rs` | ~5 (transmute) | ⚠️ Minor | ⚠️ Minor |

**Conclusion:** The runtime has extensive `unsafe` usage that is necessary for FFI and low-level memory management. All blocks have been audited and documented. The heuristic checks (`is_plausible_addr`, `validate_meta_block`) provide defense-in-depth but are not formal safety proofs. The design relies on correct codegen producing valid pointers.

**Next Steps:**
1. Implement integer overflow checks (HIGH PRIORITY)
2. Add resource limits (HIGH PRIORITY)
3. Set up fuzz testing (HIGH PRIORITY)
4. Add debug-mode double-free detection (MEDIUM PRIORITY)
