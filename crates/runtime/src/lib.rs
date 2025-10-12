//! Runtime helpers for the OATS AOT runtime.
//!
//! This crate provides the small C-callable runtime used by the code
//! generator. It implements deterministic reference counting, a simple
//! trial-deletion cycle-collector scaffold, array and string helpers,
//! and union boxing/unboxing helpers used by the compiler.
//!
//! Important runtime contract summary:
//! - Unified 64-bit header at the beginning of heap control blocks:
//!   - Bits 0..31: strong reference count (atomic u32)
//!   - Bit 32: static/immortal flag (1 = static/immortal)
//!   - Bits 33..48: weak reference count (u16)
//!   - Bits 49..63: type tag and flags
//! - Strings, arrays, class objects and union-boxed values are allocated
//!   using the runtime allocator and follow layouts described near the
//!   functions that allocate them. The code generator relies on these
//!   layouts and calls `rc_inc` / `rc_dec` appropriately.
//!
//! Concurrency and memory ordering:
//! - Reference-count updates are performed via atomic compare-and-swap on
//!   the full 64-bit header. The code uses `Acquire/Release` semantics
//!   where required so destructors observe prior writes.
//!

use libc::{c_char, c_void, size_t};
use std::ffi::{CStr, CString};
use std::io::{self, Write};
use std::mem;
use std::process;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
// Header flag constants
// New header layout reserves 16 bits for a weak reference count in bits 33-48.
const HEADER_STATIC_BIT: u64 = 1u64 << 32;
// Low 32 bits are strong refcount
const HEADER_RC_MASK: u64 = 0xffffffffu64;
// Weak count occupies bits 33-48 (16 bits)
const HEADER_WEAK_SHIFT: u64 = 33;
const HEADER_WEAK_MASK: u64 = 0xffffu64 << HEADER_WEAK_SHIFT; // bits 33-48
// Type tag bits start at bit 49 (to avoid colliding with weak count)
const HEADER_TYPE_TAG_SHIFT: u64 = 49;
// Claim bit embedded in the header to avoid a global claimed-set.
// We reserve the top-most bit (bit 63) for the collector claim flag.
const HEADER_CLAIM_BIT: u64 = 1u64 << 63;
// Flags mask includes everything above the low 32-bit refcount
const HEADER_FLAGS_MASK: u64 = 0xffffffff00000000u64;
// Metadata magic used by codegen: ASCII 'OATS' (0x4F415453)
const META_MAGIC: u64 = 0x4F415453u64;

use std::cell::RefCell;

thread_local! {
    // Per-thread recursion guard stack storing object addresses to detect cycles.
    static VISITED_OBJS: RefCell<Vec<usize>> = const { RefCell::new(Vec::new()) };
}

/// Maximum recursion depth for runtime operations (SECURITY LIMIT)
///
/// This hard limit prevents stack overflow from deeply recursive function calls
/// or cyclic data structures. When exceeded, the runtime aborts with an error.
///
/// This limit is NOT configurable and provides defense-in-depth protection
/// against:
/// - Stack overflow from excessive recursion
/// - Infinite loops in recursive algorithms
/// - Malicious deeply nested data structures
///
/// The value of 32 provides reasonable depth for most use cases while preventing
/// stack exhaustion on typical systems (default stack size ~8MB).
const MAX_RECURSION_DEPTH: usize = 128;

// Create a header value for a heap-allocated object with initial refcount
#[inline]
fn make_heap_header(initial_rc: u32) -> u64 {
    (initial_rc as u64) & HEADER_RC_MASK
}

// Helper to extract weak count from a header value
#[inline]
fn header_get_weak_bits(h: u64) -> u64 {
    (h & HEADER_WEAK_MASK) >> HEADER_WEAK_SHIFT
}

// Helper to set weak bits into a header value (weak must fit in 16 bits)
#[inline]
fn header_with_weak(h: u64, weak: u64) -> u64 {
    let cleared = h & !HEADER_WEAK_MASK;
    cleared | ((weak & 0xffffu64) << HEADER_WEAK_SHIFT)
}

// Collector implementation lives in a separate module.
mod collector;
use crate::collector::Collector;

static COLLECTOR: OnceLock<Arc<Collector>> = OnceLock::new();

// Control whether the background collector emits diagnostic logging.
// Disabled by default; enable by setting the environment variable
// OATS_COLLECTOR_LOG=1 before running the generated binary.
static COLLECTOR_LOG: AtomicBool = AtomicBool::new(false);

// Global runtime logging flag for ad-hoc diagnostics. Disabled by default.
// Set OATS_RUNTIME_LOG=1 in the environment to enable.
static RUNTIME_LOG: AtomicBool = AtomicBool::new(false);

// --- Resource Limits (Security Hardening) ---
// These limits prevent malicious or buggy programs from exhausting system resources.
// Configurable via environment variables at runtime initialization.

/// Maximum total heap allocation in bytes (default: 1 GB)
/// Override with OATS_MAX_HEAP_BYTES environment variable
static MAX_HEAP_BYTES: AtomicU64 = AtomicU64::new(1024 * 1024 * 1024);

/// Maximum size for a single allocation in bytes (default: 256 MB)
/// Override with OATS_MAX_ALLOC_BYTES environment variable
static MAX_ALLOC_BYTES: AtomicU64 = AtomicU64::new(256 * 1024 * 1024);

/// Current total allocated bytes (tracked atomically)
static CURRENT_HEAP_BYTES: AtomicU64 = AtomicU64::new(0);

/// Flag indicating resource limits have been initialized
static LIMITS_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Initialize resource limits from environment variables
fn init_resource_limits() {
    // Fast path: already initialized
    if LIMITS_INITIALIZED.load(Ordering::Relaxed) {
        return;
    }

    // Parse OATS_MAX_HEAP_BYTES (default: 1 GB)
    if let Ok(val) = std::env::var("OATS_MAX_HEAP_BYTES")
        && let Ok(limit) = val.parse::<u64>()
    {
        MAX_HEAP_BYTES.store(limit, Ordering::Relaxed);
    }

    // Parse OATS_MAX_ALLOC_BYTES (default: 256 MB)
    if let Ok(val) = std::env::var("OATS_MAX_ALLOC_BYTES")
        && let Ok(limit) = val.parse::<u64>()
    {
        MAX_ALLOC_BYTES.store(limit, Ordering::Relaxed);
    }

    LIMITS_INITIALIZED.store(true, Ordering::Relaxed);
}

/// Check if an allocation would exceed limits, and if not, reserve the space.
/// Returns true if allocation is allowed, false if it would exceed limits.
fn check_and_reserve_allocation(size: u64) -> bool {
    init_resource_limits();

    // Check single allocation limit
    let max_alloc = MAX_ALLOC_BYTES.load(Ordering::Relaxed);
    if size > max_alloc {
        return false;
    }

    // Check total heap limit with atomic compare-exchange loop
    let max_heap = MAX_HEAP_BYTES.load(Ordering::Relaxed);
    loop {
        let current = CURRENT_HEAP_BYTES.load(Ordering::Relaxed);
        let new_total = current.saturating_add(size);

        if new_total > max_heap {
            return false; // Would exceed limit
        }

        // Try to atomically update current allocation
        match CURRENT_HEAP_BYTES.compare_exchange_weak(
            current,
            new_total,
            Ordering::Relaxed,
            Ordering::Relaxed,
        ) {
            Ok(_) => return true, // Successfully reserved
            Err(_) => continue,   // Retry on contention
        }
    }
}

/// Release allocated space back to the pool
fn release_allocation(size: u64) {
    CURRENT_HEAP_BYTES.fetch_sub(size, Ordering::Relaxed);
}

fn init_runtime_log() {
    // Fast-path: if already enabled, do nothing
    if RUNTIME_LOG.load(Ordering::Relaxed) {
        return;
    }
    if std::env::var("OATS_RUNTIME_LOG")
        .map(|v| !v.is_empty() && v != "0")
        .unwrap_or(false)
    {
        RUNTIME_LOG.store(true, Ordering::Relaxed);
    }
}

fn init_collector() -> Arc<Collector> {
    COLLECTOR
        .get_or_init(|| {
            let collector = Collector::new();

            // Configure collector logging from environment (disabled by default).
            if std::env::var("OATS_COLLECTOR_LOG")
                .map(|v| !v.is_empty() && v != "0")
                .unwrap_or(false)
            {
                COLLECTOR_LOG.store(true, Ordering::Relaxed);
            }

            collector.start();

            collector
        })
        .clone()
}

/// Extract the runtime type tag from a full header value while masking out
/// the embedded claim bit. This ensures other helpers can test type tags
/// reliably even when the collector has temporarily set the claim bit.
#[inline]
pub fn header_type_tag(h: u64) -> u64 {
    let raw = h >> HEADER_TYPE_TAG_SHIFT;
    // Remove the claim bit if present in the shifted range.
    let claim_shifted = HEADER_CLAIM_BIT >> HEADER_TYPE_TAG_SHIFT;
    raw & !claim_shifted
}

// FFI: collector control helpers -------------------------------------------------

/// Initialize the background collector (idempotent).
#[unsafe(no_mangle)]
pub extern "C" fn collector_init() {
    let _ = init_collector();
}

/// Shutdown the background collector worker. This stops the background thread
/// but does not reclaim the OnceLock-held instance. Idempotent.
#[unsafe(no_mangle)]
pub extern "C" fn collector_shutdown() {
    let c = init_collector();
    c.stop();
}

/// Force a synchronous collection pass: drain queued roots and process them
/// synchronously on the calling thread. This is useful for tests.
#[unsafe(no_mangle)]
pub extern "C" fn collector_collect_now() {
    let c = init_collector();
    let roots = c.drain_now();
    crate::collector::Collector::process_roots(&roots);
}

// --- Safety helpers for collector traversal ---
// Perform lightweight, conservative checks on raw pointers to avoid
// obviously invalid addresses before dereferencing in the background
// collector. These checks do NOT guarantee safety but reduce the
// chance of immediately dereferencing small/null/unaligned addresses.

#[inline]
fn is_plausible_addr(addr: usize) -> bool {
    // Reject null or very small addresses (below common page sizes),
    // and require 8-byte alignment for our u64-based headers.
    if addr == 0 {
        return false;
    }
    if addr < 4096 {
        return false;
    }
    (addr & 7) == 0
}

// Validate a metadata block pointer: must be plausibly aligned/non-null and
// contain a reasonable length value (1..=max_len). Also check each offset is
// an 8-byte aligned positive value within a pragmatic object-size bound.
//
// Safety contract:
// - `meta` may be any raw pointer; callers must ensure it points to readable
//   memory for at least `1 + len` u64/i32 words as implied by the metadata.
// - This function performs only conservative checks (magic, length, offsets)
//   and DOES NOT guarantee that following dereferences are fully memory-safe
//   in the presence of concurrently freed memory. Callers must only invoke
//   this on pointers that are expected to be heap-allocated by this runtime
//   allocator and not already freed.
// - The function uses raw pointer reads; therefore the caller must ensure the
//   pointer is aligned and non-null. The helper `is_plausible_addr` is used to
//   reduce false positives but is not a formal memory-safety proof.
//
// Where to use:
// - Use prior to dereferencing class metadata blocks that were previously
//   stored by the code generator. If `validate_meta_block` returns `true`, the
//   caller may proceed with cautious reads of the metadata fields.
unsafe fn validate_meta_block(meta: *mut u64, max_len: usize) -> bool {
    if meta.is_null() {
        return false;
    }
    let addr = meta as usize;
    if !is_plausible_addr(addr) {
        return false;
    }
    // Expect layout: [meta0: u64 (magic<<32 | len)], [len x i32 offsets]
    unsafe {
        // Read meta0 (u64)
        let meta0 = *meta;
        let magic = meta0 >> 32;
        if magic != META_MAGIC {
            return false;
        }
        let len = (meta0 & 0xffffffffu64) as usize;
        if len == 0 || len > max_len {
            return false;
        }

        // Offsets now start at meta + 1 as i32 values. Check each.
        // Enforce: offsets are 8-byte aligned, >= header+meta_slot (16), and
        // reasonably bounded to avoid huge or negative values.
        let max_off = 1usize << 20; // conservative 1 MiB object bound
        let min_off = 16usize; // header (8) + meta_slot (8)
        let offsets_ptr = meta.add(1) as *const i32;
        for i in 0..len {
            let off_i32 = *offsets_ptr.add(i);
            // disallow zero or negative offsets
            if off_i32 <= 0 {
                return false;
            }
            let off = off_i32 as isize as usize; // sign-extend if needed
            if (off & 7) != 0 {
                return false;
            }
            if off < min_off || off > max_off {
                return false;
            }
        }
    }
    true
}

// (validate_meta_block unit tests were moved into the consolidated
// tests module at the end of this file to avoid duplicate `mod tests`.)

// The trial-deletion collector implementation was moved into `collector.rs`.

fn add_root_candidate(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    let col = init_collector();
    col.push_root(p as usize);
}

// Test helper: allocate a tiny control block and enqueue it for collector inspection.
// This helper is only included when the `collector-test` Cargo feature is enabled.
#[cfg(feature = "collector-test")]
#[unsafe(no_mangle)]
/// Test-only helper: enqueue a freshly-allocated control block for collector inspection.
///
/// Safety contract:
/// - This symbol is test-only and should not be used in normal runtime code.
/// - The function allocates and enqueues a synthetic heap block; callers need not
///   manage the returned pointer. The function is safe to call from any thread.
pub extern "C" fn collector_test_enqueue() {
    // Allocate minimal heap block: header + one u64 word
    let size = std::mem::size_of::<u64>() * 2;
    let mem = runtime_malloc(size as size_t) as *mut u8;
    if mem.is_null() {
        return;
    }
    // initialize header: refcount=1, no flags
    unsafe {
        let header_ptr = mem as *mut u64;
        *header_ptr = make_heap_header(1);
        // zero next word
        let second = mem.add(std::mem::size_of::<u64>()) as *mut u64;
        *second = 0u64;
    }

    add_root_candidate(mem as *mut c_void);
}

// When the feature is disabled, provide no symbol. This keeps release builds
// free of test-only helpers.

/// Allocate a heap string with runtime header and length field.
///
/// Layout: `[u64 header][u64 length][char data + null terminator]`
///
/// Returns a pointer to the start of the control block (header). Callers
/// that want the C-compatible `char*` should offset by +16 bytes.
/// The returned pointer must be released via `rc_dec_str` when no longer used.
///
/// The refcount is initialized to 1.
///
/// # Security
/// Uses checked arithmetic to prevent integer overflow. Returns null if
/// str_len + overhead would overflow usize or exceed allocation limits.
#[unsafe(no_mangle)]
pub extern "C" fn heap_str_alloc(str_len: size_t) -> *mut c_void {
    unsafe {
        // SECURITY: Use checked arithmetic to prevent integer overflow
        // Total size: 8 (header) + 8 (length) + str_len + 1 (null terminator)
        let total_size = match 16usize.checked_add(str_len) {
            Some(s) => match s.checked_add(1) {
                Some(total) => total,
                None => {
                    if RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            format!(
                                "[oats runtime] heap_str_alloc: integer overflow (str_len={})\n",
                                str_len
                            )
                            .as_bytes(),
                        );
                    }
                    return ptr::null_mut();
                }
            },
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] heap_str_alloc: integer overflow (str_len={})\n",
                            str_len
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        let p = runtime_malloc(total_size);
        if p.is_null() {
            return ptr::null_mut();
        }

        // Initialize header with refcount = 1
        let header_ptr = p as *mut u64;
        *header_ptr = make_heap_header(1);

        // Initialize length
        let len_ptr = (p as *mut u8).add(8) as *mut u64;
        *len_ptr = str_len as u64;

        p
    }
}

/// Create a heap string from a nul-terminated C string and return data pointer (+16).
///
/// # Safety
/// - `s` must be a valid nul-terminated C string. Passing invalid or non-nul-terminated
///   pointers is undefined behavior.
/// - The returned pointer is an owning data pointer; callers must call `rc_dec_str`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn heap_str_from_cstr(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // Use CStr to compute length safely and copy bytes
        let cstr = CStr::from_ptr(s);
        let bytes = cstr.to_bytes_with_nul();
        let len = bytes.len() - 1; // excluding trailing NUL
        let obj = heap_str_alloc(len as size_t);
        if obj.is_null() {
            return ptr::null_mut();
        }

        // Copy string data to offset +16
        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;
        ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr as *mut u8, bytes.len());

        data_ptr
    }
}

/// Convert a data pointer returned to user-space back to the object base pointer.
///
/// This reverses the +16 offset to get back to the header from a heap string data pointer.
///
/// # Safety
/// `data` must be a pointer previously returned by this runtime for string data
/// (i.e., pointer at offset +16). Passing arbitrary pointers is undefined behavior.
#[inline]
unsafe fn heap_str_to_obj(data: *const c_char) -> *mut c_void {
    if data.is_null() {
        return ptr::null_mut();
    }
    unsafe { (data as *mut u8).sub(16) as *mut c_void }
}

/// Increment the strong reference count for a string data pointer.
///
/// This helper accepts a pointer to string data (the pointer returned to
/// user-space code, which points at offset +16 from the header). It
/// resolves the header and delegates to `rc_inc` which handles both
/// string-data and object-base pointers.
///
/// Handles both static and heap strings. For heap strings, the data pointer
/// is at offset +16 from the object header.
///
/// # Safety
/// `data` must be a pointer previously returned by the runtime for a string
/// (either a static literal or a heap-allocated string). Passing arbitrary
/// or already-freed pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_inc_str(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    unsafe {
        // Check if this looks like a heap string by checking if there's a valid header at -16
        // Use a heuristic to detect static/heap pointers: attempt to read the
        // object header and test the static flag bit. This is tolerant to
        // both base-pointer and string-data-pointer inputs.
        let obj = heap_str_to_obj(data);
        rc_inc(obj);
    }
}

/// Decrement the strong reference count for a string data pointer.
///
/// This resolves the header (handles data vs base pointer) and delegates
/// to `rc_dec`. If the reference count reaches zero, destructor logic may
/// run which frees heap storage. Static string literals are immortal and
/// are not freed.
///
/// Handles both static and heap strings.
///
/// # Safety
/// `data` must be a pointer previously returned by the runtime for a string
/// (either a static literal or a heap-allocated string). Passing arbitrary
/// pointers or double-dropping is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_dec_str(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    unsafe {
        let obj = heap_str_to_obj(data);
        rc_dec(obj);
    }
}

// --- Memory Management ---

/// Allocate memory from the runtime allocator with resource limits.
///
/// This wrapper enforces heap size limits and tracks total allocations.
/// The allocator stores bookkeeping data (allocation size) at offset -8
/// from the returned pointer for use by `runtime_free`.
///
/// Resource limits can be configured via environment variables:
/// - OATS_MAX_HEAP_BYTES: Total heap size limit (default: 1 GB)
/// - OATS_MAX_ALLOC_BYTES: Single allocation limit (default: 256 MB)
///
/// Returns null if allocation would exceed limits or if system allocator fails.
/// Returned pointer must be freed with `runtime_free`.
///
/// # Safety
/// Caller must not exceed the allocated size when writing to the returned pointer.
#[unsafe(no_mangle)]
pub fn runtime_malloc(size: size_t) -> *mut c_void {
    // Allocate size + 8 bytes of bookkeeping. Return pointer to the
    // usable area (bookkeeping precedes returned pointer). The bookkeeping
    // stores the total allocation size as a u64 so deallocation can find the
    // original base and layout. This keeps the object-base ABI unchanged
    // for codegen (returned pointer is the start of the object header).
    unsafe {
        if size == 0 {
            return std::ptr::null_mut();
        }
        let total = size.checked_add(std::mem::size_of::<u64>()).unwrap_or(0);
        if total == 0 {
            return std::ptr::null_mut();
        }

        // Check resource limits before allocating
        if !check_and_reserve_allocation(total as u64) {
            // Allocation would exceed limits
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] runtime_malloc: allocation of {} bytes denied (exceeds limits)\n",
                        total
                    )
                    .as_bytes(),
                );
            }
            return std::ptr::null_mut();
        }

        let layout = std::alloc::Layout::from_size_align(total, 8).unwrap();
        let base = std::alloc::alloc(layout);
        if base.is_null() {
            // System allocator failed, release reserved space
            release_allocation(total as u64);
            return std::ptr::null_mut();
        }
        // store total size as u64 at base
        let size_ptr = base as *mut u64;
        *size_ptr = total as u64;
        // return pointer to usable area (after bookkeeping u64)
        base.add(std::mem::size_of::<u64>()) as *mut c_void
    }
}

/// Free memory previously allocated by the runtime allocator and release allocation count.
///
/// # Safety
/// The pointer `p` must have been allocated by `runtime_malloc` (or compatible
/// allocator) and not already freed. Freeing invalid or non-owned pointers is
/// undefined behavior.
#[unsafe(no_mangle)]
pub unsafe fn runtime_free(p: *mut c_void) {
    unsafe {
        if p.is_null() {
            return;
        }
        // compute base pointer where size was stored
        let base = (p as *mut u8).sub(std::mem::size_of::<u64>());
        // read stored size
        let size_ptr = base as *mut u64;
        let total = *size_ptr as usize;
        if total == 0 {
            return;
        }

        // Release allocation from resource tracking
        release_allocation(total as u64);

        let layout = std::alloc::Layout::from_size_align(total, 8).unwrap();
        std::alloc::dealloc(base, layout);
    }
}

// --- String Operations ---

/// Compute the length of a nul-terminated C string.
///
/// # Safety
/// `s` must be a valid pointer to a nul-terminated C string. Passing an
/// invalid pointer or non-nul-terminated memory is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe fn runtime_strlen(s: *const c_char) -> size_t {
    if s.is_null() {
        return 0;
    }
    // Use CStr to compute length safely
    let c = unsafe { CStr::from_ptr(s) };
    c.to_bytes().len() as size_t
}

/// Duplicate a nul-terminated C string into a newly allocated runtime buffer.
///
/// The returned pointer points to a newly allocated C string (no RC header).
/// The caller is responsible for freeing the returned buffer when no longer
/// needed using `runtime_free`.
///
/// # Safety
/// `s` must point to a valid nul-terminated C string.
#[unsafe(no_mangle)]
pub unsafe fn str_dup(s: *const c_char) -> *mut c_char {
    unsafe {
        if s.is_null() {
            return ptr::null_mut();
        }
        let len = libc::strlen(s) + 1;
        let dst = runtime_malloc(len) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(s as *const u8, dst as *mut u8, len);
        dst
    }
}

/// Concatenate two nul-terminated C strings into a newly allocated runtime string with RC header.
///
/// This allocates a heap string with the runtime header and returns a
/// pointer to the data section (offset +16). The returned string is an
/// owning pointer and must be released with `rc_dec_str` when no longer
/// needed.
///
/// # Safety
/// Both `a` and `b` must be valid pointers to nul-terminated C strings.
///
/// # Security
/// Uses checked arithmetic to prevent integer overflow when computing total length.
#[unsafe(no_mangle)]
pub unsafe fn str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    unsafe {
        if a.is_null() || b.is_null() {
            return ptr::null_mut();
        }
        let la = CStr::from_ptr(a).to_bytes().len();
        let lb = CStr::from_ptr(b).to_bytes().len();

        // SECURITY: Check for integer overflow when adding lengths
        let total_len = match la.checked_add(lb) {
            Some(len) => len,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] str_concat: integer overflow (len_a={}, len_b={})\n",
                            la, lb
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        // Allocate heap string with RC header (this also checks for overflow)
        let obj = heap_str_alloc(total_len);
        if obj.is_null() {
            return ptr::null_mut();
        }

        // Get data pointer (offset +16)
        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;

        // Copy both strings
        // copy bytes from CStrs
        let aslice = CStr::from_ptr(a).to_bytes();
        let bslice = CStr::from_ptr(b).to_bytes();
        ptr::copy_nonoverlapping(aslice.as_ptr(), data_ptr as *mut u8, la);
        ptr::copy_nonoverlapping(bslice.as_ptr(), data_ptr.add(la) as *mut u8, lb);

        // Null terminate
        *data_ptr.add(la + lb) = 0;

        data_ptr
    }
}

// --- Printing ---

/// Print a f64 value to stdout with a newline.
///
/// Safe to call from any thread.
#[unsafe(no_mangle)]
pub extern "C" fn print_f64(v: f64) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
    let _ = io::stdout().flush();
}

/// Print a nul-terminated C string to stdout (with newline).
///
/// # Safety
/// `s` must be a valid pointer to a nul-terminated C string. Passing an
/// invalid pointer is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        let _ = io::stdout().write_all(CStr::from_ptr(s).to_bytes());
        let _ = io::stdout().write_all(b"\n");
        let _ = io::stdout().flush();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn print_i32(v: i32) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
    let _ = io::stdout().flush();
}

/// Print a float without trailing newline.
///
/// # Safety
/// The caller must ensure that calling libc::printf with a formatted float is safe
/// in the current environment. This function does not dereference raw pointers.
#[unsafe(no_mangle)]
pub extern "C" fn print_f64_no_nl(v: f64) {
    let _ = io::stdout().write_all(format!("{}", v).as_bytes());
    let _ = io::stdout().flush();
}

/// Print a C string without trailing newline.
///
/// # Safety
/// `s` must be a valid nul-terminated C string pointer. Passing invalid pointers
/// is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str_no_nl(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        let _ = io::stdout().write_all(CStr::from_ptr(s).to_bytes());
        // no newline
    }
}

/// Print just a newline.
///
/// # Safety
/// This function does not dereference raw pointers and is safe to call from any thread.
#[unsafe(no_mangle)]
pub extern "C" fn print_newline() {
    // Write the newline byte to the stdout buffer
    let _ = io::stdout().write_all(b"\n");
    // Explicitly flush the buffer to ensure it's written to the console
    let _ = io::stdout().flush();
    // Also flush C stdio buffers in case other runtime helpers used libc::printf
    // to write to stdout. This avoids interleaving / buffering issues when
    // mixing Rust std::io and libc stdio.
    // We intentionally avoid calling C stdio flush here to remain pure-Rust.
    // If mixing with C stdio is needed, call libc::fflush(ptr::null_mut())
    // in a separate compatibility shim.
}
/// Convert a number to a heap string. The returned pointer is an owning data pointer
/// (offset +16) and must be released with `rc_dec_str`.
///
/// # Safety
/// The function allocates runtime memory and writes to raw pointers. Callers must
/// treat the returned pointer as owning and eventually release it with `rc_dec_str`.
#[allow(clippy::manual_c_str_literals)]
#[unsafe(no_mangle)]
pub extern "C" fn number_to_string(num: f64) -> *mut c_char {
    // Use Rust formatting to produce the string and then create a heap-owned
    // runtime string using the helper that consumes a C string pointer.
    let s = format!("{}", num);
    let c = CString::new(s).unwrap_or_default();
    unsafe { heap_str_from_cstr(c.as_ptr()) }
}

/// Convert an array object to a printable C string. The returned pointer is an owning
/// data pointer (offset +16) and must be released with `rc_dec_str`.
///
/// # Safety
/// `arr` must be a valid array pointer returned by the runtime allocator. Passing
/// invalid pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_to_string(arr: *mut c_void) -> *mut c_char {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // Read header flags to determine if elements are numbers.
        let header_ptr = arr as *const u64;
        let header = *header_ptr;
        // elem_is_number stored in high 32 bits at bit 32
        let flags = (header >> 32) as u32;
        let elem_is_number = (flags & 1) != 0;

        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;

        if elem_is_number {
            // Build a Rust String with comma-and-space separated values
            let mut s = String::new();
            s.push('[');
            for i in 0..len {
                let v = array_get_f64(arr, i);
                if i != 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", v));
            }
            s.push(']');
            // Convert to C string and allocate heap string
            let c = std::ffi::CString::new(s).unwrap_or_default();
            heap_str_from_cstr(c.as_ptr())
        } else {
            // Pointer arrays: iterate elements and attempt to stringify nested arrays/tuples
            let mut s = String::new();
            s.push('[');
            for i in 0..len {
                if i != 0 {
                    s.push_str(", ");
                }
                let p = array_get_ptr(arr, i);
                if p.is_null() {
                    s.push_str("null");
                    continue;
                }
                // Try to heuristically detect if this pointer is an array
                // by reading its header and checking the flags. If it looks
                // like an array, recursively stringify it.
                let mut printed = false;
                let header_ptr = p as *const AtomicU64;
                let header_val = (*header_ptr).load(Ordering::Relaxed);
                // If the high-most type tag bits are zero, treat as array and recurse
                let type_tag = (header_val >> HEADER_TYPE_TAG_SHIFT) as u64;
                if type_tag == 0 {
                    let nested = array_to_string(p);
                    if !nested.is_null() {
                        // read C string
                        let cstr = CStr::from_ptr(nested);
                        if let Ok(str_slice) = cstr.to_str() {
                            s.push_str(str_slice);
                            printed = true;
                        }
                        // release the temporary nested string
                        rc_dec_str(nested);
                    }
                }
                if !printed {
                    // Fallback: try to interpret as a C string (common)
                    let maybe = CStr::from_ptr(p as *const c_char);
                    if let Ok(st) = maybe.to_str() {
                        // wrap with quotes
                        s.push('"');
                        s.push_str(st);
                        s.push('"');
                        printed = true;
                    }
                }
                if !printed {
                    // As a last resort show a pointer summary
                    s.push_str(&format!("<ptr {:p}>", p));
                }
                // release the owning pointer returned by array_get_ptr
                rc_dec(p);
            }
            s.push(']');
            let c = std::ffi::CString::new(s).unwrap_or_default();
            heap_str_from_cstr(c.as_ptr())
        }
    }
}

// Helper: stringify an arbitrary pointer-or-raw-8-bytes value with depth guard.
fn stringify_value_raw(val_raw: u64, depth: usize) -> String {
    if depth > MAX_RECURSION_DEPTH {
        return "...".to_string();
    }
    // Heuristic: treat values that look like plausible addresses as pointers
    let addr = val_raw as usize;
    if is_plausible_addr(addr) {
        let p = addr as *mut c_void;
        // Try array/tuple/string recursion
        let s_ptr = unsafe { array_to_string(p) };
        if !s_ptr.is_null() {
            let s = unsafe { CStr::from_ptr(s_ptr) };
            let res = s.to_string_lossy().into_owned();
            unsafe {
                rc_dec_str(s_ptr);
            }
            return res;
        }
        // Fallback: try to interpret as a C string
        let maybe = unsafe { CStr::from_ptr(p as *const c_char) };
        if let Ok(st) = maybe.to_str() {
            return format!("\"{}\"", st);
        }
        return format!("<ptr {:p}>", p);
    }
    // Otherwise interpret as f64 bits
    let f = f64::from_bits(val_raw);
    format!("{}", f)
}

/// Convert a tuple/object into a printable C string.
///
/// Iterates the tuple metadata block and stringifies each 8-byte field.
/// The returned pointer is an owning data pointer (offset +16) and must
/// be released with `rc_dec_str`.
///
/// # Safety
/// `obj` must be a valid object pointer allocated by this runtime and contain a
/// valid metadata pointer at +8. Passing arbitrary pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn tuple_to_string(obj: *mut c_void) -> *mut c_char {
    if obj.is_null() {
        return ptr::null_mut();
    }
    // Resolve base pointer (handles string-data pointers too)
    let base = unsafe { get_object_base(obj) };
    if base.is_null() {
        return ptr::null_mut();
    }
    // meta pointer stored at offset +8
    let meta_ptr_ptr = unsafe { (base as *mut u8).add(8) as *mut *mut u64 };
    let meta = unsafe { *meta_ptr_ptr };
    if meta.is_null() {
        return ptr::null_mut();
    }
    if unsafe { !validate_meta_block(meta, 1024) } {
        return ptr::null_mut();
    }

    let len = unsafe { ((*meta) & 0xffffffffu64) as usize };
    let offsets_ptr = unsafe { meta.add(1) as *const i32 };

    let mut parts: Vec<String> = Vec::new();
    for i in 0..len {
        let off_i32 = unsafe { *offsets_ptr.add(i) };
        let off = off_i32 as isize as usize;
        let field_addr = unsafe { (base as *mut u8).add(off) as *const u64 };
        let raw = unsafe { *field_addr as u64 };
        let s = stringify_value_raw(raw, 0);
        parts.push(s);
    }
    let joined = parts.join(", ");
    let out = format!("({})", joined);
    let c = std::ffi::CString::new(out).unwrap_or_default();
    unsafe { heap_str_from_cstr(c.as_ptr()) }
}

/// PRNG helper used for `Math.random()`.
///
/// Returns a pseudo-random `f64` value in the range [0, 1). The function
/// ensures libc's PRNG is seeded once per process using high-resolution
/// time and process ID for better entropy.
#[unsafe(no_mangle)]
pub extern "C" fn math_random() -> f64 {
    // Use a small internal SplitMix64-based PRNG seeded once per process.
    // This avoids dependence on libc::rand and provides deterministic but
    // well-distributed values for Math.random() usage in tests/examples.
    use std::sync::atomic::{AtomicU64, Ordering};

    static RNG_STATE: AtomicU64 = AtomicU64::new(0);
    static INIT: std::sync::Once = std::sync::Once::new();

    // splitmix64 next function
    fn splitmix64_next(s: &AtomicU64) -> u64 {
        let mut x = s.load(Ordering::Relaxed);
        // advance by large odd constant
        x = x.wrapping_add(0x9e3779b97f4a7c15);
        s.store(x, Ordering::Relaxed);
        let mut z = x;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        z ^ (z >> 31)
    }

    INIT.call_once(|| {
        // Seed from SystemTime nanos + process id
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(0);
        let pid = process::id() as u64;
        let seed = now ^ (pid.wrapping_mul(0x9e3779b97f4a7c15));
        RNG_STATE.store(seed, Ordering::Relaxed);
    });

    let bits = splitmix64_next(&RNG_STATE);
    // Convert to double in [0,1)
    let mant = bits >> 12; // keep top 52 bits
    let denom = (1u64 << 52) as f64;
    (mant as f64) / denom
}

// --- Reference Counting ---

// --- Reference Counting ---

// --- Array Operations ---
//
// Array Layout: [header: u64][len: u64][capacity: u64][data...]
// header: high 32 bits = flags/type, low 32 bits = refcount
// len: current number of elements
// capacity: allocated space (in number of elements, not bytes)
//
// The header offset for data is 24 bytes (3 * u64).
const ARRAY_HEADER_SIZE: usize = mem::size_of::<u64>() * 3;

// Minimum initial capacity for empty arrays
const MIN_ARRAY_CAPACITY: usize = 8;

// Called when an array index is out-of-bounds. Prints a helpful message
// to stderr and aborts the process to avoid undefined behavior.
fn runtime_index_oob_abort(_arr: *mut c_void, idx: usize, len: usize) -> ! {
    // Try to print a best-effort diagnostic. Avoid panicking inside the
    // runtime; just write to stderr and abort.
    let _ = io::stderr().write_all(b"OATS runtime: array index out of bounds\n");
    let _ = io::stderr().write_all(b"Index: ");
    let s = idx.to_string();
    let _ = io::stderr().write_all(s.as_bytes());
    let _ = io::stderr().write_all(b"\nLength: ");
    let s2 = len.to_string();
    let _ = io::stderr().write_all(s2.as_bytes());
    let _ = io::stderr().write_all(b"\n");
    process::abort();
}

/// Allocate an array on the runtime heap.
///
/// Layout: `[header: u64][len: u64][capacity: u64][data...]`
/// - header: high 32 bits = flags/type, low 32 bits = refcount
/// - length: current number of elements
/// - capacity: allocated space (in number of elements)
///
/// If len is 0, allocates with MIN_ARRAY_CAPACITY to allow for growth.
///
/// # Safety
/// This function performs raw memory allocation and writes to raw pointers.
/// Callers must ensure the returned pointer is handled correctly and not
/// dereferenced from safe Rust code without proper checks.
#[unsafe(no_mangle)]
pub extern "C" fn array_alloc(len: usize, elem_size: usize, elem_is_number: i32) -> *mut c_void {
    // For empty arrays, allocate with minimum capacity to allow push/assignment
    let capacity = if len == 0 { MIN_ARRAY_CAPACITY } else { len };

    // SECURITY: Use checked arithmetic to prevent integer overflow attacks
    // An attacker could pass huge len or elem_size values to cause overflow,
    // leading to under-allocation and buffer overflows.
    let data_bytes = match capacity.checked_mul(elem_size) {
        Some(bytes) => bytes,
        None => {
            // Overflow detected - refuse allocation
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_alloc: integer overflow (capacity={}, elem_size={})\n",
                        capacity, elem_size
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        }
    };

    let total_bytes = match ARRAY_HEADER_SIZE.checked_add(data_bytes) {
        Some(total) => total,
        None => {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    b"[oats runtime] array_alloc: integer overflow in total size calculation\n",
                );
            }
            return ptr::null_mut();
        }
    };

    unsafe {
        let p = runtime_malloc(total_bytes) as *mut u8;
        if p.is_null() {
            return ptr::null_mut();
        }

        // Initialize header with unified format:
        // - Low 32 bits: refcount (initialized to 1)
        // - High 32 bits: flags (elem_is_number flag at bit 32, other bits for future use)
        let header_ptr = p as *mut u64;
        let flags = ((elem_is_number as u64) << 32) & 0xffffffff00000000u64;
        let initial_rc = 1u64;
        *header_ptr = flags | initial_rc;

        // Initialize length.
        let len_ptr = p.add(mem::size_of::<u64>()) as *mut u64;
        *len_ptr = len as u64;

        // Initialize capacity (new field at offset +16).
        let cap_ptr = p.add(mem::size_of::<u64>() * 2) as *mut u64;
        *cap_ptr = capacity as u64;

        // The data area is not zeroed by default.
        p as *mut c_void
    }
}

/// Grow an array to accommodate at least min_capacity elements.
/// Returns a new array pointer with the same refcount and data copied over.
/// The old array is NOT freed - caller is responsible for managing refcounts.
///
/// Uses geometric growth (1.5x) to amortize reallocation costs.
///
/// # Safety
/// Caller must ensure arr is a valid array pointer and manage refcounts appropriately.
unsafe fn array_grow(arr: *mut c_void, min_capacity: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        // Read current metadata
        let header_ptr = arr as *const u64;
        let header = *header_ptr;
        let elem_is_number = ((header >> 32) & 1) as i32;

        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;

        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *const u64;
        let old_capacity = *cap_ptr as usize;

        // Calculate new capacity with geometric growth (1.5x)
        let mut new_capacity = old_capacity + (old_capacity / 2).max(1);
        if new_capacity < min_capacity {
            new_capacity = min_capacity;
        }

        // Determine element size
        let elem_size = if elem_is_number != 0 {
            mem::size_of::<f64>()
        } else {
            mem::size_of::<*mut c_void>()
        };

        // Allocate new array with new_capacity but same length
        // We pass new_capacity as the length parameter to allocate that many elements
        let new_arr = array_alloc(new_capacity, elem_size, elem_is_number);
        if new_arr.is_null() {
            return ptr::null_mut();
        }

        // Update new array's length to match old array's length (not capacity)
        let new_len_ptr = (new_arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        *new_len_ptr = len as u64;

        // Copy data from old to new
        let old_data = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let new_data = (new_arr as *mut u8).add(ARRAY_HEADER_SIZE);
        ptr::copy_nonoverlapping(old_data, new_data, len * elem_size);

        // If pointer array, increment refcounts for copied pointers
        if elem_is_number == 0 {
            let ptrs = new_data as *mut *mut c_void;
            for i in 0..len {
                let p = *ptrs.add(i);
                if !p.is_null() {
                    rc_inc(p);
                }
            }
        }

        new_arr
    }
}

/// Grow an array to accommodate at least min_capacity elements.
/// Returns a new array pointer with the same refcount and data copied over.
/// The old array is NOT freed - caller is responsible for managing refcounts.
///
/// Uses geometric growth (1.5x) to amortize reallocation costs.
///
/// # Safety
/// Caller must ensure arr is a valid array pointer and manage refcounts appropriately.
/// Load a f64 element from an array.
///
/// # Safety
/// Caller must ensure `arr` is a valid array pointer returned by `array_alloc` and
/// `idx` is within bounds. Undefined behavior may occur otherwise.
#[unsafe(no_mangle)]
pub extern "C" fn array_get_f64(arr: *mut c_void, idx: usize) -> f64 {
    if arr.is_null() {
        return 0.0;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<f64>()) as *const f64;
        *elem_ptr
    }
}

/// Load a pointer element from a pointer-typed array and return an owned pointer.
///
/// This helper increments the referenced object's refcount before returning
/// so the caller receives an owning reference. The caller is responsible for
/// eventually calling `rc_dec` on the returned pointer.
///
/// # Safety
/// Caller must ensure `arr` is a valid pointer-typed array. The returned pointer
/// is an owning reference (its refcount has been incremented) and must be
/// released with `rc_dec`.
#[unsafe(no_mangle)]
pub extern "C" fn array_get_ptr(arr: *mut c_void, idx: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<*mut c_void>()) as *const *mut c_void;
        let p = *elem_ptr;
        if !p.is_null() {
            rc_inc(p);
        }
        p
    }
}

/// Return a borrowed pointer from a pointer-typed array without changing refcounts.
///
/// The caller must not store the returned pointer into any long-lived location
/// without first manually calling `rc_inc`. This is useful for short-lived,
/// read-only access where the pointer's lifetime is guaranteed to be short.
///
/// # Safety
/// Caller must ensure `arr` is valid and the borrowed pointer is not stored into
/// long-lived locations without calling `rc_inc` first.
#[unsafe(no_mangle)]
pub extern "C" fn array_get_ptr_borrow(arr: *mut c_void, idx: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<*mut c_void>()) as *const *mut c_void;
        *elem_ptr
    }
}

/// Set a f64 element in an array.
/// Set a f64 value in an array at the given index.
/// If idx >= length, extends the array with zeros and sets the value.
/// If idx >= capacity, reallocates the array and updates the pointer at arr_ptr.
///
/// # Safety
/// Caller must ensure `arr_ptr` points to a valid array pointer variable.
/// The array pointer may be updated if reallocation occurs.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_f64(arr_ptr: *mut *mut c_void, idx: usize, v: f64) {
    if arr_ptr.is_null() {
        return;
    }
    unsafe {
        let mut arr = *arr_ptr;
        if arr.is_null() {
            return;
        }

        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *mut u64;
        let capacity = *cap_ptr as usize;

        // If index is beyond capacity, grow the array
        if idx >= capacity {
            let new_arr = array_grow(arr, idx + 1);
            if new_arr.is_null() {
                return; // allocation failed
            }
            // Decrement old array refcount and update pointer
            rc_dec(arr);
            arr = new_arr;
            *arr_ptr = new_arr;
        }

        // Re-get pointers in case arr changed
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;

        // If index is beyond length, zero-fill and extend length
        if idx >= len {
            let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
            let zeros_start = data_start.add(len * mem::size_of::<f64>()) as *mut f64;
            let zeros_count = idx - len;
            for i in 0..zeros_count {
                *zeros_start.add(i) = 0.0;
            }
            *len_ptr = (idx + 1) as u64;
        }

        // Set the value
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(idx * mem::size_of::<f64>()) as *mut f64;
        *elem_ptr = v;
    }
}

/// Set a pointer into a pointer-typed array, managing refcounts correctly.
/// If idx >= length, extends the array with nulls and sets the value.
/// If idx >= capacity, reallocates the array and updates the pointer at arr_ptr.
///
/// This function increments the new pointer's refcount (if not null) before
/// swapping it into the array and decrements the old pointer's refcount after
/// it has been removed. This ordering prevents races where the old value
/// could be freed while another thread reads it.
///
/// # Safety
/// Caller must ensure `arr_ptr` points to a valid array pointer variable.
/// The array pointer may be updated if reallocation occurs.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_ptr(arr_ptr: *mut *mut c_void, idx: usize, p: *mut c_void) {
    if arr_ptr.is_null() {
        return;
    }
    unsafe {
        let mut arr = *arr_ptr;
        if arr.is_null() {
            return;
        }

        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *mut u64;
        let capacity = *cap_ptr as usize;

        // If index is beyond capacity, grow the array
        if idx >= capacity {
            let new_arr = array_grow(arr, idx + 1);
            if new_arr.is_null() {
                return; // allocation failed
            }
            // Decrement old array refcount and update pointer
            rc_dec(arr);
            arr = new_arr;
            *arr_ptr = new_arr;
        }

        // Re-get pointers in case arr changed
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;

        // If index is beyond length, null-fill and extend length
        if idx >= len {
            let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
            let nulls_start =
                data_start.add(len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
            let nulls_count = idx - len;
            for i in 0..nulls_count {
                *nulls_start.add(i) = ptr::null_mut();
            }
            *len_ptr = (idx + 1) as u64;
        }

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr =
            data_start.add(idx * mem::size_of::<*mut c_void>()) as *mut AtomicPtr<c_void>;

        // Increment the new pointer's refcount *before* swapping it in.
        if !p.is_null() {
            rc_inc(p);
        }

        // Atomically swap the new pointer into the array, getting the old one back.
        let old = (*elem_ptr).swap(p, Ordering::AcqRel);

        // Decrement the old pointer's refcount *after* it has been removed.
        if !old.is_null() {
            rc_dec(old);
        }
    }
}

/// Set a pointer into a pointer-typed array using weak references.
///
/// # Safety
/// `arr` must be a valid pointer-typed array. `p` must be either NULL or a pointer
/// previously returned by this runtime. Passing invalid pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_ptr_weak(arr: *mut c_void, idx: usize, p: *mut c_void) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;
        if idx >= len {
            runtime_index_oob_abort(arr, idx, len);
        }
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr =
            data_start.add(idx * mem::size_of::<*mut c_void>()) as *mut AtomicPtr<c_void>;

        // Increment the new pointer's WEAK refcount *before* swapping it in.
        if !p.is_null() {
            rc_weak_inc(p);
        }

        // Atomically swap the new pointer into the array, getting the old one back.
        let old = (*elem_ptr).swap(p, Ordering::AcqRel);

        // Decrement the old pointer's WEAK refcount *after* it has been removed.
        if !old.is_null() {
            rc_weak_dec(old);
        }
    }
}

/// Push a f64 value onto an existing array.
///
/// # Safety
/// Caller must ensure `arr` is a valid array pointer with sufficient capacity.
#[unsafe(no_mangle)]
pub extern "C" fn array_push_f64(arr: *mut c_void, value: f64) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;
        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *const u64;
        let capacity = *cap_ptr as usize;

        if len >= capacity {
            let _ = io::stderr().write_all(b"OATS runtime: array push capacity exceeded\n");
            let _ = io::stderr().write_all(b"Length: ");
            let s = len.to_string();
            let _ = io::stderr().write_all(s.as_bytes());
            let _ = io::stderr().write_all(b"\nCapacity: ");
            let s2 = capacity.to_string();
            let _ = io::stderr().write_all(s2.as_bytes());
            let _ =
                io::stderr().write_all(b"\nConsider using array[i] = value for dynamic growth\n");
            process::abort();
        }

        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(len * mem::size_of::<f64>()) as *mut f64;
        *elem_ptr = value;
        *len_ptr = (len + 1) as u64;
    }
}

/// Pop a f64 value from an array and return it. If the array is empty returns 0.
///
/// # Safety
/// Caller must ensure `arr` is a valid array pointer.
#[unsafe(no_mangle)]
pub extern "C" fn array_pop_f64(arr: *mut c_void) -> f64 {
    if arr.is_null() {
        return 0.0;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as isize;
        if len <= 0 {
            return 0.0;
        }
        let new_len = (len - 1) as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(new_len * mem::size_of::<f64>()) as *mut f64;
        let val = *elem_ptr;
        // Optionally zero the slot
        *elem_ptr = 0.0;
        *len_ptr = new_len as u64;
        val
    }
}

/// Push a pointer-sized element into a pointer-typed array.
///
/// Increments the pushed pointer's refcount to reflect array ownership.
///
/// # Safety
/// Caller must ensure `arr` is a valid pointer-typed array with sufficient capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_push_ptr(arr: *mut c_void, value: *mut c_void) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
        *elem_ptr = value;
        if !value.is_null() {
            rc_inc(value);
        }
        *len_ptr = (len + 1) as u64;
    }
}

/// Push a pointer-sized element into a pointer-typed array using weak references.
///
/// Increments the pushed pointer's weak refcount to reflect the array's
/// non-owning reference.
///
/// # Safety
/// Caller must ensure `arr` is a valid pointer-typed array with sufficient capacity.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_push_ptr_weak(arr: *mut c_void, value: *mut c_void) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
        *elem_ptr = value;
        if !value.is_null() {
            rc_weak_inc(value);
        }
        *len_ptr = (len + 1) as u64;
    }
}

/// Pop a pointer element from a pointer-typed array and return it.
///
/// Returns NULL if the array is empty. Ownership of the returned pointer is
/// transferred to the caller (no refcount change is performed).
///
/// # Safety
/// Caller must ensure `arr` is a valid pointer-typed array.
#[unsafe(no_mangle)]
pub extern "C" fn array_pop_ptr(arr: *mut c_void) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as isize;
        if len <= 0 {
            return ptr::null_mut();
        }
        let new_len = (len - 1) as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(new_len * mem::size_of::<*mut c_void>()) as *mut *mut c_void;
        let raw = *elem_ptr;
        // Clear the slot to avoid dangling references in the array buffer.
        *elem_ptr = ptr::null_mut();
        *len_ptr = new_len as u64;
        raw
    }
}

// --- Atomic Reference Counting ---

/// Atomically increment the strong reference count of a heap-allocated object.
///
/// This function accepts either an object base pointer (points at header)
/// or a string data pointer (points at offset +16); `get_object_base` is
/// used to resolve the real base pointer. Static/immortal objects are no-ops.
///
/// Handles both object pointers and string data pointers (at offset +16 from header).
///
/// # Safety
/// `p` must be a pointer previously returned by the runtime (string data
/// pointer or object base). Passing arbitrary pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_inc(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        // Try to determine the actual object pointer
        let p_addr = p as usize;
        if !is_plausible_addr(p_addr) {
            // avoid operating on implausible pointer values
            return;
        }
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }

        // Check if this is a static object
        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return; // Static objects are immortal, skip RC
        }

        // Increment RC on the object header. We use a compare_exchange loop
        // on the full 64-bit header so strong/weak bits and flags remain
        // consistent. This avoids separate atomics for weak+strong counts and
        // reduces the chance of race conditions during concurrent updates.
        // Loop performing an atomic compare-exchange on the 64-bit header.
        // We must do this atomically because other threads may be changing
        // the refcount concurrently. When the count reaches zero we run
        // destructor-like cleanup while holding the claim that we are the
        // thread performing finalization for this object.
        loop {
            let old_header = (*header).load(Ordering::Relaxed);
            let rc = old_header & HEADER_RC_MASK;
            let new_rc = rc.wrapping_add(1) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_rc;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(_) => continue, // Spin on contention
            }
        }
    }
}

/// Resolve the base object pointer from a possibly-offset pointer.
///
/// The runtime accepts both object base pointers (header at offset 0) and
/// string data pointers (data pointer at offset +16). This helper heuristically
/// checks for a valid header at the provided pointer and, if not found,
/// attempts to find a header at `p - 16`. The heuristics are intentionally
/// conservative to avoid UB when called with unexpected pointers.
///
/// # Safety
/// The pointer must be a pointer returned by the runtime (either a base
/// pointer or a data pointer). Passing arbitrary pointers can lead to
/// undefined behavior when the function dereferences memory.
#[inline]
unsafe fn get_object_base(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // Check if p looks like an object pointer (has valid header at offset 0)
        let header = p as *const AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);

        // Valid header check: either has static bit, or has reasonable RC value.
        // The header layout is:
        // - bits 0-31: strong refcount
        // - bit 32: static bit
        // - bits 33-48: weak refcount (16 bits)
        // - bits 49-63: type tag
        // We check that the strong count is reasonable and not too large.
        let rc = header_val & HEADER_RC_MASK;
        let is_static = (header_val & HEADER_STATIC_BIT) != 0;

        // Accept if: has static bit set OR (has reasonable RC AND header value > 256)
        // The "header_val > 256" check helps distinguish real headers from random data bytes
        // like single-char strings (e.g., "a\0" = 0x61 0x00 = 97, which is < 256)
        if is_static || (rc > 0 && rc < 10000 && header_val > 256) {
            // Looks like a valid header, use this pointer as-is
            return p;
        }

        // Otherwise, try offset -16 (string data pointer case)
        let obj_ptr = (p as *const u8).sub(16) as *mut c_void;
        let obj_header = obj_ptr as *const AtomicU64;
        let obj_header_val = (*obj_header).load(Ordering::Relaxed);
        let obj_rc = obj_header_val & HEADER_RC_MASK;
        let obj_is_static = (obj_header_val & HEADER_STATIC_BIT) != 0;

        if obj_is_static || (obj_rc > 0 && obj_rc < 10000 && obj_header_val > 256) {
            // Found valid header at -16, this is a string data pointer
            return obj_ptr;
        }

        // Fallback: assume it's an object pointer
        p
    }
}

/// Atomically decrement the strong reference count and free the object if the count reaches zero.
///
/// If the strong count drops to zero, destructor logic (if present) will be
/// invoked and the weak count will be decremented. When both strong and weak
/// counts reach zero the control block is freed.
///
/// Handles both object pointers and string data pointers (at offset +16 from header).
/// No-op for static/immortal objects.
///
/// # Safety
/// `p` must be an owning pointer previously returned by the runtime. Passing
/// invalid pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        // Initialize runtime logging flag lazily and print diagnostic only
        // when OATS_RUNTIME_LOG=1 is set in the environment.
        init_runtime_log();
        if RUNTIME_LOG.load(Ordering::Relaxed) {
            let _ = io::stdout()
                .write_all(format!("[oats runtime] rc_dec called p={:p}\n", p).as_bytes());
            let _ = io::stdout().flush();
        }
        // Quick plausibility check to avoid dereferencing obviously invalid
        // pointers (small integers or near-null values). This prevents the
        // collector or codegen bug from crashing the process while we debug.
        let p_addr = p as usize;
        if !is_plausible_addr(p_addr) {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stdout().write_all(
                    format!("[oats runtime] rc_dec: implausible p={:p}, ignoring\n", p).as_bytes(),
                );
                let _ = io::stdout().flush();
            }
            return;
        }
        // Get the actual object pointer
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }

        // Check if this is a static object
        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return; // Static objects are immortal, skip RC
        }

        loop {
            // Acquire ordering ensures that we see any writes from other threads
            // that were holding a reference before we decrement.
            let old_header = (*header).load(Ordering::Acquire);
            let rc = old_header & HEADER_RC_MASK;

            if rc == 0 {
                // This indicates a double-free or memory corruption.
                // In a real-world scenario, you might abort or log an error.
                return;
            }

            let new_rc = rc.wrapping_sub(1) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_rc;

            // AcqRel ordering:
            // - Acquire: See above.
            // - Release: Ensures that our write to the refcount is visible to any
            //   other thread that might try to acquire a reference.
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    if new_rc == 0 {
                        // An acquire fence ensures that all memory effects that happened
                        // before the last reference was dropped are visible before we run destructor.
                        std::sync::atomic::fence(Ordering::Acquire);

                        // Extract the type tag that indicates a destructor. With the
                        // new layout we expect type tags to be stored in the high bits
                        // above the weak count (shift by HEADER_TYPE_TAG_SHIFT).
                        let type_tag = (old_header >> HEADER_TYPE_TAG_SHIFT) as u32;
                        if type_tag == 1 {
                            // Read the destructor pointer from the second word.
                            let dtor_ptr_ptr = (obj_ptr as *mut u8).add(std::mem::size_of::<u64>())
                                as *mut *mut c_void;
                            let dtor_raw = *dtor_ptr_ptr;
                            if !dtor_raw.is_null() {
                                // SAFETY: we trust the stored pointer is a valid
                                // `extern "C" fn(*mut c_void)`. The pointer was
                                // stored by the emitter (codegen/tests) when the
                                // object was created.
                                let dtor: extern "C" fn(*mut c_void) =
                                    std::mem::transmute(dtor_raw);
                                dtor(obj_ptr);
                            }
                        }

                        // After destructor runs, decrement the weak count on the
                        // control block. The runtime separates object lifetime
                        // (strong refs) from control-block lifetime (weak refs);
                        // `rc_weak_dec` will free the control block once both
                        // counts reach zero.
                        rc_weak_dec(obj_ptr);
                        // rc_weak_dec is responsible for freeing when weak count reaches zero.
                    } else {
                        // The object did not reach zero: it is a candidate for cycle collection.
                        // Add to root list so the background collector can analyze cycles later.
                        add_root_candidate(obj_ptr);
                    }
                    break;
                }
                Err(_) => continue, // Spin on contention
            }
        }
    }
}

/// Atomically increment the weak reference count on an object.
///
/// Weak references are non-owning and do not prevent object destruction.
/// Weak refcounts are used by the runtime to implement weak pointers.
///
/// No-op for static objects.
///
/// # Safety
/// `p` must be a pointer previously returned by the runtime (object base or
/// string data pointer). Passing arbitrary or already-freed pointers is
/// undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_weak_inc(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }
        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return; // static objects never need weak accounting
        }

        loop {
            let old_header = (*header).load(Ordering::Relaxed);
            let weak = header_get_weak_bits(old_header);
            let new_weak = (weak.wrapping_add(1)) & 0xffffu64;
            let new_header = header_with_weak(old_header, new_weak);
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(_) => continue,
            }
        }
    }
}

/// Atomically decrement the weak reference count and free the control block
/// when both strong and weak counts reach zero.
///
/// No-op for static objects.
///
/// # Safety
/// `p` must be a pointer previously returned by the runtime. Passing arbitrary
/// pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_weak_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }
        let header = obj_ptr as *mut AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);
        if (header_val & HEADER_STATIC_BIT) != 0 {
            return; // static objects are immortal
        }

        loop {
            let old_header = (*header).load(Ordering::Acquire);
            let weak = header_get_weak_bits(old_header);
            if weak == 0 {
                // Underflow or double-dec; ignore
                return;
            }
            let new_weak = (weak - 1) & 0xffffu64;
            let new_header = header_with_weak(old_header, new_weak);

            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    // If both strong and weak are now zero, free the object
                    let strong = new_header & HEADER_RC_MASK;
                    let weak_after = header_get_weak_bits(new_header);
                    if strong == 0 && weak_after == 0 {
                        // As an extra barrier, ensure destructor effects are visible
                        std::sync::atomic::fence(Ordering::Acquire);
                        // Clear any claim bit before freeing to avoid other threads
                        // attempting to access the header after we free it.
                        let header_atomic = obj_ptr as *mut AtomicU64;
                        let _ = (*header_atomic).fetch_and(!HEADER_CLAIM_BIT, Ordering::AcqRel);
                        runtime_free(obj_ptr);
                    }
                    break;
                }
                Err(_) => continue,
            }
        }
    }
}

/// Attempt to upgrade a weak pointer into a strong one.
///
/// Returns the object pointer (same as input resolved to base) with strong
/// count incremented if successful, or NULL if the object has already been
/// destroyed (strong==0).
///
/// # Safety
/// `p` must be a pointer previously returned by the runtime and representing
/// a weak reference. Passing arbitrary pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_weak_upgrade(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return ptr::null_mut();
        }

        let header = obj_ptr as *mut AtomicU64;
        loop {
            let old_header = (*header).load(Ordering::Acquire);
            if (old_header & HEADER_STATIC_BIT) != 0 {
                // static objects: just return the base pointer without modifying counts
                return obj_ptr;
            }
            let strong = old_header & HEADER_RC_MASK;
            if strong == 0 {
                return ptr::null_mut(); // object already destroyed
            }
            let new_strong = (strong.wrapping_add(1)) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_strong;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => return obj_ptr,
                Err(_) => continue,
            }
        }
    }
}

// --- Union helpers ---

/// Destructor used for union-boxed objects.
///
/// Reads the discriminant at offset +16 and if the payload is a pointer
/// (discriminant == 1) calls `rc_dec` on the stored pointer to release
/// the nested object reference.
fn union_dtor(obj_ptr: *mut c_void) {
    if obj_ptr.is_null() {
        return;
    }
    unsafe {
        // discriminant stored at offset +16
        let discrim_ptr = (obj_ptr as *mut u8).add(16) as *mut u64;
        let discrim = *discrim_ptr;
        if discrim == 1 {
            // pointer payload at offset +24
            let payload_ptr_ptr = (obj_ptr as *mut u8).add(24) as *mut *mut c_void;
            let payload_raw = *payload_ptr_ptr;
            if !payload_raw.is_null() {
                rc_dec(payload_raw);
            }
        }
        // nothing to do for f64 payload
    }
}

/// Box a numeric payload into a union heap object.
///
/// The returned control block pointer follows the runtime union layout
/// and must be released with `rc_dec` when no longer needed.
#[unsafe(no_mangle)]
pub extern "C" fn union_box_f64(v: f64) -> *mut c_void {
    unsafe {
        // layout: header(8) | dtor_ptr(8) | discrim(8) | payload(8)
        let total = 8 * 4;
        let mem = runtime_malloc(total as size_t) as *mut u8;
        if mem.is_null() {
            return ptr::null_mut();
        }

        // header: type_tag=1 (dtor present) | rc=1 | weak=1
        // weak=1 represents the object's own existence
        let header_ptr = mem as *mut u64;
        let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
        let refcount: u64 = 1u64;
        *header_ptr = header_with_weak(type_tag | refcount, 1);

        // store dtor pointer at offset +8
        let dtor_ptr = mem.add(8) as *mut *mut c_void;
        *dtor_ptr = union_dtor as *mut c_void;

        // discriminant = 0 for f64
        let discrim_ptr = mem.add(16) as *mut u64;
        *discrim_ptr = 0u64;

        // payload f64 at offset +24
        let payload = mem.add(24) as *mut f64;
        *payload = v;

        mem as *mut c_void
    }
}

/// Box a pointer payload into a union heap object (increasing the nested pointer's RC).
///
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime or NULL. The
/// returned control block is an owning object and must be released with `rc_dec`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn union_box_ptr(p: *mut c_void) -> *mut c_void {
    unsafe {
        let total = 8 * 4;
        let mem = runtime_malloc(total as size_t) as *mut u8;
        if mem.is_null() {
            return ptr::null_mut();
        }

        let header_ptr = mem as *mut u64;
        let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
        let refcount: u64 = 1u64;
        *header_ptr = header_with_weak(type_tag | refcount, 1);

        // store dtor
        let dtor_ptr = mem.add(8) as *mut *mut c_void;
        *dtor_ptr = union_dtor as *mut c_void;

        // discriminant = 1 for pointer
        let discrim_ptr = mem.add(16) as *mut u64;
        *discrim_ptr = 1u64;

        // payload pointer at offset +24
        let payload_ptr_ptr = mem.add(24) as *mut *mut c_void;
        *payload_ptr_ptr = p;
        // Ensure the boxed union owns a strong reference to the nested pointer
        // so the payload remains live for the lifetime of the union object.
        if !p.is_null() {
            rc_inc(p);
        }

        mem as *mut c_void
    }
}

/// Unbox a numeric payload from a union object.
///
/// # Safety
/// `u` must be a valid union object previously returned by the runtime.
#[unsafe(no_mangle)]
pub extern "C" fn union_unbox_f64(u: *mut c_void) -> f64 {
    unsafe {
        if u.is_null() {
            return 0.0;
        }
        let payload = (u as *mut u8).add(24) as *mut f64;
        *payload
    }
}

/// Unbox a pointer payload from a union object and return an owning pointer
/// (increments nested object's refcount before returning).
///
/// # Safety
/// `u` must be a valid union object previously returned by the runtime.
#[unsafe(no_mangle)]
pub extern "C" fn union_unbox_ptr(u: *mut c_void) -> *mut c_void {
    unsafe {
        if u.is_null() {
            return ptr::null_mut();
        }
        let payload_ptr_ptr = (u as *mut u8).add(24) as *mut *mut c_void;
        let p = *payload_ptr_ptr;
        if !p.is_null() {
            rc_inc(p);
        }
        p
    }
}

/// Read the discriminant of a union object.
///
/// Returns an integer code describing the payload kind (e.g., 0=f64, 1=pointer).
///
/// # Safety
/// `u` must be a valid union object previously returned by the runtime.
#[unsafe(no_mangle)]
pub extern "C" fn union_get_discriminant(u: *mut c_void) -> i64 {
    unsafe {
        if u.is_null() {
            return -1;
        }
        let discrim_ptr = (u as *mut u8).add(16) as *mut u64;
        *discrim_ptr as i64
    }
}

/// Sleep helper: sleep for the given number of milliseconds.
///
/// Accepts a double (f64) to match the language `number` type and
/// avoid ABI mismatches when calling from generated code.
///
/// # Safety
/// This function does not dereference raw pointers. It simply calls into libc
/// and is safe to call from any thread.
#[unsafe(no_mangle)]
pub extern "C" fn sleep_ms(ms: f64) {
    if ms <= 0.0 {
        return;
    }
    // Use std::thread::sleep which is portable and avoids libc::usleep
    let mut ms_clamped = ms;
    if ms_clamped > 10_000.0 {
        ms_clamped = 10_000.0; // cap at 10s
    }
    let dur = std::time::Duration::from_millis(ms_clamped as u64);
    std::thread::sleep(dur);
}

/// Minimal promise helpers (MVP): promise_resolve + promise_poll_into.
///
/// These are simple helpers to allow compiler wiring and initial testing of
/// `async` lowering. They are intentionally minimal: `promise_resolve`
/// allocates a tiny heap object that holds a resolved value, and
/// `promise_poll_into` recognizes such objects and writes the resolved
/// value into `out_ptr` then returns `1` (ready). Other pointer values are
/// treated as Pending (return 0) in this MVP.
#[unsafe(no_mangle)]
pub extern "C" fn promise_resolve(ptr: *mut std::ffi::c_void) -> *mut std::ffi::c_void {
    unsafe {
        // Allocate a tiny box to hold the pointer and a tag; use libc malloc
        let size = std::mem::size_of::<*mut std::ffi::c_void>() + 8;
        let mem = runtime_malloc(size) as *mut u8;
        if mem.is_null() {
            return std::ptr::null_mut();
        }
        // Store a magic sentinel (usize::MAX) at offset 0 to mark this as a resolved promise
        let header_ptr = mem as *mut usize;
        *header_ptr = usize::MAX; // Magic value to distinguish from wrapper/state
        // store the payload pointer at offset 8
        let payload_ptr = mem.add(8) as *mut *mut std::ffi::c_void;
        *payload_ptr = ptr;
        mem as *mut std::ffi::c_void
    }
}

/// Poll a promise into `out_ptr`. Returns 1 if ready (and writes the
/// resolved value into out_ptr as a pointer), or 0 if pending.
#[unsafe(no_mangle)]
pub extern "C" fn promise_poll_into(
    promise: *mut std::ffi::c_void,
    out_ptr: *mut std::ffi::c_void,
) -> i32 {
    unsafe {
        if promise.is_null() || out_ptr.is_null() {
            return 0;
        }
        // Strategy: support three shapes used by codegen/runtime
        // 1) Legacy: the `promise` pointer is actually the state object and
        //    state[0] is a non-null poll function pointer. We detect this by
        //    reading the first word and calling it if present.
        // 2) Promise wrapper: the `promise` is an allocated wrapper whose
        //    second word (offset 8) points to a state object. We read the
        //    state pointer and dispatch to its poll function (state[0]).
        // 3) Tiny resolver: an earlier `promise_resolve` returns a tiny box
        //    that stores the payload at offset 8; in this case we treat the
        //    second word as the payload and return it as ready.

        // Read the first word
        let first_ptr = promise as *mut *mut std::ffi::c_void;
        let first_word = *first_ptr;

        // Check for resolved promise magic sentinel (usize::MAX)
        let first_as_usize = first_word as usize;
        if first_as_usize == usize::MAX {
            // This is a resolved promise: [usize::MAX, payload]
            // Read payload at offset 8 and return it as ready
            let payload_ptr = (promise as *mut u8).add(8) as *mut *mut std::ffi::c_void;
            let payload = *payload_ptr;
            let out_slot = out_ptr as *mut *mut std::ffi::c_void;
            *out_slot = payload;
            return 1;
        }

        if !first_word.is_null() {
            // Legacy/state-as-promise: first word is a poll function pointer.
            let poll_fn: extern "C" fn(*mut std::ffi::c_void, *mut std::ffi::c_void) -> i32 =
                std::mem::transmute(first_word);
            return poll_fn(promise, out_ptr);
        }

        // Read second word at offset 8
        let second_ptr = (promise as *mut u8).add(8) as *mut *mut std::ffi::c_void;
        let second_word = *second_ptr;
        if second_word.is_null() {
            // Nothing useful; treat as pending
            return 0;
        }

        // If the second word points to a state whose first word is a poll
        // function pointer, dispatch to it: this is the promise wrapper case
        // where promise -> [null, state_ptr].
        let state_first = second_word as *mut *mut std::ffi::c_void;
        let state_first_word = *state_first;
        if !state_first_word.is_null() {
            let poll_fn: extern "C" fn(*mut std::ffi::c_void, *mut std::ffi::c_void) -> i32 =
                std::mem::transmute(state_first_word);
            return poll_fn(second_word, out_ptr);
        }

        // Otherwise, treat second_word as a resolved payload (tiny resolver
        // format used by promise_resolve): write payload into out_ptr and
        // return ready.
        let out_slot = out_ptr as *mut *mut std::ffi::c_void;
        *out_slot = second_word;
        1
    }
}

// --- Phase-1 compatible executor/waker primitives (MVP) ---
// These are minimal shims that preserve the Phase-0 behaviour while
// exposing the symbols and shapes the codegen will expect when emitting
// real async state-machines. Right now they defer to the simple
// `promise_poll_into` behavior for resolved promises.

#[unsafe(no_mangle)]
pub extern "C" fn promise_new_from_state(
    state_ptr: *mut std::ffi::c_void,
) -> *mut std::ffi::c_void {
    // Allocate a small promise wrapper that stores the state pointer at
    // offset 8. Layout: [reserved/null (8)] [state_ptr (8)].
    unsafe {
        let size = std::mem::size_of::<*mut std::ffi::c_void>() * 2;
        let mem = runtime_malloc(size) as *mut u8;
        if mem.is_null() {
            return std::ptr::null_mut();
        }
        let first = mem as *mut *mut std::ffi::c_void;
        *first = std::ptr::null_mut();
        let second = mem.add(8) as *mut *mut std::ffi::c_void;
        *second = state_ptr;
        mem as *mut std::ffi::c_void
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn executor_enqueue(_promise: *mut std::ffi::c_void) {
    if _promise.is_null() {
        return;
    }
    let exec = init_executor();
    let mut q = exec.queue.lock().unwrap();
    q.push_back(_promise as usize);
    exec.cv.notify_one();
}

#[unsafe(no_mangle)]
pub extern "C" fn executor_run() {
    // Drain the queue synchronously until empty.
    let exec = init_executor();
    loop {
        let mut q = exec.queue.lock().unwrap();
        if q.is_empty() {
            break;
        }
        let p_addr = q.pop_front().unwrap();
        let p = p_addr as *mut std::ffi::c_void;
        drop(q);
        unsafe {
            let out_mem = runtime_malloc(std::mem::size_of::<*mut c_void>()) as *mut u8;
            if out_mem.is_null() {
                continue;
            }
            let ready = promise_poll_into(p, out_mem as *mut std::ffi::c_void);
            if ready == 0 {
                // not ready: re-enqueue
                let mut q2 = exec.queue.lock().unwrap();
                q2.push_back(p as usize);
                exec.cv.notify_one();
            } else {
                runtime_free(out_mem as *mut c_void);
                // We don't free `p` because it may be the state object owned by the generator
            }
        }
    }
}

// Simple executor state stored in a static OnceLock
// Executor stores addresses (usize) so the queue is Send/Sync-safe in static storage.
struct Exec {
    queue: Mutex<std::collections::VecDeque<usize>>,
    cv: Condvar,
}

static EXECUTOR: OnceLock<std::sync::Arc<Exec>> = OnceLock::new();

fn init_executor() -> std::sync::Arc<Exec> {
    EXECUTOR
        .get_or_init(|| {
            let e = std::sync::Arc::new(Exec {
                queue: Mutex::new(std::collections::VecDeque::new()),
                cv: Condvar::new(),
            });
            // spawn background worker
            let w = e.clone();
            std::thread::spawn(move || {
                loop {
                    let mut guard = w.queue.lock().unwrap();
                    while guard.is_empty() {
                        guard = w.cv.wait(guard).unwrap();
                    }
                    if let Some(p_addr) = guard.pop_front() {
                        drop(guard);
                        let p = p_addr as *mut std::ffi::c_void;
                        unsafe {
                            let out_mem =
                                runtime_malloc(std::mem::size_of::<*mut c_void>()) as *mut u8;
                            if out_mem.is_null() {
                                continue;
                            }
                            let ready = promise_poll_into(p, out_mem as *mut std::ffi::c_void);
                            if ready == 0 {
                                // not ready; re-enqueue
                                let mut q2 = w.queue.lock().unwrap();
                                q2.push_back(p as usize);
                                w.cv.notify_one();
                            } else {
                                runtime_free(out_mem as *mut c_void);
                            }
                        }
                    }
                }
            });
            e
        })
        .clone()
}

// A trivial waker representation: in Phase-1 we'll create an object that
// can be cloned and invoked from native code. For now, we return a null
// pointer as a placeholder and make `waker_wake` a no-op.
#[unsafe(no_mangle)]
pub extern "C" fn waker_create_for_task(_task: *mut std::ffi::c_void) -> *mut std::ffi::c_void {
    if _task.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        // layout: [reserved 8][task_ptr 8]
        let size = std::mem::size_of::<*mut std::ffi::c_void>() * 2;
        let mem = runtime_malloc(size) as *mut u8;
        if mem.is_null() {
            return std::ptr::null_mut();
        }
        let first = mem as *mut *mut std::ffi::c_void;
        *first = std::ptr::null_mut();
        let second = mem.add(8) as *mut *mut std::ffi::c_void;
        *second = _task;
        mem as *mut std::ffi::c_void
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn waker_wake(_w: *mut std::ffi::c_void) {
    if _w.is_null() {
        return;
    }
    unsafe {
        let task_ptr_ptr = (_w as *mut u8).add(8) as *mut *mut std::ffi::c_void;
        let task = *task_ptr_ptr;
        if task.is_null() {
            return;
        }
        executor_enqueue(task);
    }
}

// Unit tests for runtime. Placing these here (instead of `tests/`) ensures
// they can call the runtime functions directly even when the crate is built
// as a `staticlib` for linking into AOT outputs.
#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::c_void;
    use std::sync::atomic::{AtomicBool, Ordering};

    static CALLED: AtomicBool = AtomicBool::new(false);

    fn my_dtor(p: *mut c_void) {
        let _ = p;
        CALLED.store(true, Ordering::SeqCst);
    }

    #[test]
    fn test_rc_dec_calls_destructor() {
        // Reset CALLED flag at start of test
        CALLED.store(false, Ordering::SeqCst);

        unsafe {
            // allocate two u64 words: header + dtor pointer via runtime_malloc
            let size = std::mem::size_of::<u64>() * 2;
            let mem = runtime_malloc(size) as *mut u8;
            assert!(!mem.is_null());

            // set header: type_tag=1, refcount=1, weak=1
            // weak=1 represents the object's own existence; when strong reaches 0,
            // rc_weak_dec will decrement weak from 1→0 and free the memory.
            let header_ptr = mem as *mut u64;
            let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
            let refcount: u64 = 1u64;
            let header_val: u64 = header_with_weak(type_tag | refcount, 1);
            *header_ptr = header_val;

            // store destructor pointer at second word
            let dtor_ptr = mem.add(std::mem::size_of::<u64>()) as *mut *mut c_void;
            *dtor_ptr = my_dtor as *mut c_void;

            // call rc_dec which should call the destructor and free
            rc_dec(mem as *mut c_void);

            // destructor should have been called
            assert!(CALLED.load(Ordering::SeqCst));
        }
    }

    #[test]
    fn validate_meta_block_good() {
        unsafe {
            let len: usize = 2;
            // allocate enough u64 words to hold meta0 + len*i32
            let words = 1 + ((len * 4 + 7) / 8);
            let mut buf: Vec<u64> = vec![0u64; words];
            buf[0] = (META_MAGIC << 32) | (len as u64 & 0xffffffffu64);
            let ptr_u8 = buf.as_mut_ptr() as *mut u8;
            let offsets_ptr = ptr_u8.add(8) as *mut i32;
            *offsets_ptr.add(0) = 16;
            *offsets_ptr.add(1) = 24;
            assert!(validate_meta_block(buf.as_mut_ptr(), 10));
        }
    }

    #[test]
    fn validate_meta_block_bad_magic() {
        unsafe {
            let len: usize = 1;
            let words = 1 + ((len * 4 + 7) / 8);
            let mut buf: Vec<u64> = vec![0u64; words];
            // wrong magic
            buf[0] = ((0x1234u64) << 32) | (len as u64 & 0xffffffffu64);
            let ptr = buf.as_mut_ptr();
            assert!(!validate_meta_block(ptr, 10));
        }
    }

    #[test]
    fn validate_meta_block_bad_offset_small() {
        unsafe {
            let len: usize = 1;
            let words = 1 + ((len * 4 + 7) / 8);
            let mut buf: Vec<u64> = vec![0u64; words];
            buf[0] = (META_MAGIC << 32) | (len as u64 & 0xffffffffu64);
            let ptr_u8 = buf.as_mut_ptr() as *mut u8;
            let offsets_ptr = ptr_u8.add(8) as *mut i32;
            // offset too small (points to metadata area)
            *offsets_ptr.add(0) = 8;
            assert!(!validate_meta_block(buf.as_mut_ptr(), 10));
        }
    }

    #[test]
    fn validate_meta_block_bad_unaligned() {
        unsafe {
            let len: usize = 1;
            let words = 1 + ((len * 4 + 7) / 8);
            let mut buf: Vec<u64> = vec![0u64; words];
            buf[0] = (META_MAGIC << 32) | (len as u64 & 0xffffffffu64);
            let ptr_u8 = buf.as_mut_ptr() as *mut u8;
            let offsets_ptr = ptr_u8.add(8) as *mut i32;
            // unaligned offset (not multiple of 8)
            *offsets_ptr.add(0) = 18;
            assert!(!validate_meta_block(buf.as_mut_ptr(), 10));
        }
    }
}
