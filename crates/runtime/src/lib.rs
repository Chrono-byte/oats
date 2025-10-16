//! Runtime helpers for the Oats runtime.
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

use libc::c_void;
use std::mem;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, OnceLock};
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

// Module placeholders for incremental refactor.
mod array;
mod ffi;
mod header;
mod heap;
mod object;
mod promise;
mod rc;
mod string;
mod utils;

// Re-export header helpers from crate root for other modules
pub use crate::header::*;

// Re-export module functions
pub use crate::array::*;
pub use crate::heap::*;
pub use crate::object::*;
pub use crate::promise::*;
pub use crate::string::*;
pub use crate::utils::*;

// Ensure placeholder modules are referenced during incremental refactor.
// This function is called by tests or the binary runner to initialize
// runtime logging/limits and to anchor the placeholder modules so the
// compiler does not warn about unused imports while we migrate code.
#[allow(dead_code)]
pub(crate) fn init_runtime_placeholders() {
    // Call per-module no-op initializers so the modules are considered used.
    // Modules are now fully implemented, no placeholders needed.
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
// Allocation accounting helpers moved to `heap.rs`.
// See `crate::heap::check_and_reserve_allocation` and `crate::heap::release_allocation`.
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

// header_type_tag is provided by `header` module (migrated). Re-exported above.

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

// Increment the strong reference count for a string data pointer.
//
// This helper accepts a pointer to string data (the pointer returned to
// user-space code, which points at offset +16 from the header). It
// resolves the header and delegates to `rc_inc` which handles both
// string-data and object-base pointers.
//
// Handles both static and heap strings. For heap strings, the data pointer
// is at offset +16 from the object header.
//
// # Safety
// `data` must be a pointer previously returned by the runtime for a string
// (either a static literal or a heap-allocated string). Passing arbitrary
// or already-freed pointers is undefined behavior.
// --- Printing ---

// `tuple_to_string` implementation moved to `object.rs`; the C ABI symbol is
// exported from that module. Keep this file free of duplicate exports.

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
pub const ARRAY_HEADER_SIZE: usize = mem::size_of::<u64>() * 3;

// Minimum initial capacity for empty arrays
pub const MIN_ARRAY_CAPACITY: usize = 8;

// --- Atomic Reference Counting ---

// Re-export rc module functions
pub(crate) use crate::rc::get_object_base;
pub use crate::rc::rc_dec;
pub use crate::rc::rc_inc;
pub use crate::rc::rc_weak_dec;
pub use crate::rc::rc_weak_inc;
pub use crate::rc::rc_weak_upgrade;

// --- Union helpers ---

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
            let words = 1 + (len * 4).div_ceil(8);
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
            let words = 1 + (len * 4).div_ceil(8);
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
            let words = 1 + (len * 4).div_ceil(8);
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
            let words = 1 + (len * 4).div_ceil(8);
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
