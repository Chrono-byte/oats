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

use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::io::{self, Write};
use std::mem;
use std::process;
use std::ptr;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, Mutex, OnceLock};
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
mod rc;
mod string;
mod utils;
#[allow(unused_imports)]
pub use crate::ffi::*;

// Re-export header helpers from crate root for other modules
pub use crate::header::*;

// Ensure placeholder modules are referenced during incremental refactor.
// This function is called by tests or the binary runner to initialize
// runtime logging/limits and to anchor the placeholder modules so the
// compiler does not warn about unused imports while we migrate code.
#[allow(dead_code)]
pub(crate) fn init_runtime_placeholders() {
    // Call per-module no-op initializers so the modules are considered used.
    crate::array::init_array_placeholders();
    crate::heap::init_heap_placeholders();
    crate::object::init_object_placeholders();
    crate::rc::init_rc_placeholders();
    crate::string::init_string_placeholders();
    crate::utils::init_utils_placeholders();
    // Ensure ffi is referenced
    crate::ffi::ffi_init();
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

// Re-export rc module functions
pub(crate) use crate::rc::get_object_base;
pub use crate::rc::rc_dec;
pub use crate::rc::rc_inc;
pub use crate::rc::rc_weak_dec;
pub use crate::rc::rc_weak_inc;
pub use crate::rc::rc_weak_upgrade;

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
