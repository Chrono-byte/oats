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
/// The value of 128 provides reasonable depth for most use cases while preventing
/// stack exhaustion on typical systems.
pub(crate) const MAX_RECURSION_DEPTH: usize = 128;

// Module placeholders for incremental refactor.
pub mod array;
pub mod ffi;
pub mod header;
pub mod heap;
pub mod object;
pub mod process;
pub mod rc;
pub mod string;
pub mod utils;
// Collector implementation lives in a separate module.
pub mod collector;
#[allow(unused_imports)]
pub use crate::ffi::*;

// Re-export header helpers from crate root for other modules
pub use crate::header::*;

// Re-export heap allocation functions
pub use crate::heap::{runtime_free, runtime_malloc};

// Re-export array functions
pub use crate::array::{array_alloc, array_to_string};

// Re-export string functions
pub use crate::string::{heap_str_from_cstr, rc_dec_str};

// Re-export utils functions
pub use crate::utils::stringify_value_raw;

// Global runtime logging flag for ad-hoc diagnostics. Disabled by default.
// Set OATS_RUNTIME_LOG=1 in the environment to enable.
pub static RUNTIME_LOG: AtomicBool = AtomicBool::new(false);

// Global collector instance for background cycle collection.
pub static COLLECTOR: OnceLock<Arc<collector::Collector>> = OnceLock::new();

// Control whether the background collector emits diagnostic logging.
// Disabled by default; enable by setting the environment variable
// OATS_COLLECTOR_LOG=1 before running the generated binary.
pub static COLLECTOR_LOG: AtomicBool = AtomicBool::new(false);

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

pub fn init_collector() -> Arc<collector::Collector> {
    COLLECTOR
        .get_or_init(|| {
            let collector = collector::Collector::new();

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

pub fn add_root_candidate(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    let col = init_collector();
    col.push_root(p as usize);
}

/// Validate a metadata block pointer: must be plausibly aligned/non-null and
/// contain a reasonable length value (1..=max_len). Also check each offset is
/// an 8-byte aligned positive value within a pragmatic object-size bound.
///
/// Where to use:
///
/// Use prior to dereferencing class metadata blocks that were previously
/// stored by the code generator. If `validate_meta_block` returns `true`, the
/// caller may proceed with cautious reads of the metadata fields.
///
/// # Safety
///
/// `meta` must be a valid pointer to readable memory for at least the size of one `u64`,
/// plus `max_len * sizeof(i32)` bytes if the header is valid. This function ensures
/// that if it returns `true`, the full metadata block can be safely read.
pub unsafe fn validate_meta_block(meta: *const u64, max_len: usize) -> bool {
    // Reject null pointers
    if meta.is_null() {
        return false;
    }

    let addr = meta as usize;
    if !is_plausible_addr(addr) {
        return false;
    }

    // Read the header (magic + length)
    let meta0 = unsafe { *meta };

    // Validate magic number
    let magic = meta0 >> 32;
    if magic != META_MAGIC {
        return false;
    }

    // Validate length is in range
    let len = (meta0 & 0xffffffff) as usize;
    if len == 0 || len > max_len {
        return false;
    }

    // Validate each offset safely
    const MAX_OFFSET: usize = 1 << 20; // 1 MiB conservative bound
    const MIN_OFFSET: usize = 16; // header (8) + meta_slot (8)

    // Calculate the offsets array pointer and total required size
    let offsets_ptr = unsafe { meta.add(1) as *const i32 };

    // Check for arithmetic overflow in required size
    // let header_size = std::mem::size_of::<u64>();
    // let offsets_size = len.saturating_mul(std::mem::size_of::<i32>());

    // Ensure we can safely iterate over all offsets
    for i in 0..len {
        // Read each offset safely
        let off_i32 = unsafe { *offsets_ptr.add(i) };
        let off = off_i32 as usize;

        // Validate offset constraints
        if !(MIN_OFFSET..=MAX_OFFSET).contains(&off) || (off & 7) != 0 {
            return false;
        }
    }

    true
}

/// Check if an address is plausible (aligned, within reasonable bounds)
fn is_plausible_addr(addr: usize) -> bool {
    // Must be 8-byte aligned
    if addr & 7 != 0 {
        return false;
    }

    // Reject null address (already checked by caller, but defensive)
    if addr == 0 {
        return false;
    }

    // On most systems, reject addresses in the low 64 KiB (common sentinel region)
    // and reject addresses in the very high range (typically kernel space)
    #[cfg(target_pointer_width = "64")]
    {
        const LOW_BOUND: usize = 0x10000; // 64 KiB
        const HIGH_BOUND: usize = 0x0001_0000_0000_0000; // 2^48, typical user-space limit
        (LOW_BOUND..HIGH_BOUND).contains(&addr)
    }

    #[cfg(target_pointer_width = "32")]
    {
        const LOW_BOUND: usize = 0x10000; // 64 KiB
        addr >= LOW_BOUND
    }
}

// `tuple_to_string` implementation moved to `object.rs`; the C ABI symbol is
// exported from that module. Keep this file free of duplicate exports.
// `stringify_value_raw` is implemented in `utils.rs` and re-exported above.

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

// Re-export utils functions

// Re-export object/union functions
pub use crate::object::{
    union_box_f64, union_box_ptr, union_get_discriminant, union_unbox_f64, union_unbox_ptr,
};

// --- Union helpers ---

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
    let Ok(mut q) = exec.queue.lock() else { return };
    q.push_back(_promise as usize);
    exec.cv.notify_one();
}

#[unsafe(no_mangle)]
pub extern "C" fn executor_run() {
    // Drain the queue synchronously until empty.
    let exec = init_executor();
    loop {
        let Ok(mut q) = exec.queue.lock() else { break };
        if q.is_empty() {
            break;
        }
        let Some(p_addr) = q.pop_front() else { break };
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
                let Ok(mut q2) = exec.queue.lock() else {
                    continue;
                };
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
                    let Ok(mut guard) = w.queue.lock() else {
                        continue;
                    };
                    if guard.is_empty() {
                        let wait_result = w.cv.wait(guard);
                        guard = match wait_result {
                            Ok(g) => g,
                            Err(_) => continue,
                        };
                        // After waiting, check again if empty (shouldn't be)
                        if guard.is_empty() {
                            continue;
                        }
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
                                let Ok(mut q2) = w.queue.lock() else { continue };
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
        let len: usize = 1;
        let words = 1 + (len * 4).div_ceil(8);
        let mut buf: Vec<u64> = vec![0u64; words];
        // wrong magic
        buf[0] = ((0x1234u64) << 32) | (len as u64 & 0xffffffffu64);
        let ptr = buf.as_mut_ptr();
        unsafe {
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
