//! Async/promise helpers for runtime

use libc::c_void;
use std::collections::VecDeque;
use std::sync::{Arc, Condvar, Mutex, OnceLock};

use crate::runtime_malloc;

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
        let size = match std::mem::size_of::<*mut std::ffi::c_void>().checked_add(8) {
            Some(size) => size,
            None => {
                return std::ptr::null_mut();
            }
        };
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
                let Ok(mut q2) = exec.queue.lock() else { continue };
                q2.push_back(p as usize);
                exec.cv.notify_one();
            } else {
                crate::runtime_free(out_mem as *mut c_void);
                // We don't free `p` because it may be the state object owned by the generator
            }
        }
    }
}

// Simple executor state stored in a static OnceLock
// Executor stores addresses (usize) so the queue is Send/Sync-safe in static storage.
struct Exec {
    queue: Mutex<VecDeque<usize>>,
    cv: Condvar,
}

static EXECUTOR: OnceLock<Arc<Exec>> = OnceLock::new();

fn init_executor() -> Arc<Exec> {
    EXECUTOR
        .get_or_init(|| {
            let e = Arc::new(Exec {
                queue: Mutex::new(VecDeque::new()),
                cv: Condvar::new(),
            });
            // spawn background worker
            let w = e.clone();
            std::thread::spawn(move || {
                loop {
                    let Ok(mut guard) = w.queue.lock() else { continue };
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
                                crate::runtime_free(out_mem as *mut c_void);
                            }
                        }
                    }
                }
            });
            e
        })
        .clone()
}
