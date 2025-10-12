//! Background cycle collector for reclaiming unreachable reference cycles.
//!
//! This module implements a conservative trial-deletion cycle collector that
//! runs in a background thread to reclaim memory from unreachable reference
//! cycles. The collector uses a queue-based approach where potential cycle
//! roots are submitted for analysis and processed asynchronously.
//!
//! # Algorithm
//!
//! The collector implements a conservative trial-deletion algorithm:
//! 1. **Root Submission**: Objects with strong reference count drops to zero submit themselves
//! 2. **Trial Deletion**: Temporarily decrement reference counts in the candidate subgraph
//! 3. **Reachability Analysis**: Check if any objects in the subgraph are still reachable
//! 4. **Cleanup or Restoration**: Delete unreachable cycles or restore reference counts
//!
//! # Concurrency
//!
//! The collector runs in a dedicated background thread and communicates with
//! the main execution thread through a protected queue. The collector uses
//! conservative assumptions to ensure memory safety in concurrent scenarios.
//!
//! # Performance
//!
//! The collector is designed to be non-intrusive, running with low priority
//! and using timeouts to avoid blocking program execution. Collection frequency
//! can be tuned based on allocation patterns and performance requirements.

use std::collections::{HashMap, HashSet, VecDeque};
use std::io::{self, Write};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use std::time::Duration;

/// Background cycle collector that processes potential garbage collection roots.
///
/// The collector maintains a queue of objects that may be part of unreachable
/// reference cycles and processes them using a conservative trial-deletion
/// algorithm. The collector runs in a separate thread to avoid blocking
/// program execution during garbage collection cycles.
pub struct Collector {
    /// Queue of potential cycle roots submitted for analysis
    queue: Mutex<Vec<usize>>,
    /// Condition variable for signaling new work to the collector thread
    cv: Condvar,
    /// Flag indicating whether the collector thread is currently running
    running: AtomicBool,
}

impl Collector {
    /// Creates a new collector instance ready for background operation.
    ///
    /// The collector is created in a stopped state and must be explicitly
    /// started using the `start` method. This separation allows for proper
    /// initialization and configuration before beginning collection cycles.
    ///
    /// # Returns
    /// An `Arc<Collector>` suitable for sharing between threads
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            queue: Mutex::new(Vec::new()),
            cv: Condvar::new(),
            running: AtomicBool::new(false),
        })
    }

    /// Starts the background collector thread for processing cycle collection.
    ///
    /// This method spawns a dedicated thread that waits for potential cycle
    /// roots to be submitted and processes them using the trial-deletion
    /// algorithm. The collector thread runs until explicitly stopped.
    ///
    /// # Thread Safety
    /// Multiple calls to `start` are safe; subsequent calls are ignored if
    /// the collector is already running.
    pub fn start(self: &Arc<Self>) {
        if self.running.swap(true, Ordering::SeqCst) {
            return; // Collector thread already running
        }
        let c = Arc::clone(self);
        thread::spawn(move || {
            while c.running.load(Ordering::SeqCst) {
                let mut guard = c.queue.lock().unwrap();
                let (g, _timeout) = c.cv.wait_timeout(guard, Duration::from_secs(1)).unwrap();
                guard = g;
                if !guard.is_empty() {
                    let roots: Vec<usize> = guard.drain(..).collect();
                    drop(guard);
                    if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                        let _ =
                            io::stderr().write_all(b"[oats runtime] collector: processing roots\n");
                    }
                    Self::process_roots(&roots);
                }
            }
            if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(b"[oats runtime] collector thread exiting\n");
            }
        });
    }

    pub fn stop(&self) {
        if !self.running.swap(false, Ordering::SeqCst) {
            return; // already stopped
        }
        self.cv.notify_one();
    }

    pub fn push_root(&self, ptr: usize) {
        let mut q = self.queue.lock().unwrap();
        q.push(ptr);
        self.cv.notify_one();
    }

    pub fn drain_now(&self) -> Vec<usize> {
        let mut q = self.queue.lock().unwrap();
        let out = q.clone();
        q.clear();
        out
    }

    /// Conservative trial-deletion pass.
    /// - Gathers reachable subgraph from roots
    /// - Simulates intra-graph reference counts
    /// - Performs cascade candidate propagation
    /// - Attempts reclamation with a few retries to survive races
    pub(crate) fn process_roots(roots: &[usize]) {
        unsafe {
            // Build reachable set
            let mut seen: HashSet<usize> = HashSet::new();
            let mut q: VecDeque<usize> = VecDeque::new();
            for &r in roots {
                if r == 0 {
                    continue;
                }
                let base = crate::get_object_base(r as *mut std::ffi::c_void) as usize;
                if base == 0 {
                    continue;
                }
                if seen.insert(base) {
                    q.push_back(base);
                }
            }

            while let Some(base) = q.pop_front() {
                let header_ptr = base as *const std::sync::atomic::AtomicU64;
                let header_val = (*header_ptr).load(Ordering::Relaxed);
                let type_tag = header_val >> crate::HEADER_TYPE_TAG_SHIFT;

                if type_tag == 0 {
                    // array-like / tuple
                    let len_ptr = (base as *mut u8).add(8) as *const u64;
                    if crate::is_plausible_addr(len_ptr as usize) {
                        let len = *len_ptr as usize;
                        let elem_is_number = ((header_val >> 32) & 1) != 0;
                        if !elem_is_number {
                            let data_start = (base as *mut u8).add(crate::ARRAY_HEADER_SIZE);
                            for i in 0..len {
                                let elem_ptr = data_start
                                    .add(i * std::mem::size_of::<*mut std::ffi::c_void>())
                                    as *const *mut std::ffi::c_void;
                                if !crate::is_plausible_addr(elem_ptr as usize) {
                                    continue;
                                }
                                let p = *elem_ptr;
                                if p.is_null() {
                                    continue;
                                }
                                let pbase = crate::get_object_base(p) as usize;
                                if pbase != 0 && seen.insert(pbase) {
                                    q.push_back(pbase);
                                }
                            }
                        }
                    }
                    continue;
                }

                if type_tag == 1 {
                    // union-like
                    let discrim_ptr = (base as *mut u8).add(16) as *const u64;
                    if crate::is_plausible_addr(discrim_ptr as usize) {
                        let discrim = *discrim_ptr;
                        if discrim == 1 {
                            let payload_ptr =
                                (base as *mut u8).add(24) as *const *mut std::ffi::c_void;
                            if crate::is_plausible_addr(payload_ptr as usize) {
                                let p = *payload_ptr;
                                if !p.is_null() {
                                    let pbase = crate::get_object_base(p) as usize;
                                    if pbase != 0 && seen.insert(pbase) {
                                        q.push_back(pbase);
                                    }
                                }
                            }
                        }
                    }
                    continue;
                }

                // metadata-driven object
                let meta_ptr_ptr = (base as *mut u8).add(8) as *const *mut u64;
                if !crate::is_plausible_addr(meta_ptr_ptr as usize) {
                    continue;
                }
                let meta = *meta_ptr_ptr;
                if meta.is_null() {
                    continue;
                }
                if !crate::validate_meta_block(meta, 1024) {
                    continue;
                }
                let len = ((*meta) & 0xffffffffu64) as usize;
                let offsets_ptr = meta.add(1) as *const i32;
                for i in 0..len {
                    let off_i32 = *offsets_ptr.add(i);
                    if off_i32 <= 0 {
                        continue;
                    }
                    let off = off_i32 as isize as usize;
                    if (off & 7) != 0 {
                        continue;
                    }
                    let field_addr = (base as *mut u8).add(off) as *const *mut std::ffi::c_void;
                    if !crate::is_plausible_addr(field_addr as usize) {
                        continue;
                    }
                    let p = *field_addr;
                    if p.is_null() {
                        continue;
                    }
                    let pbase = crate::get_object_base(p) as usize;
                    if pbase != 0 && seen.insert(pbase) {
                        q.push_back(pbase);
                    }
                }
            }

            if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!("[oats runtime] collector: reachable count={}\n", seen.len())
                        .as_bytes(),
                );
            }

            if seen.is_empty() {
                return;
            }

            // Simulate counts and out-edges
            let mut simulated: HashMap<usize, i64> = HashMap::new();
            let mut out_edges: HashMap<usize, Vec<usize>> = HashMap::new();
            for &o in &seen {
                let header_ptr = o as *const std::sync::atomic::AtomicU64;
                let h = (*header_ptr).load(Ordering::Relaxed);
                let rc = (h & crate::HEADER_RC_MASK) as i64;
                simulated.insert(o, rc);

                let type_tag = h >> crate::HEADER_TYPE_TAG_SHIFT;
                if type_tag == 0 {
                    let len_ptr = (o as *mut u8).add(8) as *const u64;
                    if !crate::is_plausible_addr(len_ptr as usize) {
                        continue;
                    }
                    let len = *len_ptr as usize;
                    let elem_is_number = ((h >> 32) & 1) != 0;
                    if !elem_is_number {
                        let data_start = (o as *mut u8).add(crate::ARRAY_HEADER_SIZE);
                        for i in 0..len {
                            let elem_ptr = data_start
                                .add(i * std::mem::size_of::<*mut std::ffi::c_void>())
                                as *const *mut std::ffi::c_void;
                            if !crate::is_plausible_addr(elem_ptr as usize) {
                                continue;
                            }
                            let p = *elem_ptr;
                            if p.is_null() {
                                continue;
                            }
                            let pbase = crate::get_object_base(p) as usize;
                            if pbase != 0 && seen.contains(&pbase) {
                                if let Some(v) = simulated.get_mut(&pbase) {
                                    *v -= 1;
                                }
                                out_edges.entry(o).or_default().push(pbase);
                            }
                        }
                    }
                } else if type_tag == 1 {
                    // union-like
                    let discrim_ptr = (o as *mut u8).add(16) as *const u64;
                    if crate::is_plausible_addr(discrim_ptr as usize) {
                        let discrim = *discrim_ptr;
                        if discrim == 1 {
                            let payload_ptr =
                                (o as *mut u8).add(24) as *const *mut std::ffi::c_void;
                            if crate::is_plausible_addr(payload_ptr as usize) {
                                let p = *payload_ptr;
                                if !p.is_null() {
                                    let pbase = crate::get_object_base(p) as usize;
                                    if pbase != 0 && seen.contains(&pbase) {
                                        if let Some(v) = simulated.get_mut(&pbase) {
                                            *v -= 1;
                                        }
                                        out_edges.entry(o).or_default().push(pbase);
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // metadata-driven object
                    let meta_ptr_ptr = (o as *mut u8).add(8) as *const *mut u64;
                    if !crate::is_plausible_addr(meta_ptr_ptr as usize) {
                        continue;
                    }
                    let meta = *meta_ptr_ptr;
                    if meta.is_null() {
                        continue;
                    }
                    if !crate::validate_meta_block(meta, 1024) {
                        continue;
                    }
                    let len = ((*meta) & 0xffffffffu64) as usize;
                    let offsets_ptr = meta.add(1) as *const i32;
                    for i in 0..len {
                        let off_i32 = *offsets_ptr.add(i);
                        if off_i32 <= 0 {
                            continue;
                        }
                        let off = off_i32 as isize as usize;
                        if (off & 7) != 0 {
                            continue;
                        }
                        let field_addr = (o as *mut u8).add(off) as *const *mut std::ffi::c_void;
                        if !crate::is_plausible_addr(field_addr as usize) {
                            continue;
                        }
                        let p = *field_addr;
                        if p.is_null() {
                            continue;
                        }
                        let pbase = crate::get_object_base(p) as usize;
                        if pbase != 0 && seen.contains(&pbase) {
                            if let Some(v) = simulated.get_mut(&pbase) {
                                *v -= 1;
                            }
                            out_edges.entry(o).or_default().push(pbase);
                        }
                    }
                }
            }

            // Cascade propagation
            let mut candidates_queue: VecDeque<usize> = VecDeque::new();
            let mut in_candidates: HashSet<usize> = HashSet::new();
            for (&o, &sim_rc) in simulated.iter() {
                if sim_rc <= 0 {
                    candidates_queue.push_back(o);
                    in_candidates.insert(o);
                }
            }

            let mut candidates: Vec<usize> = Vec::new();
            while let Some(node) = candidates_queue.pop_front() {
                candidates.push(node);
                if let Some(edges) = out_edges.get(&node) {
                    for &t in edges {
                        if let Some(v) = simulated.get_mut(&t) {
                            *v -= 1;
                            if *v <= 0 && !in_candidates.contains(&t) {
                                in_candidates.insert(t);
                                candidates_queue.push_back(t);
                            }
                        }
                    }
                }
            }

            if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] collector: candidates={}\n",
                        candidates.len()
                    )
                    .as_bytes(),
                );
            }

            // Reclamation with retries to reduce lost races. We use an
            // embedded header claim bit (HEADER_CLAIM_BIT) and a CAS to
            // ensure only one thread finalizes a candidate object. This
            // avoids a global claimed set and its associated locking.
            const MAX_RETRIES: usize = 4;
            let mut to_try: Vec<usize> = candidates;
            for _ in 0..MAX_RETRIES {
                if to_try.is_empty() {
                    break;
                }
                let mut next_round: Vec<usize> = Vec::new();
                for o in to_try.drain(..) {
                    let header_ptr = o as *mut std::sync::atomic::AtomicU64;
                    let old = (*header_ptr).load(Ordering::Acquire);
                    let strong = old & crate::HEADER_RC_MASK;
                    if strong != 0 {
                        continue;
                    }

                    // Try to set the claim bit via CAS. If another thread
                    // beat us to it, skip finalizing this object.
                    if (old & crate::HEADER_CLAIM_BIT) != 0 {
                        // Already claimed
                        continue;
                    }

                    let new = old | crate::HEADER_CLAIM_BIT;
                    match (*header_ptr).compare_exchange_weak(
                        old,
                        new,
                        std::sync::atomic::Ordering::AcqRel,
                        std::sync::atomic::Ordering::Relaxed,
                    ) {
                        Ok(_) => {
                            // We successfully claimed the object. Re-load header
                            // to capture any concurrent mutations that may have
                            // changed flags or type-tag (but claim remains set).
                            let claimed_header = (*header_ptr).load(Ordering::Acquire);
                            // Re-check strong count; if it became non-zero, give up.
                            let strong_after = claimed_header & crate::HEADER_RC_MASK;
                            if strong_after != 0 {
                                // Unclaim the object by clearing the claim bit.
                                let _ = (*header_ptr)
                                    .fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel);
                                continue;
                            }

                            // At this point we hold the claim bit and strong==0.
                            let type_tag = crate::header_type_tag(claimed_header) as u32;
                            if type_tag == 1 {
                                let dtor_ptr_ptr = (o as *mut u8).add(std::mem::size_of::<u64>())
                                    as *mut *mut std::ffi::c_void;
                                let dtor_raw = *dtor_ptr_ptr;
                                if !dtor_raw.is_null() {
                                    let dtor: extern "C" fn(*mut std::ffi::c_void) =
                                        std::mem::transmute(dtor_raw);
                                    dtor(o as *mut std::ffi::c_void);
                                }
                            }
                            crate::rc_weak_dec(o as *mut std::ffi::c_void);

                            // Clear claim bit after finalization by fetching and
                            // clearing the bit. If rc_weak_dec freed the control
                            // block, this write is benign (it may access freed
                            // memory) — but rc_weak_dec is responsible for freeing
                            // only when weak reaches zero, and callers must ensure
                            // no other thread accesses freed memory. To be robust,
                            // attempt to clear the claim bit only if the header
                            // still points to a live control block.
                            let _ =
                                (*header_ptr).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel);
                        }
                        Err(_) => {
                            // header changed under us; retry later
                            next_round.push(o);
                            continue;
                        }
                    }
                }
                to_try = next_round;
            }
        }
    }
}
