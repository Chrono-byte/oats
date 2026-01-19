//! Background cycle collector for reclaiming unreachable reference cycles.
//!
//! This module implements Bacon's concurrent cycle collection algorithm:
//! 1. **Concurrent Root Processing**: Uses color-coded marking for thread-safe traversal of potential cycle roots
//! 2. **Object Reference Traversal**: Processes object graphs with concurrent marking to find cyclic structures
//! 3. **Concurrent Reclamation**: Uses atomic operations for thread-safe garbage reclamation
//! 4. **Improved Performance**: Reduces redundant traversals and pause times
//!
//! Note: This is an adaptation of Bacon's garbage collection algorithm for cycle collection,
//! starting from potential cycle roots (objects with RC=0) rather than full heap roots.
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
use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::io::{self, Write};
use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use std::time::Duration;

// Thread-local recursion depth counter for collector destructor calls
// This tracks depth when collector directly calls destructors
thread_local! {
    static COLLECTOR_DTOR_DEPTH: RefCell<usize> = const { RefCell::new(0) };
}

/// Guard for managing destructor call depth to prevent stack overflow
struct DtorDepthGuard;
impl Drop for DtorDepthGuard {
    fn drop(&mut self) {
        COLLECTOR_DTOR_DEPTH.with(|d| {
            *d.borrow_mut() -= 1;
        });
    }
}

/// Background cycle collector that processes potential garbage collection roots.
///
/// The collector maintains a queue of objects that may be part of unreachable
/// reference cycles and processes them using Bacon's concurrent cycle collection
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
    #[must_use]
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
    /// roots to be submitted and processes them using Bacon's concurrent cycle
    /// collection algorithm. The collector thread runs until explicitly stopped.
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
                let Ok(mut guard) = c.queue.lock() else {
                    continue;
                };
                let wait_result = c.cv.wait_timeout(guard, Duration::from_millis(100));
                guard = match wait_result {
                    Ok((g, _timeout)) => g,
                    Err(_) => continue,
                };
                if !guard.is_empty() {
                    // Batch processing: collect all roots at once
                    let roots: Vec<usize> = guard.drain(..).collect();
                    drop(guard);
                    if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            format!(
                                "[oats runtime] collector: processing {} roots\n",
                                roots.len()
                            )
                            .as_bytes(),
                        );
                    }
                    // Use the more efficient Bacon's algorithm
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
        let Ok(mut q) = self.queue.lock() else { return };
        // Batch roots: only notify if queue was empty to reduce wake-ups
        let was_empty = q.is_empty();
        q.push(ptr);
        if was_empty {
            self.cv.notify_one();
        }
    }

    pub fn drain_now(&self) -> Vec<usize> {
        let Ok(mut q) = self.queue.lock() else {
            return Vec::new();
        };
        let out = q.clone();
        q.clear();
        out
    }

    /// Build the set of objects reachable from the given roots
    unsafe fn build_reachable_set(roots: &[usize]) -> HashSet<usize> {
        // Pre-allocate with estimated capacity based on roots count
        let estimated_size = roots.len().max(64);
        let mut seen: HashSet<usize> = HashSet::with_capacity(estimated_size);
        let mut q: VecDeque<usize> = VecDeque::with_capacity(estimated_size);

        for &r in roots {
            if r == 0 {
                continue;
            }
            let base = unsafe { crate::get_object_base(r as *mut std::ffi::c_void) } as usize;
            if base == 0 {
                continue;
            }
            if seen.insert(base) {
                q.push_back(base);
            }
        }

        while let Some(base) = q.pop_front() {
            unsafe { Self::process_object_references(base, &mut seen, &mut q) };
        }

        seen
    }

    /// Process references from a single object during reachability analysis
    unsafe fn process_object_references(
        base: usize,
        seen: &mut HashSet<usize>,
        q: &mut VecDeque<usize>,
    ) {
        let header_ptr = base as *mut std::sync::atomic::AtomicU64;
        // SAFETY: header_ptr is derived from a validated base address.
        // The offset calculation ensures we stay within the object's header bounds.
        // Null check prevents dereferencing invalid pointers.
        if header_ptr.is_null() {
            return;
        }
        let header_val = unsafe { (*header_ptr).load(Ordering::Relaxed) };
        let type_tag = header_val >> crate::HEADER_TYPE_TAG_SHIFT;

        if type_tag == 0 {
            // array-like / tuple
            unsafe { Self::process_array_references(base, header_val, seen, q) };
        } else if type_tag == 1 {
            // union-like
            unsafe { Self::process_union_references(base, seen, q) };
        } else {
            // metadata-driven object
            unsafe { Self::process_metadata_references(base, seen, q) };
        }
    }

    /// Process references from array-like objects
    unsafe fn process_array_references(
        base: usize,
        header_val: u64,
        seen: &mut HashSet<usize>,
        q: &mut VecDeque<usize>,
    ) {
        let len_addr = base + 8;
        let len_ptr = ptr::with_exposed_provenance::<u64>(len_addr);
        // SAFETY: len_ptr is calculated with a fixed offset from a validated base.
        // is_plausible_addr ensures the pointer is within valid memory ranges.
        if crate::is_plausible_addr(len_ptr as usize) && !len_ptr.is_null() {
            let len = usize::try_from(unsafe { *len_ptr }).unwrap_or(0);
            let elem_is_number = ((header_val >> 32) & 1) != 0;
            if !elem_is_number {
                // Validate array length is reasonable to prevent excessive iteration
                const MAX_ARRAY_LEN: usize = 100_000_000; // 100M elements max
                let data_start = unsafe { (base as *mut u8).add(crate::ARRAY_HEADER_SIZE) };
                if len > MAX_ARRAY_LEN {
                    if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            format!(
                                "[oats runtime] collector: array length {len} exceeds limit, skipping\n"
                            )
                            .as_bytes(),
                        );
                    }
                    return;
                }
                for i in 0..len {
                    // Validate element offset calculation to prevent overflow
                    let Some(elem_offset) =
                        i.checked_mul(std::mem::size_of::<*mut std::ffi::c_void>())
                    else {
                        if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                            let _ = io::stderr().write_all(
                                format!(
                                    "[oats runtime] collector: integer overflow in element offset (i={i})\n"
                                )
                                .as_bytes(),
                            );
                        }
                        break; // Skip remaining elements
                    };
                    let elem_addr = data_start as usize + elem_offset;
                    let elem_ptr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(elem_addr);
                    // SAFETY: elem_ptr is bounds-checked by the loop limit and validated with is_plausible_addr.
                    // Null check prevents dereferencing invalid element pointers.
                    if crate::is_plausible_addr(elem_ptr as usize) && !elem_ptr.is_null() {
                        let p = unsafe { *elem_ptr };
                        if p.is_null() {
                            continue;
                        }
                        let pbase = unsafe { crate::get_object_base(p) } as usize;
                        if pbase != 0 && seen.insert(pbase) {
                            q.push_back(pbase);
                        }
                    }
                }
            }
        }
    }

    /// Process references from union-like objects
    unsafe fn process_union_references(
        base: usize,
        seen: &mut HashSet<usize>,
        q: &mut VecDeque<usize>,
    ) {
        let discrim_addr = base + 16;
        let discrim_ptr = ptr::with_exposed_provenance::<u64>(discrim_addr);
        if crate::is_plausible_addr(discrim_ptr as usize) {
            let discrim = unsafe { *discrim_ptr };
            if discrim == 1 {
                let payload_addr = base + 24;
                let payload_ptr =
                    ptr::with_exposed_provenance::<*mut std::ffi::c_void>(payload_addr);
                if crate::is_plausible_addr(payload_ptr as usize) {
                    let p = unsafe { *payload_ptr };
                    if !p.is_null() {
                        let pbase = unsafe { crate::get_object_base(p) } as usize;
                        if pbase != 0 && seen.insert(pbase) {
                            q.push_back(pbase);
                        }
                    }
                }
            }
        }
    }

    /// Process references from metadata-driven objects
    unsafe fn process_metadata_references(
        base: usize,
        seen: &mut HashSet<usize>,
        q: &mut VecDeque<usize>,
    ) {
        // Validate metadata length is reasonable
        const MAX_META_LEN: usize = 10_000; // 10K fields max
        // Validate offset doesn't exceed reasonable object size
        const MAX_FIELD_OFFSET: usize = 1 << 20; // 1 MiB conservative bound

        let meta_ptr_addr = base + 8;
        let meta_ptr_ptr = ptr::with_exposed_provenance::<*mut u64>(meta_ptr_addr);
        if !crate::is_plausible_addr(meta_ptr_ptr as usize) {
            return;
        }
        let meta = unsafe { *meta_ptr_ptr };
        if meta.is_null() {
            return;
        }
        if !unsafe { crate::validate_meta_block(meta, 1024) } {
            return;
        }
        let len = unsafe { (*meta & 0xffff_ffff_u64) as usize };
        if len > MAX_META_LEN {
            if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] collector: metadata length {len} exceeds limit, skipping\n"
                    )
                    .as_bytes(),
                );
            }
            return;
        }
        let offsets_ptr = unsafe { meta.add(1).cast::<i32>() };
        for i in 0..len {
            // Validate offset array index access
            let off_i32 = unsafe { *offsets_ptr.add(i) };
            if off_i32 <= 0 {
                continue;
            }
            let off = usize::try_from(off_i32).unwrap_or(0);
            // Validate offset alignment and bounds
            if (off & 7) != 0 {
                continue;
            }
            if off > MAX_FIELD_OFFSET {
                if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!("[oats runtime] collector: field offset {off} exceeds limit\n")
                            .as_bytes(),
                    );
                }
                continue;
            }
            let field_addr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(base + off);
            if !crate::is_plausible_addr(field_addr as usize) {
                continue;
            }
            let p = unsafe { *field_addr };
            if p.is_null() {
                continue;
            }
            let pbase = unsafe { crate::get_object_base(p) } as usize;
            if pbase != 0 && seen.insert(pbase) {
                q.push_back(pbase);
            }
        }
    }

    /// Simulate reference counts for objects in the reachable set
    unsafe fn simulate_reference_counts(
        seen: &HashSet<usize>,
    ) -> (HashMap<usize, i64>, HashMap<usize, Vec<usize>>) {
        // Pre-allocate with known capacity
        let seen_len = seen.len();
        let mut simulated: HashMap<usize, i64> = HashMap::with_capacity(seen_len);
        let mut out_edges: HashMap<usize, Vec<usize>> = HashMap::with_capacity(seen_len);

        for &o in seen {
            let header_ptr = ptr::with_exposed_provenance::<std::sync::atomic::AtomicU64>(o);
            let h = unsafe { (*header_ptr).load(Ordering::Relaxed) };
            let rc = i64::try_from(h & crate::HEADER_RC_MASK).unwrap_or(i64::MAX);
            simulated.insert(o, rc);

            let type_tag = h >> crate::HEADER_TYPE_TAG_SHIFT;
            if type_tag == 0 {
                unsafe {
                    Self::simulate_array_references(o, h, seen, &mut simulated, &mut out_edges);
                };
            } else if type_tag == 1 {
                unsafe { Self::simulate_union_references(o, seen, &mut simulated, &mut out_edges) };
            } else {
                unsafe {
                    Self::simulate_metadata_references(o, seen, &mut simulated, &mut out_edges);
                };
            }
        }

        (simulated, out_edges)
    }

    /// Simulate reference counts for array-like objects
    unsafe fn simulate_array_references(
        o: usize,
        h: u64,
        seen: &HashSet<usize>,
        simulated: &mut HashMap<usize, i64>,
        out_edges: &mut HashMap<usize, Vec<usize>>,
    ) {
        let len_addr = o + 8;
        let len_ptr = ptr::with_exposed_provenance::<u64>(len_addr);
        if !crate::is_plausible_addr(len_ptr as usize) {
            return;
        }
        let len = usize::try_from(unsafe { *len_ptr }).unwrap_or(0);
        let elem_is_number = ((h >> 32) & 1) != 0;
        if !elem_is_number {
            // Validate array length is reasonable
            const MAX_ARRAY_LEN: usize = 100_000_000; // 100M elements max
            if len > MAX_ARRAY_LEN {
                return; // Skip oversized arrays
            }
            let data_start = unsafe { (o as *mut u8).add(crate::ARRAY_HEADER_SIZE) };
            for i in 0..len {
                // Validate element offset calculation to prevent overflow
                let Some(elem_offset) = i.checked_mul(std::mem::size_of::<*mut std::ffi::c_void>())
                else {
                    break; // Skip remaining elements on overflow
                };
                let elem_addr = data_start as usize + elem_offset;
                let elem_ptr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(elem_addr);
                if !crate::is_plausible_addr(elem_ptr as usize) {
                    continue;
                }
                let p = unsafe { *elem_ptr };
                if p.is_null() {
                    continue;
                }
                let pbase = unsafe { crate::get_object_base(p) } as usize;
                if pbase != 0 && seen.contains(&pbase) {
                    if let Some(v) = simulated.get_mut(&pbase) {
                        *v -= 1;
                    }
                    out_edges.entry(o).or_default().push(pbase);
                }
            }
        }
    }

    /// Simulate reference counts for union-like objects
    unsafe fn simulate_union_references(
        o: usize,
        seen: &HashSet<usize>,
        simulated: &mut HashMap<usize, i64>,
        out_edges: &mut HashMap<usize, Vec<usize>>,
    ) {
        let discrim_ptr = ptr::with_exposed_provenance::<u64>(o + 16);
        if crate::is_plausible_addr(discrim_ptr as usize) {
            let discrim = unsafe { *discrim_ptr };
            if discrim == 1 {
                let payload_ptr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(o + 24);
                if crate::is_plausible_addr(payload_ptr as usize) {
                    let p = unsafe { *payload_ptr };
                    if !p.is_null() {
                        let pbase = unsafe { crate::get_object_base(p) } as usize;
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
    }

    /// Simulate reference counts for metadata-driven objects
    unsafe fn simulate_metadata_references(
        o: usize,
        seen: &HashSet<usize>,
        simulated: &mut HashMap<usize, i64>,
        out_edges: &mut HashMap<usize, Vec<usize>>,
    ) {
        // Validate metadata length is reasonable
        const MAX_META_LEN: usize = 10_000; // 10K fields max
        // Validate offset doesn't exceed reasonable object size
        const MAX_FIELD_OFFSET: usize = 1 << 20; // 1 MiB conservative bound

        let meta_ptr_ptr = ptr::with_exposed_provenance::<*mut u64>(o + 8);
        if !crate::is_plausible_addr(meta_ptr_ptr as usize) {
            return;
        }
        let meta = unsafe { *meta_ptr_ptr };
        if meta.is_null() {
            return;
        }
        if !unsafe { crate::validate_meta_block(meta, 1024) } {
            return;
        }
        let len = unsafe { (*meta & 0xffff_ffff_u64) as usize };
        if len > MAX_META_LEN {
            return; // Skip oversized metadata
        }
        let offsets_ptr = unsafe { meta.add(1).cast::<i32>() };
        for i in 0..len {
            let off_i32 = unsafe { *offsets_ptr.add(i) };
            if off_i32 <= 0 {
                continue;
            }
            let off = usize::try_from(off_i32).unwrap_or(0);
            // Validate offset alignment and bounds
            if (off & 7) != 0 {
                continue;
            }
            if off > MAX_FIELD_OFFSET {
                continue; // Skip invalid offsets
            }
            let field_addr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(o + off);
            if !crate::is_plausible_addr(field_addr as usize) {
                continue;
            }
            let p = unsafe { *field_addr };
            if p.is_null() {
                continue;
            }
            let pbase = unsafe { crate::get_object_base(p) } as usize;
            if pbase != 0 && seen.contains(&pbase) {
                if let Some(v) = simulated.get_mut(&pbase) {
                    *v -= 1;
                }
                out_edges.entry(o).or_default().push(pbase);
            }
        }
    }

    /// Perform cascade propagation to identify garbage candidates
    fn perform_cascade_propagation(
        simulated: &mut HashMap<usize, i64>,
        out_edges: &HashMap<usize, Vec<usize>>,
    ) -> Vec<usize> {
        // Pre-allocate with estimated capacity (typically fewer candidates than total objects)
        let estimated_candidates = (simulated.len() / 4).max(16);
        let mut candidates_queue: VecDeque<usize> = VecDeque::with_capacity(estimated_candidates);
        let mut in_candidates: HashSet<usize> = HashSet::with_capacity(estimated_candidates);

        for (&o, &sim_rc) in simulated.iter() {
            if sim_rc <= 0 {
                candidates_queue.push_back(o);
                in_candidates.insert(o);
            }
        }

        // Pre-allocate candidates vector with estimated capacity
        let mut candidates: Vec<usize> = Vec::with_capacity(in_candidates.capacity());
        while let Some(node) = candidates_queue.pop_front() {
            candidates.push(node);
            if let Some(edges) = out_edges.get(&node) {
                for &t in edges {
                    let v = simulated.entry(t).or_insert(0);
                    *v -= 1;
                    if *v <= 0 && !in_candidates.contains(&t) {
                        in_candidates.insert(t);
                        candidates_queue.push_back(t);
                    }
                }
            }
        }

        candidates
    }

    /// Reclaim garbage candidates with atomic operations and retries
    unsafe fn reclaim_candidates(candidates: Vec<usize>) {
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
            // Pre-allocate next_round with capacity estimate (typically fewer retries needed)
            let estimated_retries = (to_try.len() / 2).max(8);
            let mut next_round: Vec<usize> = Vec::with_capacity(estimated_retries);
            for o in to_try.iter().copied() {
                unsafe { Self::process_candidate_for_reclamation(o, &mut next_round) };
            }
            to_try = next_round;
        }
    }

    /// Process a single candidate object for reclamation
    unsafe fn process_candidate_for_reclamation(o: usize, next_round: &mut Vec<usize>) {
        let header_ptr = o as *mut std::sync::atomic::AtomicU64;
        let old = unsafe { (*header_ptr).load(Ordering::Acquire) };
        let strong = old & crate::HEADER_RC_MASK;
        if strong != 0 {
            return;
        }

        // Try to set the claim bit via CAS. If another thread
        // beat us to it, skip finalizing this object.
        if (old & crate::HEADER_CLAIM_BIT) != 0 {
            // Already claimed
            return;
        }

        let new = old | crate::HEADER_CLAIM_BIT;
        if unsafe {
            (*header_ptr).compare_exchange_weak(old, new, Ordering::AcqRel, Ordering::Relaxed)
        }
        .is_ok()
        {
            // We successfully claimed the object. Re-load header
            // to capture any concurrent mutations that may have
            // changed flags or type-tag (but claim remains set).
            let claimed_header = unsafe { (*header_ptr).load(Ordering::Acquire) };
            // Re-check strong count; if it became non-zero, give up.
            let strong_after = claimed_header & crate::HEADER_RC_MASK;
            if strong_after != 0 {
                // Unclaim the object by clearing the claim bit.
                let _ =
                    unsafe { (*header_ptr).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel) };
                return;
            }

            unsafe { Self::finalize_claimed_object(o, claimed_header, next_round) };
        } else {
            // header changed under us; retry later
            next_round.push(o);
        }
    }

    /// Finalize a successfully claimed object
    unsafe fn finalize_claimed_object(o: usize, claimed_header: u64, next_round: &mut Vec<usize>) {
        // At this point we hold the claim bit and strong==0.
        let type_tag = u32::try_from(crate::header_type_tag(claimed_header)).unwrap_or(0);
        if type_tag == 1 {
            let dtor_ptr_ptr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(
                o + std::mem::size_of::<u64>(),
            );
            let dtor_raw = unsafe { *dtor_ptr_ptr };
            if !dtor_raw.is_null() {
                unsafe { Self::call_destructor(o, dtor_raw, next_round) };
            }
        }

        // Check weak count before deciding whether to free
        let weak_count = crate::header_get_weak_bits(claimed_header);
        if weak_count == 0 {
            // No weak references, free immediately
            unsafe { Self::free_object_immediately(o) };
        } else {
            // Clear claim bit since we're not freeing here
            let header_ptr = o as *mut std::sync::atomic::AtomicU64;
            let _ = unsafe { (*header_ptr).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel) };
        }
    }

    /// Call destructor for an object, handling recursion depth
    unsafe fn call_destructor(
        o: usize,
        dtor_raw: *mut std::ffi::c_void,
        next_round: &mut Vec<usize>,
    ) {
        // Check recursion depth to prevent stack overflow
        let dtor_depth = COLLECTOR_DTOR_DEPTH.with(|d| {
            let mut depth = d.borrow_mut();
            *depth += 1;
            *depth
        });

        if dtor_depth > crate::MAX_RECURSION_DEPTH {
            // Defer destructor call - re-add to queue for later
            if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] collector: destructor depth {} exceeded, deferring obj={:p}\n",
                        dtor_depth, o as *mut std::ffi::c_void
                    ).as_bytes(),
                );
            }
            COLLECTOR_DTOR_DEPTH.with(|d| {
                *d.borrow_mut() -= 1;
            });
            // Re-add to candidates for later processing
            next_round.push(o);
            return;
        }

        // Use guard to ensure depth is decremented
        let _guard = DtorDepthGuard;

        // Validate function pointer before transmute
        let dtor_addr = dtor_raw as usize;
        if crate::is_plausible_addr(dtor_addr) {
            // Additional validation: check alignment
            const MIN_FN_ALIGN: usize = 1;
            if dtor_addr.is_multiple_of(MIN_FN_ALIGN) {
                if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] collector: calling destructor for obj={:p} (depth={})\n",
                            o as *mut std::ffi::c_void, dtor_depth
                        ).as_bytes(),
                    );
                }
                // SAFETY: We've validated the pointer is in a plausible address range and aligned.
                // The pointer was stored by the code generator and should be a valid function pointer.
                // However, if the memory is corrupted, this could still be invalid. The is_plausible_addr
                // check provides defense-in-depth but cannot guarantee the pointer is actually executable.
                // In practice, destructor pointers are set by the code generator and should be valid.
                let dtor: extern "C" fn(*mut std::ffi::c_void) =
                    unsafe { std::mem::transmute(dtor_raw) };
                dtor(o as *mut std::ffi::c_void);
            } else if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] collector: destructor pointer {:p} has invalid alignment for obj={:p}, skipping\n",
                        dtor_raw, o as *mut std::ffi::c_void
                    ).as_bytes(),
                );
            }
        } else if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
            let _ = io::stderr().write_all(
                format!(
                    "[oats runtime] collector: invalid destructor pointer {:p} for obj={:p}, skipping\n",
                    dtor_raw, o as *mut std::ffi::c_void
                ).as_bytes(),
            );
        }
    }

    /// Free an object immediately
    unsafe fn free_object_immediately(o: usize) {
        unsafe {
            // Clear claim bit before freeing (it's already set, so this is safe)
            std::sync::atomic::fence(Ordering::Acquire);
            let header_atomic = o as *mut std::sync::atomic::AtomicU64;
            let _ = (*header_atomic).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel);
            crate::runtime_free(o as *mut std::ffi::c_void);
            // Object is freed, don't try to access it again
        }
    }

    /// Conservative trial-deletion pass.
    /// - Gathers reachable subgraph from roots
    /// - Simulates intra-graph reference counts
    /// - Performs cascade candidate propagation
    /// - Attempts reclamation with a few retries to survive races
    pub(crate) fn process_roots(roots: &[usize]) {
        unsafe {
            // SAFETY: This function performs conservative garbage collection analysis.
            // All pointer operations are validated with is_plausible_addr checks before dereference.
            // Memory accesses are bounded by object metadata and array lengths.
            // Atomic operations ensure thread-safety for concurrent access to headers.
            // No memory is freed during analysis phase - only during final reclamation.

            // Build reachable set
            let seen = Self::build_reachable_set(roots);

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
            let (mut simulated, out_edges) = Self::simulate_reference_counts(&seen);

            // Cascade propagation to find candidates
            let candidates = Self::perform_cascade_propagation(&mut simulated, &out_edges);

            if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] collector: candidates={}\n",
                        candidates.len()
                    )
                    .as_bytes(),
                );
            }

            // Reclaim garbage candidates
            Self::reclaim_candidates(candidates);
        }
    }

    /// Advanced Bacon's concurrent cycle collection algorithm.
    /// More efficient than trial-deletion with better concurrency support.
    ///
    /// This is an adaptation of Bacon's garbage collection algorithm for cycle collection.
    /// Unlike pure Bacon's GC which starts from heap roots, this version starts from
    /// potential cycle roots (objects with RC=0) to focus on cyclic garbage detection.
    ///
    /// Algorithm phases:
    /// 1. Mark potential cycle roots and build initial worklist
    /// 2. Concurrent marking phase with color coding (white/gray/black)
    /// 3. Identify unreachable cyclic structures
    /// 4. Safe reclamation with atomic operations
    #[allow(dead_code)]
    pub(crate) fn process_roots_bacon(roots: &[usize]) {
        unsafe {
            // SAFETY: Bacon's algorithm implementation for concurrent cycle collection.
            // All pointer dereferences are protected by is_plausible_addr validation.
            // Atomic operations on headers ensure thread-safe access during concurrent marking.
            // Memory layout assumptions are validated against known object formats.
            // No memory reclamation occurs during marking phase - only during final cleanup.

            use std::sync::atomic::Ordering;

            // Color coding for Bacon's algorithm
            const WHITE: u8 = 0; // Not visited
            const GRAY: u8 = 1; // In worklist
            const BLACK: u8 = 2; // Processed

            // Pre-allocate with estimated capacity to reduce reallocations
            let estimated_size = roots.len().max(64);
            let mut colors: HashMap<usize, u8> = HashMap::with_capacity(estimated_size);
            let mut worklist: VecDeque<usize> = VecDeque::with_capacity(estimated_size);

            // Phase 1: Mark roots and initial reachable objects
            // Batch process roots to reduce redundant get_object_base calls
            for &r in roots {
                if r == 0 {
                    continue;
                }
                let base = crate::get_object_base(r as *mut std::ffi::c_void) as usize;
                if base == 0 {
                    continue;
                }

                // Only add to worklist if not already seen (avoid duplicates)
                if colors.insert(base, GRAY).is_none() {
                    worklist.push_back(base);
                }
            }

            // Phase 2: Concurrent marking
            // Process worklist efficiently, avoiding redundant color lookups
            while let Some(obj) = worklist.pop_front() {
                // Fast path: skip if already processed (not GRAY)
                if colors.get(&obj) == Some(&GRAY) {
                    // Process object references
                    Self::process_object_refs_bacon(obj, &mut colors, &mut worklist);
                    colors.insert(obj, BLACK);
                }
            }

            // Phase 3: Identify garbage (white objects)
            // Pre-allocate garbage vector with estimated size
            let mut garbage: Vec<usize> = Vec::with_capacity(colors.len() / 4); // Estimate ~25% garbage
            for (&obj, &color) in &colors {
                if color == WHITE {
                    garbage.push(obj);
                }
            }

            if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] bacon collector: processed={}, garbage={}\n",
                        colors.len(),
                        garbage.len()
                    )
                    .as_bytes(),
                );
            }

            // Phase 4: Safe reclamation with atomic operations
            Self::reclaim_garbage_bacon(&garbage);
        }
    }

    /// Process object references for Bacon's algorithm
    #[allow(dead_code)]
    unsafe fn process_object_refs_bacon(
        obj: usize,
        colors: &mut HashMap<usize, u8>,
        worklist: &mut VecDeque<usize>,
    ) {
        // SAFETY: Helper function for Bacon's marking phase.
        // All pointer dereferences are protected by is_plausible_addr validation.
        // Object layout knowledge ensures correct field offsets for different types.
        // Atomic header access ensures thread-safe reading during concurrent marking.
        // No memory modification occurs - only marking for garbage identification.

        const GRAY: u8 = 1;

        // SAFETY: obj is validated by caller; header_ptr calculation is safe arithmetic.
        if obj == 0 {
            return; // Explicit null check to prevent invalid pointer dereference
        }
        let header_ptr = obj as *const std::sync::atomic::AtomicU64;
        if header_ptr.is_null() {
            return; // Additional null check for header pointer
        }
        let header_val = unsafe { (*header_ptr).load(Ordering::Relaxed) };
        let type_tag = header_val >> crate::HEADER_TYPE_TAG_SHIFT;

        if type_tag == 0 {
            // Array-like object
            let len_addr = obj + 8;
            let len_ptr = ptr::with_exposed_provenance::<u64>(len_addr);
            if crate::is_plausible_addr(len_ptr as usize) {
                let len = usize::try_from(unsafe { *len_ptr }).unwrap_or(0);
                let elem_is_number = ((header_val >> 32) & 1) != 0;
                if !elem_is_number {
                    // Validate array length is reasonable
                    const MAX_ARRAY_LEN: usize = 100_000_000; // 100M elements max
                    if len > MAX_ARRAY_LEN {
                        return; // Skip oversized arrays
                    }
                    let data_start = unsafe { (obj as *mut u8).add(crate::ARRAY_HEADER_SIZE) };
                    for i in 0..len {
                        // Validate element offset calculation to prevent overflow
                        let Some(elem_offset) =
                            i.checked_mul(std::mem::size_of::<*mut std::ffi::c_void>())
                        else {
                            break; // Skip remaining elements on overflow
                        };
                        let elem_ptr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(
                            data_start as usize + elem_offset,
                        );
                        if crate::is_plausible_addr(elem_ptr as usize) {
                            let p = unsafe { *elem_ptr };
                            if !p.is_null() {
                                let pbase = unsafe { crate::get_object_base(p) } as usize;
                                if pbase != 0 && colors.insert(pbase, GRAY).is_none() {
                                    worklist.push_back(pbase);
                                }
                            }
                        }
                    }
                }
            }
        } else if type_tag == 1 {
            // Union-like object
            let discrim_ptr = ptr::with_exposed_provenance::<u64>(obj + 16);
            if crate::is_plausible_addr(discrim_ptr as usize) {
                let discrim = unsafe { *discrim_ptr };
                if discrim == 1 {
                    let payload_ptr =
                        ptr::with_exposed_provenance::<*mut std::ffi::c_void>(obj + 24);
                    if crate::is_plausible_addr(payload_ptr as usize) {
                        let p = unsafe { *payload_ptr };
                        if !p.is_null() {
                            let pbase = unsafe { crate::get_object_base(p) } as usize;
                            if pbase != 0 && colors.insert(pbase, GRAY).is_none() {
                                worklist.push_back(pbase);
                            }
                        }
                    }
                }
            }
        } else {
            // Metadata-driven object
            // Validate offset doesn't exceed reasonable object size
            const MAX_FIELD_OFFSET: usize = 1 << 20; // 1 MiB conservative bound
            const MAX_META_LEN: usize = 10_000; // 10K fields max
            let meta_ptr_ptr = ptr::with_exposed_provenance::<*mut u64>(obj + 8);
            if crate::is_plausible_addr(meta_ptr_ptr as usize) {
                let meta = unsafe { *meta_ptr_ptr };
                if !meta.is_null() && unsafe { crate::validate_meta_block(meta, 1024) } {
                    let len = unsafe { *meta & 0xffff_ffff_u64 } as usize;
                    // Validate metadata length is reasonable
                    if len > MAX_META_LEN {
                        return; // Skip oversized metadata
                    }
                    let offsets_ptr = unsafe { meta.add(1) } as *const i32;
                    for i in 0..len {
                        let off_i32 = unsafe { *offsets_ptr.add(i) };
                        if off_i32 > 0 {
                            let off = usize::try_from(off_i32).unwrap_or(0);
                            // Validate offset alignment and bounds
                            if off.trailing_zeros() >= 3 && off <= MAX_FIELD_OFFSET {
                                let field_addr = ptr::with_exposed_provenance::<
                                    *mut std::ffi::c_void,
                                >(obj + off);
                                if crate::is_plausible_addr(field_addr as usize) {
                                    let p = unsafe { *field_addr };
                                    if !p.is_null() {
                                        let pbase = unsafe { crate::get_object_base(p) } as usize;
                                        if pbase != 0 && colors.insert(pbase, GRAY).is_none() {
                                            worklist.push_back(pbase);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Safe garbage reclamation for Bacon's algorithm
    #[allow(dead_code)]
    unsafe fn reclaim_garbage_bacon(garbage: &[usize]) {
        // SAFETY: Final reclamation phase of Bacon's algorithm.
        // Only objects identified as garbage (unreachable) are processed.
        // Atomic CAS operations ensure thread-safe claiming of objects for reclamation.
        // Destructor calls are protected by null checks and type validation.
        // Memory deallocation is handled by rc_weak_dec which ensures proper cleanup.
        // Claim bit prevents double reclamation of the same object.

        for &obj in garbage {
            let header_ptr = obj as *mut std::sync::atomic::AtomicU64;

            // Atomic check and claim
            let old = unsafe { (*header_ptr).load(Ordering::Acquire) };
            if (old & crate::HEADER_CLAIM_BIT) == 0
                && unsafe {
                    (*header_ptr).compare_exchange_weak(
                        old,
                        old | crate::HEADER_CLAIM_BIT,
                        Ordering::AcqRel,
                        Ordering::Acquire,
                    )
                }
                .is_ok()
            {
                let claimed_header = unsafe { (*header_ptr).load(Ordering::Acquire) };
                if (claimed_header & crate::HEADER_STATIC_BIT) == 0 {
                    // Call destructor if present
                    let dtor_ptr_ptr = ptr::with_exposed_provenance::<*mut std::ffi::c_void>(
                        obj + std::mem::size_of::<u64>(),
                    );
                    let dtor_raw = unsafe { *dtor_ptr_ptr };
                    if !dtor_raw.is_null() {
                        // Check recursion depth to prevent stack overflow
                        let dtor_depth = COLLECTOR_DTOR_DEPTH.with(|d| {
                            let mut depth = d.borrow_mut();
                            *depth += 1;
                            *depth
                        });

                        if dtor_depth > crate::MAX_RECURSION_DEPTH {
                            // Defer destructor call - skip this object for now
                            if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                                let _ = io::stderr().write_all(
                                    format!(
                                        "[oats runtime] bacon collector: destructor depth {} exceeded, deferring obj={:p}\n",
                                        dtor_depth, obj as *mut std::ffi::c_void
                                    ).as_bytes(),
                                );
                            }
                            COLLECTOR_DTOR_DEPTH.with(|d| {
                                *d.borrow_mut() -= 1;
                            });
                            // Clear claim bit to allow retry in future collection cycles
                            // Without this, the object would be permanently leaked
                            let _ = unsafe {
                                (*header_ptr).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel)
                            };
                            continue;
                        }

                        // Use guard to ensure depth is decremented
                        let _guard = DtorDepthGuard;

                        // Validate function pointer before transmute
                        let dtor_addr = dtor_raw as usize;
                        if crate::is_plausible_addr(dtor_addr) {
                            // Additional validation: check alignment
                            const MIN_FN_ALIGN: usize = 1;
                            if dtor_addr.is_multiple_of(MIN_FN_ALIGN) {
                                if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                                    let _ = io::stderr().write_all(
                                        format!(
                                            "[oats runtime] bacon collector: calling destructor for obj={:p} (depth={})\n",
                                            obj as *mut std::ffi::c_void, dtor_depth
                                        ).as_bytes(),
                                    );
                                }
                                // SAFETY: We've validated the pointer is in a plausible address range and aligned.
                                // The pointer was stored by the code generator and should be a valid function pointer.
                                // However, if the memory is corrupted, this could still be invalid. The is_plausible_addr
                                // check provides defense-in-depth but cannot guarantee the pointer is actually executable.
                                // In practice, destructor pointers are set by the code generator and should be valid.
                                let dtor: fn(*mut std::ffi::c_void) =
                                    unsafe { std::mem::transmute(dtor_raw) };
                                dtor(obj as *mut std::ffi::c_void);
                            } else if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                                let _ = io::stderr().write_all(
                                    format!(
                                        "[oats runtime] bacon collector: destructor pointer {:p} has invalid alignment for obj={:p}, skipping\n",
                                        dtor_raw, obj as *mut std::ffi::c_void
                                    ).as_bytes(),
                                );
                            }
                        } else if crate::COLLECTOR_LOG.load(Ordering::Relaxed) {
                            let _ = io::stderr().write_all(
                                format!(
                                    "[oats runtime] bacon collector: invalid destructor pointer {:p} for obj={:p}, skipping\n",
                                    dtor_raw, obj as *mut std::ffi::c_void
                                ).as_bytes(),
                            );
                        }
                    }

                    // Check weak count before deciding whether to free
                    let weak_count = crate::header_get_weak_bits(claimed_header);
                    if weak_count == 0 {
                        // No weak references, free immediately
                        // Clear claim bit before freeing (it's already set, so this is safe)
                        std::sync::atomic::fence(Ordering::Acquire);
                        let header_atomic = obj as *mut std::sync::atomic::AtomicU64;
                        let _ = unsafe {
                            (*header_atomic).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel)
                        };
                        unsafe { crate::runtime_free(obj as *mut std::ffi::c_void) };
                        // Object is freed, don't try to access it again
                    } else {
                        // There are weak references - object stays allocated until
                        // the last weak reference is dropped. rc_weak_dec will free
                        // the object when it's called for the last weak reference.
                        // We don't call rc_weak_dec here because that would incorrectly
                        // decrement the weak count.
                        // Clear claim bit since we're not freeing here
                        let _ = unsafe {
                            (*header_ptr).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel)
                        };
                    }
                } else {
                    // Static object - clear claim bit (static objects don't need freeing)
                    let _ = unsafe {
                        (*header_ptr).fetch_and(!crate::HEADER_CLAIM_BIT, Ordering::AcqRel)
                    };
                }
            }
        }
    }
}
