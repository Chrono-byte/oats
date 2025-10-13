use std::sync::atomic::Ordering;

use crate::{CURRENT_HEAP_BYTES, MAX_ALLOC_BYTES, MAX_HEAP_BYTES, init_resource_limits};

/// Check if an allocation would exceed limits, and if not, reserve the space.
/// Returns true if allocation is allowed, false if it would exceed limits.
pub(crate) fn check_and_reserve_allocation(size: u64) -> bool {
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
pub(crate) fn release_allocation(size: u64) {
    CURRENT_HEAP_BYTES.fetch_sub(size, Ordering::Relaxed);
}

pub(crate) fn init_heap_placeholders() {
    // no-op shim for staged refactor
}
