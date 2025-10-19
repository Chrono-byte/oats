use std::sync::atomic::Ordering;

use crate::{
    CURRENT_HEAP_BYTES, MAX_ALLOC_BYTES, MAX_HEAP_BYTES, RUNTIME_LOG, init_resource_limits,
};

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

use libc::{c_void, size_t};
use std::alloc::Layout;
use std::io::{self, Write};

#[unsafe(no_mangle)]
pub fn runtime_malloc(size: size_t) -> *mut c_void {
    unsafe {
        if size == 0 {
            return std::ptr::null_mut();
        }
        let total = size.checked_add(std::mem::size_of::<u64>()).unwrap_or(0);
        if total == 0 {
            return std::ptr::null_mut();
        }

        if !check_and_reserve_allocation(total as u64) {
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

        let layout = match Layout::from_size_align(total, 8) {
            Ok(layout) => layout,
            Err(_) => return std::ptr::null_mut(),
        };
        let base = std::alloc::alloc(layout);
        if base.is_null() {
            release_allocation(total as u64);
            return std::ptr::null_mut();
        }
        let size_ptr = base as *mut u64;
        *size_ptr = total as u64;
        base.add(std::mem::size_of::<u64>()) as *mut c_void
    }
}

/// # Safety
/// `p` must be a pointer previously returned by `runtime_malloc` or null.
/// Passing arbitrary or already-freed pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe fn runtime_free(p: *mut c_void) {
    unsafe {
        if p.is_null() {
            return;
        }
        // SAFETY: p is checked for null and plausible address before accessing memory
        if !crate::is_plausible_addr(p as usize) {
            return;
        }
        let base = (p as *mut u8).sub(std::mem::size_of::<u64>());
        let size_ptr = base as *mut u64;
        let total = *size_ptr as usize;
        if total == 0 {
            return;
        }

        release_allocation(total as u64);

        let layout = match Layout::from_size_align(total, 8) {
            Ok(layout) => layout,
            Err(_) => return, // Skip deallocation if layout is invalid
        };
        std::alloc::dealloc(base, layout);
    }
}
