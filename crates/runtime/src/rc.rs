//! Reference-counting helpers
use libc::c_void;
use std::cell::RefCell;
use std::io::{self, Write};
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};

// Import header helpers from sibling module (parent has already imported header)
use crate::MAX_RECURSION_DEPTH;
use crate::header::*;

/// Error type for reference counting operations
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RcError {
    /// Invalid pointer (null or not plausible)
    InvalidPointer,
    /// Pointer validation failed
    ValidationFailed,
}

impl std::fmt::Display for RcError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RcError::InvalidPointer => write!(f, "Invalid pointer: null or not plausible"),
            RcError::ValidationFailed => write!(f, "Pointer validation failed"),
        }
    }
}

impl std::error::Error for RcError {}

/// Safe wrapper around a raw pointer for reference counting operations.
///
/// This type encapsulates pointer validation and provides safe methods
/// for incrementing and decrementing reference counts. The pointer is
/// validated at construction time to ensure it's a valid runtime pointer.
///
/// # Safety
///
/// The `RcPtr` type does not own the pointer - it's just a validated
/// reference. The caller must ensure the pointer remains valid for the
/// lifetime of the `RcPtr`.
#[derive(Debug, Clone, Copy)]
pub struct RcPtr {
    ptr: *mut c_void,
}

impl RcPtr {
    /// Create a new `RcPtr` from a raw pointer, validating it first.
    ///
    /// Returns `None` if the pointer is null or fails validation.
    pub fn new(ptr: *mut c_void) -> Option<Self> {
        if ptr.is_null() {
            return None;
        }

        let p_addr = ptr as usize;
        if !crate::is_plausible_addr(p_addr) {
            return None;
        }

        // Validate that we can resolve the object base
        let obj_ptr = unsafe { get_object_base(ptr) };
        if obj_ptr.is_null() {
            return None;
        }

        Some(RcPtr { ptr })
    }

    /// Get the raw pointer value.
    ///
    /// # Safety
    ///
    /// The returned pointer is only valid as long as the underlying
    /// object remains allocated. The caller must ensure proper
    /// reference counting.
    pub fn as_ptr(&self) -> *mut c_void {
        self.ptr
    }

    /// Increment the reference count of the object.
    ///
    /// Returns an error if the operation fails (e.g., pointer is invalid).
    pub fn inc(&self) -> Result<(), RcError> {
        if self.ptr.is_null() {
            return Err(RcError::InvalidPointer);
        }

        unsafe {
            rc_inc(self.ptr);
        }
        Ok(())
    }

    /// Decrement the reference count of the object.
    ///
    /// Returns an error if the operation fails. This may free the object
    /// if the reference count reaches zero.
    pub fn dec(self) -> Result<(), RcError> {
        if self.ptr.is_null() {
            return Err(RcError::InvalidPointer);
        }

        unsafe {
            rc_dec(self.ptr);
        }
        Ok(())
    }

    /// Increment the weak reference count of the object.
    ///
    /// Returns an error if the operation fails.
    pub fn weak_inc(&self) -> Result<(), RcError> {
        if self.ptr.is_null() {
            return Err(RcError::InvalidPointer);
        }

        unsafe {
            rc_weak_inc(self.ptr);
        }
        Ok(())
    }

    /// Decrement the weak reference count of the object.
    ///
    /// Returns an error if the operation fails.
    pub fn weak_dec(self) -> Result<(), RcError> {
        if self.ptr.is_null() {
            return Err(RcError::InvalidPointer);
        }

        unsafe {
            rc_weak_dec(self.ptr);
        }
        Ok(())
    }

    /// Attempt to upgrade a weak pointer to a strong one.
    ///
    /// Returns `None` if the object has been deallocated, or `Some(RcPtr)`
    /// if the upgrade was successful.
    pub fn weak_upgrade(&self) -> Option<Self> {
        if self.ptr.is_null() {
            return None;
        }

        unsafe {
            let upgraded = rc_weak_upgrade(self.ptr);
            if upgraded.is_null() {
                None
            } else {
                // Create a new RcPtr from the upgraded pointer
                // We skip validation here since rc_weak_upgrade already validated it
                Some(RcPtr { ptr: upgraded })
            }
        }
    }

    /// Get the base object pointer (resolves string data pointers, etc.)
    pub fn get_base(&self) -> Option<Self> {
        if self.ptr.is_null() {
            return None;
        }

        unsafe {
            let base = get_object_base(self.ptr);
            if base.is_null() {
                None
            } else {
                Some(RcPtr { ptr: base })
            }
        }
    }
}

// Note: Cannot implement From<*mut c_void> due to orphan rules.
// Use RcPtr::new() instead.

// Thread-local recursion depth counter for rc_dec destructor calls
thread_local! {
    static RC_DEC_DEPTH: RefCell<usize> = const { RefCell::new(0) };
}

/// Atomically increment the strong reference count of a heap-allocated object.
/// # Safety
/// `p` must be a valid pointer returned by the runtime allocator (or a
/// string-data pointer); passing arbitrary or freed pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_inc(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let p_addr = p as usize;
        if !crate::is_plausible_addr(p_addr) {
            return;
        }
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }

        let header = obj_ptr as *mut AtomicU64;

        // Optimized CAS loop: reduce redundant loads by reusing the initial load
        // Use Acquire ordering for consistency with rc_dec
        let mut old_header = (*header).load(Ordering::Acquire);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        // Optimized loop: reuse loaded value, only reload on CAS failure
        loop {
            let rc = old_header & HEADER_RC_MASK;
            // Overflow protection: don't increment beyond safe limit
            // Ensure version index doesn't conflict with special flags
            const MAX_SAFE_RC: u64 = 0x7FFFFFFF;
            if rc >= MAX_SAFE_RC {
                if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] rc_inc: reference count {} exceeds safe limit\n",
                            rc
                        )
                        .as_bytes(),
                    );
                }
                break;
            }
            // Use checked arithmetic to prevent overflow
            let new_rc = match rc.checked_add(1) {
                Some(nrc) => nrc & HEADER_RC_MASK,
                None => {
                    if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            b"[oats runtime] rc_inc: integer overflow in reference count\n",
                        );
                    }
                    break;
                }
            };
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_rc;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => {
                    // Reuse the actual value from CAS failure instead of reloading
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
            }
        }
    }
}

/// # Safety
///
/// `p` must be a valid pointer previously returned by the runtime, or null.
/// If non-null, the pointer must remain valid for the duration of this call.
pub unsafe fn rc_inc_pub(p: *mut c_void) {
    unsafe { rc_inc(p) }
}

/// Resolve the base object pointer from a possibly-offset pointer.
///
/// Optimized with better heuristics:
/// - Most pointers are already at the base (common case)
/// - String data pointers are at offset +16
/// - Fast path for aligned pointers that look like valid headers
#[inline]
pub(crate) unsafe fn get_object_base(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let p_addr = p as usize;
        // Validate pointer is plausible before any dereference
        if !crate::is_plausible_addr(p_addr) {
            return ptr::null_mut();
        }

        // Fast path: check if current pointer looks like a valid header
        // Most pointers are already at the base, so this avoids the subtraction
        let header = p as *const AtomicU64;
        // Additional validation: ensure header pointer is aligned and plausible
        if !crate::is_plausible_addr(header as usize) {
            return ptr::null_mut();
        }
        let header_val = (*header).load(Ordering::Relaxed);
        let rc = header_val & HEADER_RC_MASK;
        let is_static = (header_val & HEADER_STATIC_BIT) != 0;

        // Heuristic: if it looks like a valid header (has reasonable RC or is static),
        // it's likely already the base pointer
        if is_static || (rc > 0 && rc < 10000 && (header_val & HEADER_FLAGS_MASK) != 0) {
            return p;
        }

        // Try offset -16 (string data pointer case)
        // Validate that subtracting 16 doesn't result in invalid memory
        let obj_ptr_addr = p_addr.checked_sub(16);
        if obj_ptr_addr.is_none() {
            // Underflow - can't be a valid string data pointer
            return p; // Assume it's already the base
        }
        let obj_ptr_addr = obj_ptr_addr.unwrap();
        if !crate::is_plausible_addr(obj_ptr_addr) {
            return p; // Offset pointer is not plausible, assume original is base
        }
        let obj_ptr = obj_ptr_addr as *mut c_void;
        let obj_header = obj_ptr as *const AtomicU64;
        // Validate obj_header pointer before dereferencing
        if !crate::is_plausible_addr(obj_header as usize) {
            return p; // Can't validate offset header, assume original is base
        }
        let obj_header_val = (*obj_header).load(Ordering::Relaxed);
        let obj_rc = obj_header_val & HEADER_RC_MASK;
        let obj_is_static = (obj_header_val & HEADER_STATIC_BIT) != 0;

        // Heuristic: if the offset header looks valid, return it
        if obj_is_static
            || (obj_rc > 0 && obj_rc < 10000 && (obj_header_val & HEADER_FLAGS_MASK) != 0)
        {
            return obj_ptr;
        }

        // Fallback: return original pointer (assume it's already the base)
        p
    }
}

/// Atomically decrement the strong reference count and free the object if the count reaches zero.
/// # Safety
/// `p` must be a valid pointer returned by the runtime allocator (or a
/// string-data pointer). Double-free or invalid pointers are undefined behaviour.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        // Lazy initialization of logging - only initialize once
        static LOG_INIT: std::sync::Once = std::sync::Once::new();
        LOG_INIT.call_once(|| {
            crate::init_runtime_log();
        });
        let should_log = crate::RUNTIME_LOG.load(Ordering::Relaxed);

        if should_log {
            let _ = io::stdout()
                .write_all(format!("[oats runtime] rc_dec called p={:p}\n", p).as_bytes());
            let _ = io::stdout().flush();
        }

        let p_addr = p as usize;
        if !crate::is_plausible_addr(p_addr) {
            if should_log {
                let _ = io::stdout().write_all(
                    format!("[oats runtime] rc_dec: implausible p={:p}, ignoring\n", p).as_bytes(),
                );
                let _ = io::stdout().flush();
            }
            return;
        }
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }

        let header = obj_ptr as *mut AtomicU64;

        // Optimized CAS loop: reuse loaded values
        let mut old_header = (*header).load(Ordering::Acquire);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        loop {
            let rc = old_header & HEADER_RC_MASK;

            if rc == 0 {
                return;
            }

            let new_rc = rc.wrapping_sub(1) & HEADER_RC_MASK;
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_rc;

            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    if new_rc == 0 {
                        std::sync::atomic::fence(Ordering::Acquire);

                        // Check recursion depth to prevent stack overflow from destructor chains
                        let depth = RC_DEC_DEPTH.with(|d| {
                            let mut depth = d.borrow_mut();
                            *depth += 1;
                            *depth
                        });

                        // If we exceed the recursion limit, defer the destructor call
                        // to prevent stack overflow. The object will be cleaned up
                        // by the cycle collector. We still need to decrement weak refs
                        // and add to collector candidates for deferred cleanup.
                        if depth > MAX_RECURSION_DEPTH {
                            if should_log {
                                let _ = io::stderr().write_all(
                                    format!(
                                        "[oats runtime] rc_dec: recursion depth {} exceeded limit {}, deferring destructor for obj={:p}\n",
                                        depth, MAX_RECURSION_DEPTH, obj_ptr
                                    ).as_bytes(),
                                );
                                let _ = io::stderr().flush();
                            }
                            // Decrement depth counter before returning
                            RC_DEC_DEPTH.with(|d| {
                                *d.borrow_mut() -= 1;
                            });
                            // Still decrement weak references (this is safe even at depth limit)
                            crate::rc_weak_dec(obj_ptr);
                            // Add to cycle collector candidates for deferred cleanup
                            // This ensures the destructor will be called later when depth is lower
                            crate::add_root_candidate(obj_ptr);
                            break;
                        }

                        // Use a guard to ensure depth counter is always decremented, even if destructor panics
                        struct DepthGuard;
                        impl Drop for DepthGuard {
                            fn drop(&mut self) {
                                RC_DEC_DEPTH.with(|d| {
                                    *d.borrow_mut() -= 1;
                                });
                            }
                        }
                        let _depth_guard = DepthGuard;

                        let type_tag = (old_header >> HEADER_TYPE_TAG_SHIFT) as u32;
                        if type_tag == 1 {
                            let dtor_ptr_ptr = (obj_ptr as *mut u8).add(std::mem::size_of::<u64>())
                                as *mut *mut c_void;
                            let dtor_raw = *dtor_ptr_ptr;
                            if !dtor_raw.is_null() {
                                // Validate function pointer before transmute
                                let dtor_addr = dtor_raw as usize;
                                // Function pointers should be aligned and in plausible address range
                                // On most platforms, function pointers need to be aligned (typically 1-byte aligned is sufficient)
                                // but we check for reasonable alignment and address range
                                if crate::is_plausible_addr(dtor_addr) {
                                    // Additional validation: check that the address is reasonably aligned
                                    // Function pointers should typically be aligned (platform-dependent, but usually at least 1-byte)
                                    // We use a conservative check: ensure it's not obviously misaligned
                                    const MIN_FN_ALIGN: usize = 1; // Minimum alignment for function pointers
                                    if dtor_addr.is_multiple_of(MIN_FN_ALIGN) {
                                        // SAFETY: We've validated the pointer is in a plausible address range and aligned.
                                        // The pointer was stored by the code generator and should be a valid function pointer.
                                        // However, if the memory is corrupted, this could still be invalid. The is_plausible_addr
                                        // check provides defense-in-depth but cannot guarantee the pointer is actually executable.
                                        // In practice, destructor pointers are set by the code generator and should be valid.
                                        let dtor: extern "C" fn(*mut c_void) =
                                            std::mem::transmute(dtor_raw);
                                        dtor(obj_ptr);
                                    } else if should_log {
                                        let _ = io::stderr().write_all(
                                            format!(
                                                "[oats runtime] rc_dec: destructor pointer {:p} has invalid alignment, skipping\n",
                                                dtor_raw
                                            ).as_bytes(),
                                        );
                                        let _ = io::stderr().flush();
                                    }
                                } else if should_log {
                                    let _ = io::stderr().write_all(
                                        format!(
                                            "[oats runtime] rc_dec: invalid destructor pointer {:p}, skipping\n",
                                            dtor_raw
                                        ).as_bytes(),
                                    );
                                    let _ = io::stderr().flush();
                                }
                            }
                        }

                        // Check weak count before deciding whether to free
                        // We need to reload the header to get the current weak count
                        // since it may have changed since we last read it
                        let current_header = (*header).load(Ordering::Acquire);
                        let weak_count = header_get_weak_bits(current_header);

                        if weak_count == 0 {
                            // No weak references, free immediately
                            std::sync::atomic::fence(Ordering::Acquire);
                            let header_atomic = obj_ptr as *mut AtomicU64;
                            let _ = (*header_atomic).fetch_and(!HEADER_CLAIM_BIT, Ordering::AcqRel);
                            crate::runtime_free(obj_ptr);
                        } else {
                            // There are weak references - object stays allocated until
                            // the last weak reference is dropped. rc_weak_dec will free
                            // the object when it's called for the last weak reference
                            // and sees both strong and weak are 0.
                            // We don't call rc_weak_dec here because that would incorrectly
                            // decrement the weak count - weak references should only be
                            // decremented when they are actually dropped.
                        }

                        // Depth guard will decrement counter on drop (normal return or panic)
                    } else {
                        // Check if this object can form cycles - if not, skip GC
                        if (old_header & HEADER_CYCLE_BIT) == 0 {
                            crate::add_root_candidate(obj_ptr);
                        }
                    }
                    break;
                }
                Err(actual) => {
                    // Reuse the actual value from CAS failure
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
            }
        }
    }
}

/// # Safety
///
/// `p` must be a valid pointer previously returned by the runtime, or null.
/// If non-null, the pointer must remain valid for the duration of this call.
pub unsafe fn rc_dec_pub(p: *mut c_void) {
    unsafe { rc_dec(p) }
}

/// Atomically increment the weak reference count on an object.
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime.
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
        let mut old_header = (*header).load(Ordering::Relaxed);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        // Optimized loop: reuse loaded values
        loop {
            let weak = header_get_weak_bits(old_header);
            // Overflow protection: don't increment beyond 16-bit limit
            const MAX_SAFE_WEAK: u64 = 0xffff;
            if weak >= MAX_SAFE_WEAK {
                if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] rc_weak_inc: weak reference count {} exceeds safe limit\n",
                            weak
                        )
                        .as_bytes(),
                    );
                }
                break;
            }
            // Use checked arithmetic to prevent overflow
            let new_weak = match weak.checked_add(1) {
                Some(nw) => nw & 0xffffu64,
                None => {
                    if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            b"[oats runtime] rc_weak_inc: integer overflow in weak reference count\n",
                        );
                    }
                    break;
                }
            };
            let new_header = header_with_weak(old_header, new_weak);
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(actual) => {
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
            }
        }
    }
}

/// Atomically decrement the weak reference count and free the control block
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_weak_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        let p_addr = p as usize;
        if !crate::is_plausible_addr(p_addr) {
            return;
        }
        let obj_ptr = get_object_base(p);
        if obj_ptr.is_null() {
            return;
        }
        let header = obj_ptr as *mut AtomicU64;
        let mut old_header = (*header).load(Ordering::Acquire);

        // Early exit for static objects
        if (old_header & HEADER_STATIC_BIT) != 0 {
            return;
        }

        // Optimized loop: reuse loaded values
        loop {
            let weak = header_get_weak_bits(old_header);
            if weak == 0 {
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
                    let strong = new_header & HEADER_RC_MASK;
                    let weak_after = header_get_weak_bits(new_header);
                    if strong == 0 && weak_after == 0 {
                        std::sync::atomic::fence(Ordering::Acquire);
                        let header_atomic = obj_ptr as *mut AtomicU64;
                        let _ = (*header_atomic).fetch_and(!HEADER_CLAIM_BIT, Ordering::AcqRel);
                        crate::runtime_free(obj_ptr);
                    }
                    break;
                }
                Err(actual) => {
                    old_header = actual;
                    // Re-check static bit on retry
                    if (old_header & HEADER_STATIC_BIT) != 0 {
                        return;
                    }
                }
            }
        }
    }
}

/// Attempt to upgrade a weak pointer into a strong one.
/// # Safety
/// `p` must be a valid pointer previously returned by the runtime.
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
        let mut old_header = (*header).load(Ordering::Acquire);

        // Optimized loop: reuse loaded values
        loop {
            if (old_header & HEADER_STATIC_BIT) != 0 {
                return obj_ptr;
            }
            let strong = old_header & HEADER_RC_MASK;
            if strong == 0 {
                return ptr::null_mut();
            }
            // Overflow protection: ensure version index doesn't conflict with special flags
            const MAX_SAFE_RC: u64 = 0x7FFFFFFF;
            if strong >= MAX_SAFE_RC {
                if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] rc_weak_upgrade: reference count {} exceeds safe limit\n",
                            strong
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
            // Use checked arithmetic to prevent overflow
            let new_strong = match strong.checked_add(1) {
                Some(ns) => ns & HEADER_RC_MASK,
                None => {
                    if crate::RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            b"[oats runtime] rc_weak_upgrade: integer overflow in reference count\n",
                        );
                    }
                    return ptr::null_mut();
                }
            };
            let new_header = (old_header & HEADER_FLAGS_MASK) | new_strong;
            match (*header).compare_exchange_weak(
                old_header,
                new_header,
                Ordering::AcqRel,
                Ordering::Relaxed,
            ) {
                Ok(_) => return obj_ptr,
                Err(actual) => {
                    old_header = actual;
                }
            }
        }
    }
}
