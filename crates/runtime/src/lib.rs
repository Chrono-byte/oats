// Runtime helpers for the OATS AOT runtime.
//
// This file provides a small set of C-callable helpers used by the
// code generator. It intentionally keeps implementations small and
// conservative. The object header layout used here is a u64 where the
// high 32 bits contain type/flags and the low 32 bits contain a
// refcount. All refcount updates are done via atomic CAS on the full
// u64 word.
//
// OBJECT HEADER FORMAT (64 bits):
// - Bits 0-31:  Reference count (atomic)
// - Bit 32:     Static/immortal flag (1 = static, 0 = heap-allocated)
// - Bits 33-63: Type tag and other flags (reserved for future use)
//
// Static objects (string literals, etc.) have bit 32 set and RC operations are no-ops.

use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::io::{self, Write};
use std::mem;
use std::process;
use std::ptr;
use std::sync::atomic::{AtomicPtr, AtomicU64, Ordering};

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
// Flags mask includes everything above the low 32-bit refcount
const HEADER_FLAGS_MASK: u64 = 0xffffffff00000000u64;

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

// Allocate a heap string with RC header (refcount initialized to 1).
// Layout: [u64 header][u64 length][char data + null terminator]
// Returns pointer to the header (caller should offset by 16 bytes to get char* for C compatibility).
#[unsafe(no_mangle)]
pub extern "C" fn heap_str_alloc(str_len: size_t) -> *mut c_void {
    unsafe {
        // Total size: 8 (header) + 8 (length) + str_len + 1 (null terminator)
        let total_size = 16 + str_len + 1;
        let p = libc::malloc(total_size);
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

// Copy a C string into a heap-allocated string with RC header.
// Returns pointer to the data section (offset +16 from header) for C compatibility.
//
// # Safety
// The argument `s` must be a valid, nul-terminated C string pointer. Passing
// an invalid or non-nul-terminated pointer is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn heap_str_from_cstr(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len = libc::strlen(s);
        let obj = heap_str_alloc(len);
        if obj.is_null() {
            return ptr::null_mut();
        }

        // Copy string data to offset +16
        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;
        libc::memcpy(data_ptr as *mut c_void, s as *const c_void, len + 1);

        data_ptr
    }
}

// Get the object pointer from a heap string data pointer
// (reverse the +16 offset to get back to the header)
#[inline]
unsafe fn heap_str_to_obj(data: *const c_char) -> *mut c_void {
    if data.is_null() {
        return ptr::null_mut();
    }
    unsafe { (data as *mut u8).sub(16) as *mut c_void }
}

// RC increment for string pointers (handles both static and heap strings)
// For heap strings, the data pointer is at offset +16 from the object header.
//
// # Safety
// `data` must be a pointer previously returned by the runtime for a string
// (either a static literal or a heap-allocated string). Passing arbitrary
// pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_inc_str(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    unsafe {
        // Check if this looks like a heap string by checking if there's a valid header at -16
        // For now, we'll use a heuristic: try to read the header and see if it has the static bit
        let obj = heap_str_to_obj(data);
        rc_inc(obj);
    }
}

// RC decrement for string pointers (handles both static and heap strings)
//
// # Safety
// `data` must be a pointer previously returned by the runtime for a string
// (either a static literal or a heap-allocated string). Passing arbitrary
// pointers or double-dropping a pointer is undefined behavior.
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

#[unsafe(no_mangle)]
pub extern "C" fn runtime_malloc(size: size_t) -> *mut c_void {
    unsafe { libc::malloc(size) }
}

// Free memory previously allocated by the runtime (or compatible allocator).
//
// # Safety
// The pointer `p` must have been allocated by the runtime allocator and not
// already freed. Freeing invalid or non-owned pointers is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_free(p: *mut c_void) {
    unsafe { libc::free(p) }
}

// --- String Operations ---

// Compute the length of a nul-terminated C string.
//
// # Safety
// `s` must be a valid pointer to a nul-terminated C string. Passing an
// invalid pointer or non-nul-terminated memory is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn runtime_strlen(s: *const c_char) -> size_t {
    if s.is_null() {
        return 0;
    }
    unsafe { libc::strlen(s) }
}

// Duplicate a nul-terminated C string into a newly allocated buffer.
//
// # Safety
// `s` must point to a valid nul-terminated C string. The caller is
// responsible for freeing the returned buffer when no longer needed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn str_dup(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let len = libc::strlen(s) + 1;
        let dst = libc::malloc(len) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        libc::memcpy(dst as *mut c_void, s as *const c_void, len);
        dst
    }
}

// Concatenate two nul-terminated C strings into a newly allocated runtime string.
//
// # Safety
// Both `a` and `b` must be valid pointers to nul-terminated C strings. The
// caller is responsible for freeing the returned buffer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    if a.is_null() || b.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let la = libc::strlen(a);
        let lb = libc::strlen(b);

        // Allocate heap string with RC header
        let obj = heap_str_alloc(la + lb);
        if obj.is_null() {
            return ptr::null_mut();
        }

        // Get data pointer (offset +16)
        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;

        // Copy both strings
        libc::memcpy(data_ptr as *mut c_void, a as *const c_void, la);
        libc::memcpy(
            data_ptr.add(la as usize) as *mut c_void,
            b as *const c_void,
            lb,
        );

        // Null terminate
        *data_ptr.add((la + lb) as usize) = 0;

        data_ptr
    }
}

// --- Printing ---

#[unsafe(no_mangle)]
pub extern "C" fn print_f64(v: f64) {
    unsafe {
        libc::printf(b"%f\n\0".as_ptr() as *const c_char, v);
    }
}

// Print a nul-terminated C string to stdout (with newline).
//
// # Safety
// `s` must be a valid pointer to a nul-terminated C string. Passing an
// invalid pointer is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str(s: *const c_char) {
    if s.is_null() {
        return;
    }
    let _ = io::stdout().write_all(unsafe { CStr::from_ptr(s).to_bytes() });
    let _ = io::stdout().write_all(b"\n");
}

#[unsafe(no_mangle)]
pub extern "C" fn print_i32(v: i32) {
    unsafe {
        libc::printf(b"%d\n\0".as_ptr() as *const c_char, v);
    }
}

// Convert a number (f64) to a string with RC header
// Returns a heap-allocated string (data pointer, header is at offset -16)
#[unsafe(no_mangle)]
pub extern "C" fn number_to_string(num: f64) -> *mut c_char {
    // Format the number using libc's snprintf
    unsafe {
        // First, get the required buffer size
        let len = libc::snprintf(ptr::null_mut(), 0, b"%g\0".as_ptr() as *const c_char, num);

        if len < 0 {
            return ptr::null_mut();
        }

        // Allocate heap string with RC header
        let obj = heap_str_alloc(len as size_t);
        if obj.is_null() {
            return ptr::null_mut();
        }

        // Get data pointer (offset +16)
        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;

        // Format the number into the buffer
        libc::snprintf(
            data_ptr,
            (len + 1) as size_t,
            b"%g\0".as_ptr() as *const c_char,
            num,
        );

        data_ptr
    }
}

// Math.random() -> f64 in [0, 1)
#[unsafe(no_mangle)]
pub extern "C" fn math_random() -> f64 {
    // Seed libc PRNG once per process to avoid deterministic output across runs.
    // We use std::sync::Once for thread-safe one-time initialization.
    static INIT_RAND: std::sync::Once = std::sync::Once::new();
    INIT_RAND.call_once(|| unsafe {
        // Use gettimeofday for microsecond resolution to avoid same-second collisions.
        let mut tv: libc::timeval = std::mem::zeroed();
        if libc::gettimeofday(&mut tv as *mut libc::timeval, std::ptr::null_mut()) == 0 {
            let secs = tv.tv_sec as u64;
            let usec = tv.tv_usec as u64;
            let pid = libc::getpid() as u64;
            let seed = ((secs << 32) ^ usec ^ (pid as u64)) as u32;
            libc::srand(seed);
        } else {
            // Fallback to time-based seeding
            let now = libc::time(std::ptr::null_mut()) as u64;
            libc::srand((now as u32).wrapping_mul(1664525).wrapping_add(1013904223));
        }
    });

    // Simple PRNG using libc::rand() normalized to [0,1).
    unsafe {
        let r = libc::rand() as f64;
        let m = libc::RAND_MAX as f64;
        if m <= 0.0 { 0.0 } else { r / (m + 1.0) }
    }
}

// --- Reference Counting ---

// --- Reference Counting ---

// --- Array Operations ---
//
// Array Layout: [header: u64][len: u64][data...]
// header: high 32 bits = flags/type, low 32 bits = refcount
//
// The header offset for data is consistently 16 bytes (2 * u64).
const ARRAY_HEADER_SIZE: usize = mem::size_of::<u64>() * 2;

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

// Allocate an array on the runtime heap.
//
// # Safety
// This function performs raw memory allocation and writes to raw pointers.
// Callers must ensure the returned pointer is handled correctly and not
// dereferenced from safe Rust code without proper checks.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_alloc(
    len: usize,
    elem_size: usize,
    elem_is_number: i32,
) -> *mut c_void {
    let data_bytes = len * elem_size;
    let total_bytes = ARRAY_HEADER_SIZE + data_bytes;
    unsafe {
        let p = libc::malloc(total_bytes) as *mut u8;
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

        // The data area is not zeroed by default.
        p as *mut c_void
    }
}

// Load a f64 element from an array.
//
// # Safety
// Caller must ensure `arr` is a valid array pointer returned by `array_alloc` and
// `idx` is within bounds. Undefined behavior may occur otherwise.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_get_f64(arr: *mut c_void, idx: usize) -> f64 {
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

// Loads an element from a pointer-typed array and returns an owned pointer.
//
// This helper increments the referenced object's refcount before returning
// so the caller receives an owning reference. The caller is responsible for
// eventually calling `rc_dec` on the returned pointer.
// Load a pointer element from a pointer-typed array and return an owned pointer.
//
// # Safety
// Caller must ensure `arr` is a valid pointer-typed array. The returned pointer
// is an owning reference (its refcount has been incremented) and must be
// eventually released with `rc_dec`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_get_ptr(arr: *mut c_void, idx: usize) -> *mut c_void {
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

// Returns a borrowed pointer from a pointer-typed array without changing refcounts.
//
// The caller must not store the returned pointer into any long-lived location
// without first manually calling `rc_inc`. This is useful for short-lived,
// read-only access where the pointer's lifetime is guaranteed to be short.
// Borrow a pointer from a pointer-typed array without changing refcounts.
//
// # Safety
// Caller must ensure `arr` is valid and the borrowed pointer is not stored into
// long-lived locations without calling `rc_inc` first.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_get_ptr_borrow(arr: *mut c_void, idx: usize) -> *mut c_void {
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

// Set a f64 element in an array.
//
// # Safety
// Caller must ensure `arr` is a valid array pointer and `idx` is within bounds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_f64(arr: *mut c_void, idx: usize, v: f64) {
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
        let elem_ptr = data_start.add(idx * mem::size_of::<f64>()) as *mut f64;
        *elem_ptr = v;
    }
}

// Sets a pointer in a pointer-typed array, managing refcounts correctly.
//
// It increments the new pointer's refcount (if not null) and decrements
// the old pointer's refcount (if not null) that was replaced.
// Set a pointer into a pointer-typed array, managing refcounts.
//
// # Safety
// Caller must ensure `arr` is a valid pointer-typed array and `idx` is within bounds.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_set_ptr(arr: *mut c_void, idx: usize, p: *mut c_void) {
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

// Sets a pointer in a pointer-typed array, managing weak refcounts correctly.
// This is like `array_set_ptr` but uses weak refcount ops so the array holds
// a non-owning (weak) reference to the object.
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

// Push a f64 value onto an existing array.
// Note: this does not perform capacity checks or reallocation. The caller
// must ensure the array was allocated with sufficient space.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_push_f64(arr: *mut c_void, value: f64) {
    if arr.is_null() {
        return;
    }
    unsafe {
        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        let len = *len_ptr as usize;
        let data_start = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let elem_ptr = data_start.add(len * mem::size_of::<f64>()) as *mut f64;
        *elem_ptr = value;
        *len_ptr = (len + 1) as u64;
    }
}

// Pop a f64 value from an array and return it. If the array is empty,
// returns 0.0.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_pop_f64(arr: *mut c_void) -> f64 {
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

// Push a pointer-sized element into a pointer-typed array. Increments the
// pushed pointer's refcount to reflect array ownership. Caller must ensure
// sufficient capacity.
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

// Push a pointer-sized element into a pointer-typed array using WEAK refs.
// Increments the pushed pointer's WEAK refcount to reflect array's non-owning reference.
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

// Pop a pointer element from a pointer-typed array and return it. Returns
// NULL if the array is empty. Ownership of the returned pointer is
// transferred to the caller (no refcount change is performed).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_pop_ptr(arr: *mut c_void) -> *mut c_void {
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

// Atomically increments the reference count of a heap-allocated object.
// No-op for static/immortal objects.
// Handles both object pointers and string data pointers (at offset +16 from header).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_inc(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
        // Try to determine the actual object pointer
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

        // Increment RC on the object header
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

// Get the base object pointer from a pointer that might be either:
// - An object pointer (points to header at offset 0)
// - A string data pointer (points to data at offset +16 from header)
#[inline]
unsafe fn get_object_base(p: *mut c_void) -> *mut c_void {
    if p.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        // Check if p looks like an object pointer (has valid header at offset 0)
        let header = p as *const AtomicU64;
        let header_val = (*header).load(Ordering::Relaxed);

        // Valid header check: either has static bit, or has reasonable RC value
        // If RC is between 0 and 10000 and flags look reasonable, assume it's a valid header
        let rc = header_val & HEADER_RC_MASK;
        let flags = (header_val >> 32) as u32;

        if rc < 10000 && (flags == 0 || flags == 1) {
            // Looks like a valid header, use this pointer as-is
            return p;
        }

        // Otherwise, try offset -16 (string data pointer case)
        let obj_ptr = (p as *const u8).sub(16) as *mut c_void;
        let obj_header = obj_ptr as *const AtomicU64;
        let obj_header_val = (*obj_header).load(Ordering::Relaxed);
        let obj_rc = obj_header_val & HEADER_RC_MASK;
        let obj_flags = (obj_header_val >> 32) as u32;

        if obj_rc < 10000 && (obj_flags == 0 || obj_flags == 1) {
            // Found valid header at -16, this is a string data pointer
            return obj_ptr;
        }

        // Fallback: assume it's an object pointer
        p
    }
}

// Atomically decrements the reference count and frees the object if the count reaches zero.
// No-op for static/immortal objects.
// Handles both object pointers and string data pointers (at offset +16 from header).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_dec(p: *mut c_void) {
    if p.is_null() {
        return;
    }
    unsafe {
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

                        // After destructor runs, decrement the weak count on the control block
                        // (the object's control block must survive until weak count reaches zero).
                        rc_weak_dec(obj_ptr);
                        // rc_weak_dec is responsible for freeing when weak count reaches zero.
                    }
                    break;
                }
                Err(_) => continue, // Spin on contention
            }
        }
    }
}

// Atomically increment the weak count on an object. No-op for static objects.
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

// Atomically decrement the weak count and free the object when weak reaches zero
// (and strong is already zero). No-op for static objects.
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
                        libc::free(obj_ptr);
                    }
                    break;
                }
                Err(_) => continue,
            }
        }
    }
}

// Attempt to upgrade a weak pointer into a strong one.
// Returns the object pointer (same as input resolved to base) with strong
// count incremented if successful, or NULL if the object has already been
// destroyed (strong==0).
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

// Destructor for union objects: reads discriminant at offset +16 and if the
// payload is a pointer (discriminant == 1) calls rc_dec on the stored pointer.
extern "C" fn union_dtor(obj_ptr: *mut c_void) {
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

#[unsafe(no_mangle)]
pub extern "C" fn union_box_f64(v: f64) -> *mut c_void {
    unsafe {
        // layout: header(8) | dtor_ptr(8) | discrim(8) | payload(8)
        let total = 8 * 4;
        let mem = runtime_malloc(total as size_t) as *mut u8;
        if mem.is_null() {
            return ptr::null_mut();
        }

        // header: type_tag=1 (dtor present) | rc=1
        let header_ptr = mem as *mut u64;
        let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
        let refcount: u64 = 1u64;
        *header_ptr = header_with_weak(type_tag | refcount, 0);

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

#[unsafe(no_mangle)]
pub extern "C" fn union_box_ptr(p: *mut c_void) -> *mut c_void {
    unsafe {
        let total = 8 * 4;
        let mem = runtime_malloc(total as size_t) as *mut u8;
        if mem.is_null() {
            return ptr::null_mut();
        }

        let header_ptr = mem as *mut u64;
        let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
        let refcount: u64 = 1u64;
        *header_ptr = header_with_weak(type_tag | refcount, 0);

        // store dtor
        let dtor_ptr = mem.add(8) as *mut *mut c_void;
        *dtor_ptr = union_dtor as *mut c_void;

        // discriminant = 1 for pointer
        let discrim_ptr = mem.add(16) as *mut u64;
        *discrim_ptr = 1u64;

        // payload pointer at offset +24
        let payload_ptr_ptr = mem.add(24) as *mut *mut c_void;
        *payload_ptr_ptr = p;
        if !p.is_null() {
            rc_inc(p);
        }

        mem as *mut c_void
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn union_unbox_f64(u: *mut c_void) -> f64 {
    if u.is_null() {
        return 0.0;
    }
    unsafe {
        let payload = (u as *mut u8).add(24) as *mut f64;
        *payload
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn union_unbox_ptr(u: *mut c_void) -> *mut c_void {
    if u.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let payload_ptr_ptr = (u as *mut u8).add(24) as *mut *mut c_void;
        let p = *payload_ptr_ptr;
        if !p.is_null() {
            rc_inc(p);
        }
        p
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn union_get_discriminant(u: *mut c_void) -> i64 {
    if u.is_null() {
        return -1;
    }
    unsafe {
        let discrim_ptr = (u as *mut u8).add(16) as *mut u64;
        *discrim_ptr as i64
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

    extern "C" fn my_dtor(p: *mut c_void) {
        let _ = p;
        CALLED.store(true, Ordering::SeqCst);
    }

    #[test]
    fn test_rc_dec_calls_destructor() {
        unsafe {
            // allocate two u64 words: header + dtor pointer via runtime_malloc
            let size = std::mem::size_of::<u64>() * 2;
            let mem = runtime_malloc(size) as *mut u8;
            assert!(!mem.is_null());

            // set header: type_tag=1, refcount=1, weak=0
            let header_ptr = mem as *mut u64;
            let type_tag: u64 = 1u64 << HEADER_TYPE_TAG_SHIFT;
            let refcount: u64 = 1u64;
            let header_val: u64 = header_with_weak(type_tag | refcount, 0);
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
}
