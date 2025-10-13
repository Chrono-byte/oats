use std::sync::atomic::AtomicU64;

// Header flag constants
// New header layout reserves 16 bits for a weak reference count in bits 33-48.
pub const HEADER_STATIC_BIT: u64 = 1u64 << 32;
// Low 32 bits are strong refcount
pub const HEADER_RC_MASK: u64 = 0xffffffffu64;
// Weak count occupies bits 33-48 (16 bits)
pub const HEADER_WEAK_SHIFT: u64 = 33;
pub const HEADER_WEAK_MASK: u64 = 0xffffu64 << HEADER_WEAK_SHIFT; // bits 33-48
// Type tag bits start at bit 49 (to avoid colliding with weak count)
pub const HEADER_TYPE_TAG_SHIFT: u64 = 49;
// Claim bit embedded in the header to avoid a global claimed-set.
// We reserve the top-most bit (bit 63) for the collector claim flag.
pub const HEADER_CLAIM_BIT: u64 = 1u64 << 63;
// Flags mask includes everything above the low 32-bit refcount
pub const HEADER_FLAGS_MASK: u64 = 0xffffffff00000000u64;

/// Create a header value for a heap-allocated object with initial refcount
#[inline]
pub fn make_heap_header(initial_rc: u32) -> u64 {
	(initial_rc as u64) & HEADER_RC_MASK
}

/// Helper to extract weak count from a header value
#[inline]
pub fn header_get_weak_bits(h: u64) -> u64 {
	(h & HEADER_WEAK_MASK) >> HEADER_WEAK_SHIFT
}

/// Helper to set weak bits into a header value (weak must fit in 16 bits)
#[inline]
pub fn header_with_weak(h: u64, weak: u64) -> u64 {
	let cleared = h & !HEADER_WEAK_MASK;
	cleared | ((weak & 0xffffu64) << HEADER_WEAK_SHIFT)
}

/// Extract the runtime type tag from a full header value while masking out
/// the embedded claim bit.
#[inline]
pub fn header_type_tag(h: u64) -> u64 {
	let raw = h >> HEADER_TYPE_TAG_SHIFT;
	let claim_shifted = HEADER_CLAIM_BIT >> HEADER_TYPE_TAG_SHIFT;
	raw & !claim_shifted
}

// Re-export atomic type for callers
pub type HeaderAtomic = AtomicU64;

