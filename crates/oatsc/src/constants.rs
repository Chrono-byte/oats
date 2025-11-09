//! Compiler constants for magic numbers and bit patterns
//!
//! This module centralizes magic numbers and bit patterns used throughout
//! code generation. This improves Locality of Behaviour by making these
//! values explicit and traceable to their definitions.
//!
//! Note: Runtime constants are defined in `crates/runtime/src/header.rs`.
//! This module contains compiler-specific constants.

/// Static header bit for string literals and const objects
///
/// This bit (bit 32) marks objects as static/immutable, which tells the
/// runtime not to perform reference counting operations on them.
///
/// Value: `1u64 << 32`
pub const STATIC_HEADER_BIT: u64 = 1u64 << 32;

/// Metadata magic number for field maps
///
/// This is the magic number used in metadata structures to identify
/// field maps. The value is 'OATS' in ASCII.
///
/// Value: `0x4F415453u64`
pub const META_MAGIC: u64 = 0x4F415453u64; // 'OATS'

/// Type tag value for closure objects
///
/// This type tag (bits 49-63) identifies closure objects in the runtime.
///
/// Value: `3u64 << 49`
pub const CLOSURE_TYPE_TAG: u64 = 3u64 << 49;

/// Initial reference count value
///
/// New objects start with a reference count of 1.
pub const INITIAL_REFCOUNT: u64 = 1;

/// Object layout offsets (in bytes)
pub mod offsets {
    /// Header size in bytes
    pub const HEADER_SIZE: u64 = 8;

    /// Metadata slot offset from object base
    pub const META_SLOT: u64 = 8;

    /// First field offset (after header + meta)
    pub const FIRST_FIELD: u64 = 16;

    /// Size of a pointer field in bytes
    pub const FIELD_SIZE: u64 = 8;
}

