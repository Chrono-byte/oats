//! Heap allocation and layout helpers (shims)
//!
//! Re-export runtime allocator functions implemented in the crate root.

#[allow(unused_imports)]
pub use crate::runtime_malloc;
#[allow(unused_imports)]
pub(crate) use crate::runtime_free;
#[allow(unused_imports)]
pub use crate::heap_str_alloc;
#[allow(unused_imports)]
pub use crate::heap_str_from_cstr;
#[allow(unused_imports)]
pub use crate::rc_inc_str;
#[allow(unused_imports)]
pub use crate::rc_dec_str;

pub(crate) fn init_heap_placeholders() {
    // no-op shim
}
