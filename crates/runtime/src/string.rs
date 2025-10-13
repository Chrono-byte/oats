//! String helpers for runtime (shims)

#[allow(unused_imports)]
pub use crate::heap_str_alloc;
#[allow(unused_imports)]
pub use crate::heap_str_from_cstr;
#[allow(unused_imports)]
pub use crate::rc_dec_str;
#[allow(unused_imports)]
pub use crate::rc_inc_str;
#[allow(unused_imports)]
pub use crate::str_concat;
#[allow(unused_imports)]
pub use crate::str_dup;

pub(crate) fn init_string_placeholders() {
    // no-op shim
}
