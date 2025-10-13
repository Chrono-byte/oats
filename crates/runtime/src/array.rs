//! Array helpers for runtime (shims)

#[allow(unused_imports)]
pub use crate::array_alloc;
#[allow(unused_imports)]
pub(crate) use crate::array_get_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_get_ptr;
#[allow(unused_imports)]
pub(crate) use crate::array_get_ptr_borrow;
#[allow(unused_imports)]
pub(crate) use crate::array_set_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_set_ptr;
#[allow(unused_imports)]
pub(crate) use crate::array_set_ptr_weak;
#[allow(unused_imports)]
pub(crate) use crate::array_push_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_push_ptr;
#[allow(unused_imports)]
pub(crate) use crate::array_push_ptr_weak;
#[allow(unused_imports)]
pub(crate) use crate::array_pop_f64;
#[allow(unused_imports)]
pub(crate) use crate::array_pop_ptr;

pub(crate) fn init_array_placeholders() {
    // no-op shim
}
