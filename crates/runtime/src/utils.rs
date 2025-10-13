//! Misc helpers and logging for the runtime (shims)

#[allow(unused_imports)]
pub(crate) use crate::init_runtime_log;
#[allow(unused_imports)]
pub(crate) use crate::init_resource_limits;
#[allow(unused_imports)]
pub(crate) use crate::check_and_reserve_allocation;
#[allow(unused_imports)]
pub(crate) use crate::release_allocation;
#[allow(unused_imports)]
pub(crate) use crate::is_plausible_addr;
#[allow(unused_imports)]
pub(crate) use crate::validate_meta_block;

pub(crate) fn init_utils_placeholders() {
    // no-op shim
}
