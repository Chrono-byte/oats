//! Misc helpers and logging for the runtime

use libc::{c_char, c_void};
use std::ffi::CStr;

use crate::{MAX_RECURSION_DEPTH, is_plausible_addr};

/// Helper: stringify an arbitrary pointer-or-raw-8-bytes value with depth guard.
#[allow(dead_code)]
pub(crate) fn stringify_value_raw(val_raw: u64, depth: usize) -> String {
    if depth > MAX_RECURSION_DEPTH {
        return "...".to_string();
    }
    // Heuristic: treat values that look like plausible addresses as pointers
    let addr = val_raw as usize;
    if is_plausible_addr(addr) {
        let p = addr as *mut c_void;
        // Try array/tuple/string recursion
        let s_ptr = unsafe { crate::array_to_string(p) };
        if !s_ptr.is_null() {
            let s = unsafe { CStr::from_ptr(s_ptr) };
            let res = s.to_string_lossy().into_owned();
            unsafe {
                crate::rc_dec_str(s_ptr);
            }
            return res;
        }
        // Fallback: try to interpret as a C string
        let maybe = unsafe { CStr::from_ptr(p as *const c_char) };
        if let Ok(st) = maybe.to_str() {
            return format!("\"{}\"", st);
        }
        return format!("<ptr {:p}>", p);
    }
    // Otherwise interpret as f64 bits
    let f = f64::from_bits(val_raw);
    format!("{}", f)
}

#[allow(unused_imports)]
pub(crate) use crate::heap::check_and_reserve_allocation;
#[allow(unused_imports)]
pub(crate) use crate::heap::release_allocation;
#[allow(unused_imports)]
pub(crate) use crate::init_resource_limits;
#[allow(unused_imports)]
pub(crate) use crate::init_runtime_log;
#[allow(unused_imports)]
pub(crate) use crate::validate_meta_block;
