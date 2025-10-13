//! C ABI exports for the runtime. This module centralizes all C-callable
//! symbols (#[no_mangle] extern "C" functions) so the public ABI is easy
//! to audit and maintain.

use libc::{c_char, c_void, size_t};
use std::ffi::{CStr, CString};
use std::io::{self, Write};
use std::mem;
use std::process;
use std::ptr;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::*;

// Re-exported from this module by `lib.rs`.

/// Initialize the background collector (idempotent).
#[unsafe(no_mangle)]
pub extern "C" fn collector_init() {
    let _ = init_collector();
}

#[unsafe(no_mangle)]
pub extern "C" fn collector_shutdown() {
    let c = init_collector();
    c.stop();
}

#[unsafe(no_mangle)]
pub extern "C" fn collector_collect_now() {
    let c = init_collector();
    let roots = c.drain_now();
    crate::collector::Collector::process_roots(&roots);
}

// collector_test_enqueue is provided under the feature gate by the original
// file; keep it here as well.
#[cfg(feature = "collector-test")]
#[unsafe(no_mangle)]
pub extern "C" fn collector_test_enqueue() {
    // body moved from original lib.rs
    let size = std::mem::size_of::<u64>() * 2;
    let mem = runtime_malloc(size as size_t) as *mut u8;
    if mem.is_null() {
        return;
    }
    unsafe {
        let header_ptr = mem as *mut u64;
        *header_ptr = make_heap_header(1);
        let second = mem.add(std::mem::size_of::<u64>()) as *mut u64;
        *second = 0u64;
    }
    add_root_candidate(mem as *mut c_void);
}

#[unsafe(no_mangle)]
pub extern "C" fn heap_str_alloc(str_len: size_t) -> *mut c_void {
    unsafe {
        let total_size = match 16usize.checked_add(str_len) {
            Some(s) => match s.checked_add(1) {
                Some(total) => total,
                None => {
                    if RUNTIME_LOG.load(Ordering::Relaxed) {
                        let _ = io::stderr().write_all(
                            format!(
                                "[oats runtime] heap_str_alloc: integer overflow (str_len={})\n",
                                str_len
                            )
                            .as_bytes(),
                        );
                    }
                    return ptr::null_mut();
                }
            },
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] heap_str_alloc: integer overflow (str_len={})\n",
                            str_len
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        let p = runtime_malloc(total_size);
        if p.is_null() {
            return ptr::null_mut();
        }

        let header_ptr = p as *mut u64;
        *header_ptr = make_heap_header(1);

        let len_ptr = (p as *mut u8).add(8) as *mut u64;
        *len_ptr = str_len as u64;

        p
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn heap_str_from_cstr(s: *const c_char) -> *mut c_char {
    if s.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let cstr = CStr::from_ptr(s);
        let bytes = cstr.to_bytes_with_nul();
        let len = bytes.len() - 1;
        let obj = heap_str_alloc(len as size_t);
        if obj.is_null() {
            return ptr::null_mut();
        }
        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;
        ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr as *mut u8, bytes.len());
        data_ptr
    }
}

#[inline]
unsafe fn heap_str_to_obj(data: *const c_char) -> *mut c_void {
    if data.is_null() {
        return ptr::null_mut();
    }
    unsafe { (data as *mut u8).sub(16) as *mut c_void }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn rc_inc_str(data: *mut c_char) {
    if data.is_null() {
        return;
    }
    unsafe {
        let obj = heap_str_to_obj(data);
        rc_inc(obj);
    }
}

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

        let layout = std::alloc::Layout::from_size_align(total, 8).unwrap();
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

#[unsafe(no_mangle)]
pub unsafe fn runtime_free(p: *mut c_void) {
    unsafe {
        if p.is_null() {
            return;
        }
        let base = (p as *mut u8).sub(std::mem::size_of::<u64>());
        let size_ptr = base as *mut u64;
        let total = *size_ptr as usize;
        if total == 0 {
            return;
        }

        release_allocation(total as u64);

        let layout = std::alloc::Layout::from_size_align(total, 8).unwrap();
        std::alloc::dealloc(base, layout);
    }
}

#[unsafe(no_mangle)]
pub unsafe fn runtime_strlen(s: *const c_char) -> size_t {
    if s.is_null() {
        return 0;
    }
    let c = unsafe { CStr::from_ptr(s) };
    c.to_bytes().len() as size_t
}

#[unsafe(no_mangle)]
pub unsafe fn str_dup(s: *const c_char) -> *mut c_char {
    unsafe {
        if s.is_null() {
            return ptr::null_mut();
        }
        let len = libc::strlen(s) + 1;
        let dst = runtime_malloc(len) as *mut c_char;
        if dst.is_null() {
            return ptr::null_mut();
        }
        ptr::copy_nonoverlapping(s as *const u8, dst as *mut u8, len);
        dst
    }
}

#[unsafe(no_mangle)]
pub unsafe fn str_concat(a: *const c_char, b: *const c_char) -> *mut c_char {
    unsafe {
        if a.is_null() || b.is_null() {
            return ptr::null_mut();
        }
        let la = CStr::from_ptr(a).to_bytes().len();
        let lb = CStr::from_ptr(b).to_bytes().len();

        let total_len = match la.checked_add(lb) {
            Some(len) => len,
            None => {
                if RUNTIME_LOG.load(Ordering::Relaxed) {
                    let _ = io::stderr().write_all(
                        format!(
                            "[oats runtime] str_concat: integer overflow (len_a={}, len_b={})\n",
                            la, lb
                        )
                        .as_bytes(),
                    );
                }
                return ptr::null_mut();
            }
        };

        let obj = heap_str_alloc(total_len);
        if obj.is_null() {
            return ptr::null_mut();
        }

        let data_ptr = (obj as *mut u8).add(16) as *mut c_char;

        let aslice = CStr::from_ptr(a).to_bytes();
        let bslice = CStr::from_ptr(b).to_bytes();
        ptr::copy_nonoverlapping(aslice.as_ptr(), data_ptr as *mut u8, la);
        ptr::copy_nonoverlapping(bslice.as_ptr(), data_ptr.add(la) as *mut u8, lb);

        *data_ptr.add(la + lb) = 0;

        data_ptr
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn print_f64(v: f64) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
    let _ = io::stdout().flush();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        let _ = io::stdout().write_all(CStr::from_ptr(s).to_bytes());
        let _ = io::stdout().write_all(b"\n");
        let _ = io::stdout().flush();
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn print_i32(v: i32) {
    let _ = io::stdout().write_all(format!("{}\n", v).as_bytes());
    let _ = io::stdout().flush();
}

#[unsafe(no_mangle)]
pub extern "C" fn print_f64_no_nl(v: f64) {
    let _ = io::stdout().write_all(format!("{}", v).as_bytes());
    let _ = io::stdout().flush();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn print_str_no_nl(s: *const c_char) {
    unsafe {
        if s.is_null() {
            return;
        }
        let _ = io::stdout().write_all(CStr::from_ptr(s).to_bytes());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn print_newline() {
    let _ = io::stdout().write_all(b"\n");
    let _ = io::stdout().flush();
}

#[allow(clippy::manual_c_str_literals)]
#[unsafe(no_mangle)]
pub extern "C" fn number_to_string(num: f64) -> *mut c_char {
    let s = format!("{}", num);
    let c = CString::new(s).unwrap_or_default();
    unsafe { heap_str_from_cstr(c.as_ptr()) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn array_to_string(arr: *mut c_void) -> *mut c_char {
    if arr.is_null() {
        return ptr::null_mut();
    }
    unsafe {
        let header_ptr = arr as *const u64;
        let header = *header_ptr;
        let flags = (header >> 32) as u32;
        let elem_is_number = (flags & 1) != 0;

        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;

        if elem_is_number {
            let mut s = String::new();
            s.push('[');
            for i in 0..len {
                let v = array_get_f64(arr, i);
                if i != 0 {
                    s.push_str(", ");
                }
                s.push_str(&format!("{}", v));
            }
            s.push(']');
            let c = std::ffi::CString::new(s).unwrap_or_default();
            heap_str_from_cstr(c.as_ptr())
        } else {
            let mut s = String::new();
            s.push('[');
            for i in 0..len {
                if i != 0 {
                    s.push_str(", ");
                }
                let p = array_get_ptr(arr, i);
                if p.is_null() {
                    s.push_str("null");
                    continue;
                }
                let mut printed = false;
                let header_ptr = p as *const AtomicU64;
                let header_val = (*header_ptr).load(Ordering::Relaxed);
                let type_tag = (header_val >> HEADER_TYPE_TAG_SHIFT) as u64;
                if type_tag == 0 {
                    let nested = array_to_string(p);
                    if !nested.is_null() {
                        let cstr = CStr::from_ptr(nested);
                        if let Ok(str_slice) = cstr.to_str() {
                            s.push_str(str_slice);
                            printed = true;
                        }
                        rc_dec_str(nested);
                    }
                }
                if !printed {
                    let maybe = CStr::from_ptr(p as *const c_char);
                    if let Ok(st) = maybe.to_str() {
                        s.push('"');
                        s.push_str(st);
                        s.push('"');
                        printed = true;
                    }
                }
                if !printed {
                    s.push_str(&format!("<ptr {:p}>", p));
                }
                rc_dec(p);
            }
            s.push(']');
            let c = std::ffi::CString::new(s).unwrap_or_default();
            heap_str_from_cstr(c.as_ptr())
        }
    }
}

fn stringify_value_raw(val_raw: u64, depth: usize) -> String {
    if depth > MAX_RECURSION_DEPTH {
        return "...".to_string();
    }
    let addr = val_raw as usize;
    if is_plausible_addr(addr) {
        let p = addr as *mut c_void;
        let s_ptr = unsafe { array_to_string(p) };
        if !s_ptr.is_null() {
            let s = unsafe { CStr::from_ptr(s_ptr) };
            let res = s.to_string_lossy().into_owned();
            unsafe { rc_dec_str(s_ptr) };
            return res;
        }
        let maybe = unsafe { CStr::from_ptr(p as *const c_char) };
        if let Ok(st) = maybe.to_str() {
            return format!("\"{}\"", st);
        }
        return format!("<ptr {:p}>", p);
    }
    let f = f64::from_bits(val_raw);
    format!("{}", f)
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn tuple_to_string(obj: *mut c_void) -> *mut c_char {
    if obj.is_null() {
        return ptr::null_mut();
    }
    let base = unsafe { get_object_base(obj) };
    if base.is_null() {
        return ptr::null_mut();
    }
    let meta_ptr_ptr = unsafe { (base as *mut u8).add(8) as *mut *mut u64 };
    let meta = unsafe { *meta_ptr_ptr };
    if meta.is_null() {
        return ptr::null_mut();
    }
    if unsafe { !validate_meta_block(meta, 1024) } {
        return ptr::null_mut();
    }

    let len = unsafe { ((*meta) & 0xffffffffu64) as usize };
    let offsets_ptr = unsafe { meta.add(1) as *const i32 };

    let mut parts: Vec<String> = Vec::new();
    for i in 0..len {
        let off_i32 = unsafe { *offsets_ptr.add(i) };
        let off = off_i32 as isize as usize;
        let field_addr = unsafe { (base as *mut u8).add(off) as *const u64 };
        let raw = unsafe { *field_addr as u64 };
        let s = stringify_value_raw(raw, 0);
        parts.push(s);
    }
    let joined = parts.join(", ");
    let out = format!("({})", joined);
    let c = std::ffi::CString::new(out).unwrap_or_default();
    unsafe { heap_str_from_cstr(c.as_ptr()) }
}

#[unsafe(no_mangle)]
pub extern "C" fn math_random() -> f64 {
    use std::sync::atomic::{AtomicU64, Ordering};

    static RNG_STATE: AtomicU64 = AtomicU64::new(0);
    static INIT: std::sync::Once = std::sync::Once::new();

    fn splitmix64_next(s: &AtomicU64) -> u64 {
        let mut x = s.load(Ordering::Relaxed);
        x = x.wrapping_add(0x9e3779b97f4a7c15);
        s.store(x, Ordering::Relaxed);
        let mut z = x;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        z ^ (z >> 31)
    }

    INIT.call_once(|| {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_nanos() as u64)
            .unwrap_or(0);
        let pid = process::id() as u64;
        let seed = now ^ (pid.wrapping_mul(0x9e3779b97f4a7c15));
        RNG_STATE.store(seed, Ordering::Relaxed);
    });

    let bits = splitmix64_next(&RNG_STATE);
    let mant = bits >> 12;
    let denom = (1u64 << 52) as f64;
    (mant as f64) / denom
}

// -- Arrays and RC extern functions moved below --

#[unsafe(no_mangle)]
pub extern "C" fn array_alloc(len: usize, elem_size: usize, elem_is_number: i32) -> *mut c_void {
    // body copied from lib.rs
    // For empty arrays, allocate with minimum capacity to allow push/assignment
    const ARRAY_HEADER_SIZE: usize = mem::size_of::<u64>() * 3;
    const MIN_ARRAY_CAPACITY: usize = 8;
    let capacity = if len == 0 { MIN_ARRAY_CAPACITY } else { len };
    let data_bytes = match capacity.checked_mul(elem_size) {
        Some(bytes) => bytes,
        None => {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    format!(
                        "[oats runtime] array_alloc: integer overflow (capacity={}, elem_size={})\n",
                        capacity, elem_size
                    )
                    .as_bytes(),
                );
            }
            return ptr::null_mut();
        }
    };
    let total_bytes = match ARRAY_HEADER_SIZE.checked_add(data_bytes) {
        Some(total) => total,
        None => {
            if RUNTIME_LOG.load(Ordering::Relaxed) {
                let _ = io::stderr().write_all(
                    b"[oats runtime] array_alloc: integer overflow in total size calculation\n",
                );
            }
            return ptr::null_mut();
        }
    };

    unsafe {
        let p = runtime_malloc(total_bytes) as *mut u8;
        if p.is_null() {
            return ptr::null_mut();
        }

        let header_ptr = p as *mut u64;
        let flags = ((elem_is_number as u64) << 32) & 0xffffffff00000000u64;
        let initial_rc = 1u64;
        *header_ptr = flags | initial_rc;

        let len_ptr = p.add(mem::size_of::<u64>()) as *mut u64;
        *len_ptr = len as u64;

        let cap_ptr = p.add(mem::size_of::<u64>() * 2) as *mut u64;
        *cap_ptr = capacity as u64;

        p as *mut c_void
    }
}

// array_grow remains internal to this module
unsafe fn array_grow(arr: *mut c_void, min_capacity: usize) -> *mut c_void {
    if arr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        let header_ptr = arr as *const u64;
        let header = *header_ptr;
        let elem_is_number = ((header >> 32) & 1) as i32;

        let len_ptr = (arr as *mut u8).add(mem::size_of::<u64>()) as *const u64;
        let len = *len_ptr as usize;

        let cap_ptr = (arr as *mut u8).add(mem::size_of::<u64>() * 2) as *const u64;
        let old_capacity = *cap_ptr as usize;

        let mut new_capacity = old_capacity + (old_capacity / 2).max(1);
        if new_capacity < min_capacity {
            new_capacity = min_capacity;
        }

        let elem_size = if elem_is_number != 0 { mem::size_of::<f64>() } else { mem::size_of::<*mut c_void>() };

        let new_arr = array_alloc(new_capacity, elem_size, elem_is_number);
        if new_arr.is_null() {
            return ptr::null_mut();
        }

        let new_len_ptr = (new_arr as *mut u8).add(mem::size_of::<u64>()) as *mut u64;
        *new_len_ptr = len as u64;

        let old_data = (arr as *mut u8).add(ARRAY_HEADER_SIZE);
        let new_data = (new_arr as *mut u8).add(ARRAY_HEADER_SIZE);
        ptr::copy_nonoverlapping(old_data, new_data, len * elem_size);

        if elem_is_number == 0 {
            let ptrs = new_data as *mut *mut c_void;
            for i in 0..len {
                let p = *ptrs.add(i);
                if !p.is_null() {
                    rc_inc(p);
                }
            }
        }

        new_arr
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn array_get_f64(arr: *mut c_void, idx: usize) -> f64 {
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

// End of `ffi.rs` — extern symbols centralized here.

