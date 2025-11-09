//! Random number generation

use libc::c_double;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

// Simple LCG (Linear Congruential Generator) for basic randomness
// Using thread-local state to avoid needing a global mutex
thread_local! {
    static RNG_STATE: Mutex<u64> = Mutex::new(0);
}

fn init_rng() -> u64 {
    let seed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos() as u64;
    RNG_STATE.with(|state| {
        let mut s = state.lock().unwrap();
        if *s == 0 {
            *s = seed;
        }
        *s
    })
}

fn next_random() -> u64 {
    RNG_STATE.with(|state| {
        let mut s = state.lock().unwrap();
        if *s == 0 {
            *s = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos() as u64;
        }
        // LCG parameters (same as used in glibc)
        *s = s.wrapping_mul(1103515245).wrapping_add(12345) & 0x7fffffff;
        *s
    })
}

/// Seed the random number generator
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_random_seed(seed: u64) {
    RNG_STATE.with(|state| {
        let mut s = state.lock().unwrap();
        *s = if seed == 0 { 1 } else { seed };
    });
}

/// Generate a random integer between 0 and max (exclusive)
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_random_int(max: u64) -> u64 {
    if max == 0 {
        return 0;
    }
    init_rng();
    next_random() % max
}

/// Generate a random floating point number between 0.0 and 1.0
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_random_float() -> c_double {
    init_rng();
    let r = next_random();
    (r as c_double) / (0x7fffffff as c_double)
}

/// Generate a random floating point number between min and max
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_random_float_range(min: c_double, max: c_double) -> c_double {
    if max <= min {
        return min;
    }
    let range = max - min;
    min + (oats_std_random_float() * range)
}

/// Generate a random boolean
/// #[oats_export]
#[no_mangle]
pub extern "C" fn oats_std_random_bool() -> libc::c_int {
    if oats_std_random_float() < 0.5 {
        0
    } else {
        1
    }
}

