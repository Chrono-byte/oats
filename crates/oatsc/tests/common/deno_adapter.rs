//! Minimal assertion shims that mirror Deno's `std/testing/asserts.ts` helpers.
//! These helpers let us port Deno tests without immediately rewriting their
//! expectation syntax.

#![allow(dead_code)]

use std::fmt::Debug;

/// Matches Deno's `assert(condition)` helper.
pub fn assert(condition: bool, msg: &str) {
    if !condition {
        panic!("assertion failed: {msg}");
    }
}

/// Matches Deno's `assertEquals(actual, expected)` helper.
pub fn assert_equals<T>(actual: T, expected: T, msg: &str)
where
    T: PartialEq + Debug,
{
    if actual != expected {
        panic!("assertEquals failed: {msg}; actual={actual:?}, expected={expected:?}");
    }
}

/// Matches Deno's `assertNotEquals(actual, expected)` helper.
pub fn assert_not_equals<T>(actual: T, expected: T, msg: &str)
where
    T: PartialEq + Debug,
{
    if actual == expected {
        panic!("assertNotEquals failed: {msg}; both={actual:?}");
    }
}

/// Matches Deno's `assertThrows` helper for sync functions.
pub fn assert_throws<F, E>(func: F, expected: &str)
where
    F: FnOnce() -> Result<(), E>,
    E: Debug,
{
    match func() {
        Ok(()) => panic!("assertThrows expected error containing '{expected}'"),
        Err(err) => {
            let err_str = format!("{err:?}");
            if !err_str.contains(expected) {
                panic!(
                    "assertThrows message mismatch: expected fragment '{expected}', got {err_str}"
                );
            }
        }
    }
}

/// Marks a test case as pending while we stand up the full Deno harness.
pub fn pending(reason: &str) {
    eprintln!("skipping Deno-sourced test: {reason}");
}
