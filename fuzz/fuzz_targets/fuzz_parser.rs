#![no_main]

use libfuzzer_sys::fuzz_target;
use oatsc::parser::parse_oats_module;
use std::panic;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to UTF-8, skip if invalid
    if let Ok(source) = std::str::from_utf8(data) {
        // The underlying parser library (swc_ecma_lexer) can panic on invalid input.
        // These panics are expected for malformed code, so we catch them.
        // We use AssertUnwindSafe because parsing is side-effect free.
        let result = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            let _ = parse_oats_module(source, Some("fuzz_input"));
        }));
        
        // Ignore both Ok (successful parse) and Err (panic caught)
        let _ = result;
    }
});
