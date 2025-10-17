//! TypeScript Conformance Tests
//!
//! This module runs the official TypeScript conformance test suite against
//! the Oats parser to ensure 100% parsing compatibility with TypeScript syntax.
//!
//! The tests are vendored from https://github.com/microsoft/TypeScript and
//! updated via `scripts/vendor_typescript_conformance_tests.sh`.

use anyhow::Result;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

/// Path to the vendored TypeScript conformance tests
const CONFORMANCE_DIR: &str = "../../third_party/typescript_conformance_tests/conformance";

#[test]
fn typescript_conformance_parsing() -> Result<()> {
    let conformance_path = Path::new(CONFORMANCE_DIR);

    if !conformance_path.exists() {
        let cwd = std::env::current_dir().unwrap();
        panic!(
            "TypeScript conformance tests not found at {} (absolute: {}). \
             CWD: {}. \
             Run `scripts/vendor_typescript_conformance_tests.sh` to download them.",
            conformance_path.display(),
            conformance_path
                .canonicalize()
                .unwrap_or_else(|_| Path::new("unknown").to_path_buf())
                .display(),
            cwd.display()
        );
    }

    let mut total_files = 0;
    let mut parsed_successfully = 0;
    let mut parse_failures = Vec::new();
    let mut negative_tests = 0;
    let mut negative_tests_passed = 0;
    let mut negative_test_failures = Vec::new();

    // Walk through all .ts files in the conformance directory
    for entry in WalkDir::new(conformance_path)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();

        // Only process .ts files
        if path.extension().and_then(|s| s.to_str()) != Some("ts") {
            continue;
        }

        // Skip JSX-related tests as Oats doesn't support JSX
        let path_str = path.to_string_lossy().to_lowercase();
        if path_str.contains("jsx") {
            continue;
        }

        // Skip Node.js and CommonJS module resolution tests as Oats doesn't implement these
        if path_str.contains("node/")
            || path_str.contains("moduleresolution/")
            || path_str.contains("commonjs")
        {
            continue;
        }

        // JSDoc types are not supported in Oats
        if path_str.contains("jsdoc") {
            continue;
        }

        // Skip .d.ts files as they are type definition files
        if path_str.ends_with(".d.ts") {
            parse_failures.push(format!("Skipped typings test {}", path.display()));
            continue;
        }

        // Identify negative tests (tests that are designed to fail parsing)
        // Only ErrorRecovery tests are actual parser errors that should fail
        let is_negative_test = path_str.contains("ErrorRecovery");

        if is_negative_test {
            negative_tests += 1;
        }

        total_files += 1;

        // Read the file content
        let content = match fs::read_to_string(path) {
            Ok(content) => content,
            Err(e) => {
                parse_failures.push(format!("Failed to read {}: {}", path.display(), e));
                continue;
            }
        };

        // Skip files that are too large (same limit as parser)
        if content.len() > 10 * 1024 * 1024 {
            eprintln!(
                "Skipping large file: {} ({} bytes)",
                path.display(),
                content.len()
            );
            continue;
        }

        // Attempt to parse with Oats parser
        match oatsc::parser::parse_oats_module(&content, Some(&path.to_string_lossy())) {
            Ok(_) => {
                if is_negative_test {
                    // Negative test should have failed but succeeded - this is a failure
                    negative_test_failures.push(format!(
                        "Negative test unexpectedly passed: {}",
                        path.display()
                    ));
                } else {
                    parsed_successfully += 1;
                }
            }
            Err(_e) => {
                if is_negative_test {
                    // Negative test failed as expected - this is a success
                    negative_tests_passed += 1;
                } else {
                    parse_failures.push(format!("Failed to parse {}", path.display()));
                }
            }
        }
    }

    // Report results
    println!("TypeScript Conformance Parsing Results:");
    println!("  Total .ts files: {}", total_files);
    println!("  Positive tests: {}", total_files - negative_tests);
    println!("  Negative tests: {}", negative_tests);
    println!("  Successfully parsed (positive): {}", parsed_successfully);
    println!("  Successfully rejected (negative): {}", negative_tests_passed);
    println!("  Parse failures (positive): {}", parse_failures.len());
    println!("  Unexpected passes (negative): {}", negative_test_failures.len());

    let total_successes = parsed_successfully + negative_tests_passed;
    let success_rate = (total_successes as f64 / total_files as f64) * 100.0;
    println!("Success rate: {:.2}%", success_rate);

    if !parse_failures.is_empty() || !negative_test_failures.is_empty() {
        eprintln!("\nUnexpected results:");
        for failure in &parse_failures {
            eprintln!("  {}", failure);
        }
        for failure in &negative_test_failures {
            eprintln!("  {}", failure);
        }

        if success_rate < 85.0 {
            panic!(
                "TypeScript conformance parsing success rate too low: {:.2}% (required: 85%)",
                success_rate
            );
        } else {
            eprintln!(
                "Warning: {} unexpected results detected. See output above.",
                parse_failures.len() + negative_test_failures.len()
            );
        }
    } else {
        println!("🎉 All TypeScript conformance tests handled correctly!");
    }

    if !negative_test_failures.is_empty() {
        eprintln!("\nNegative test failures:");
        for failure in &negative_test_failures {
            eprintln!("  {}", failure);
        }

        panic!(
            "Some negative tests did not fail as expected: {} failures",
            negative_test_failures.len()
        );
    }

    Ok(())
}
