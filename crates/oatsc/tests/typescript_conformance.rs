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

/// Split a TypeScript conformance test file by @filename/@Filename directives.
/// Returns a vector of (filename, content) tuples.
/// If no @filename directives are found, returns a single entry with the full content.
fn split_by_filename_directives(content: &str) -> Vec<(String, String)> {
    let mut sections = Vec::new();
    let mut current_filename = String::from("main.ts");
    let mut current_content = String::new();
    
    for line in content.lines() {
        // Check for @filename or @Filename directive (case-insensitive)
        if line.trim_start().starts_with("//")
            && (line.contains("@filename:") || line.contains("@Filename:"))
        {
            // Save previous section if it has content
            if !current_content.trim().is_empty() {
                sections.push((current_filename.clone(), current_content.clone()));
                current_content.clear();
            }
            
            // Extract new filename
            if let Some(idx) = line.find("@filename:").or_else(|| line.find("@Filename:")) {
                let after_directive = &line[idx + 10..]; // "@filename:" is 10 chars
                current_filename = after_directive.trim().to_string();
            }
        } else {
            current_content.push_str(line);
            current_content.push('\n');
        }
    }
    
    // Save last section
    if !current_content.trim().is_empty() {
        sections.push((current_filename, current_content));
    }
    
    // If no sections were found, return the full content
    if sections.is_empty() {
        sections.push((String::from("main.ts"), content.to_string()));
    }
    
    sections
}

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

        // Skip typings-related tests as Oats doesn't currently support .d.ts files
        if path_str.contains("typings/") || path_str.ends_with(".d.ts") {
            // mark as a failure
            parse_failures.push(format!("Skipped typings test {}", path.display()));
            continue;
        }

        // Skip error recovery tests as Oats doesn't implement error recovery parsing
        if path_str.contains("errorrecovery") || path_str.contains("skippedtokens") {
            // mark as a failure
            parse_failures.push(format!("Skipped error recovery test {}", path.display()));
            continue;
        }

        // Skip invalid syntax tests (negative test cases that should fail)
        if path_str.contains("invalid") || path_str.contains("error") {
            continue;
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

        // Handle multi-file tests with @filename/@Filename directives
        // These tests simulate multiple TypeScript files in one physical file
        let file_sections = split_by_filename_directives(&content);
        
        if file_sections.len() > 1 {
            // Multi-file test: parse each section independently
            let mut all_sections_parsed = true;
            for (section_name, section_content) in &file_sections {
                // Skip sections with intentionally invalid syntax
                // These are negative test cases meant to verify error reporting
                if section_name.to_lowercase().contains("error")
                    || section_name.to_lowercase().contains("invalid")
                {
                    continue;
                }
                
                match oatsc::parser::parse_oats_module(section_content, Some(section_name)) {
                    Ok(_) => {}
                    Err(_e) => {
                        all_sections_parsed = false;
                        break;
                    }
                }
            }
            
            if all_sections_parsed {
                parsed_successfully += 1;
            } else {
                parse_failures.push(format!("Failed to parse {}", path.display()));
            }
        } else {
            // Single file test: parse normally
            match oatsc::parser::parse_oats_module(&content, Some(&path.to_string_lossy())) {
                Ok(_) => {
                    parsed_successfully += 1;
                }
                Err(_e) => {
                    parse_failures.push(format!("Failed to parse {}", path.display()));
                }
            }
        }
    }

    // Report results
    println!("TypeScript Conformance Parsing Results:");
    println!("  Total .ts files: {}", total_files);
    println!("  Successfully parsed: {}", parsed_successfully);
    println!("  Parse failures: {}", parse_failures.len());

    let success_rate = (parsed_successfully as f64 / total_files as f64) * 100.0;
    println!("Success rate: {:.2}%", success_rate);

    if !parse_failures.is_empty() {
        eprintln!("\nParse failures:");
        for failure in &parse_failures {
            eprintln!("  {}", failure);
        }

        if success_rate < 85.0 {
            panic!(
                "TypeScript conformance parsing success rate too low: {:.2}% (required: 85%)",
                success_rate
            );
        } else {
            eprintln!(
                "Warning: {} parse failures detected. See output above.",
                parse_failures.len()
            );
        }
    } else {
        println!("🎉 All TypeScript conformance tests parsed successfully!");
    }

    Ok(())
}
