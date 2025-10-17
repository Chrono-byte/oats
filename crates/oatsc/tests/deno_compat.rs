//! Smoke tests for vendored Deno fixtures.
//!
//! These tests ensure the Oats parser can ingest the TypeScript/JavaScript
//! files we vendor from Deno. For now we limit the scope to syntactic
//! compatibility: we simply parse each listed source file and verify the
//! parser does not emit diagnostics. This provides an early warning if new
//! language constructs appear in the upstream fixtures.

use oatsc::parser;
use std::fs;
use std::path::{Path, PathBuf};

#[test]
fn deno_fixtures_parse_cleanly() -> Result<(), Box<dyn std::error::Error>> {
    let root = deno_root();
    let allowlist = load_allowlist(&root)?;
    assert!(
        !allowlist.is_empty(),
        "allow-list is empty; populate third_party/deno_tests/allowlist.txt"
    );

    let mut parsed_files = Vec::new();

    for fixture in allowlist {
        let fixture_root = root.join(&fixture);
        assert!(
            fixture_root.exists(),
            "allow-listed fixture does not exist: {}",
            fixture_root.display()
        );

        let sources = collect_source_files(&fixture_root)?;

        assert!(
            !sources.is_empty(),
            "fixture contains no parseable .ts/.js files: {}",
            fixture_root.display()
        );

        for source in sources {
            let contents = fs::read_to_string(&source)?;
            parser::parse_oats_module(&contents, source.to_str())?;
            parsed_files.push(source);
        }
    }

    assert!(
        !parsed_files.is_empty(),
        "no files parsed from Deno fixtures; check allow-list entries"
    );

    Ok(())
}

fn load_allowlist(root: &Path) -> std::io::Result<Vec<String>> {
    let allowlist_path = root.join("allowlist.txt");
    let data = fs::read_to_string(&allowlist_path)?;
    let mut entries = Vec::new();
    for line in data.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        entries.push(trimmed.to_string());
    }
    Ok(entries)
}

fn collect_source_files(root: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut out = Vec::new();
    collect_recursive(root, &mut out)?;
    Ok(out)
}

fn collect_recursive(dir: &Path, out: &mut Vec<PathBuf>) -> std::io::Result<()> {
    if dir.is_file() {
        if is_source_file(dir) {
            out.push(dir.to_path_buf());
        }
        return Ok(());
    }

    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if entry.file_type()?.is_dir() {
            collect_recursive(&path, out)?;
        } else if is_source_file(&path) {
            out.push(path);
        }
    }
    Ok(())
}

fn is_source_file(path: &Path) -> bool {
    if let Some(ext) = path.extension().and_then(|ext| ext.to_str()) {
        let ext_lower = ext.to_ascii_lowercase();
        matches!(
            ext_lower.as_str(),
            "ts" | "tsx" | "js" | "jsx" | "mts" | "cts" | "mjs" | "cjs"
        )
    } else {
        false
    }
}

fn deno_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../third_party/deno_tests")
}
