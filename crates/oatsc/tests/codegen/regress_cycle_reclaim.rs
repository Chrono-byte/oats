use std::fs;
use std::path::PathBuf;

use super::common;
use common::gen_ir_for_source;

#[test]
fn regress_cycle_reclaim() -> Result<(), Box<dyn std::error::Error>> {
    // Silence diagnostics printed to stderr during tests
    let _guard = oatsc::diagnostics::suppress();

    // examples/cycle_reclaim.oats lives at repository_root/examples/cycle_reclaim.oats
    let repo_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..");
    let example_path = repo_root.join("examples").join("cycle_reclaim.oats");
    let src = fs::read_to_string(&example_path)?;

    // Generate IR for the example source and ensure it produces something non-empty.
    // common::gen_ir_for_source() is the shared test helper used across the test suite.
    let ir = gen_ir_for_source(&src)?;
    assert!(
        !ir.trim().is_empty(),
        "IR generation produced empty output for cycle_reclaim.oats"
    );
    Ok(())
}
