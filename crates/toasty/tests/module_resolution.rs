use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn module_resolution_discovers_dependencies() -> Result<(), Box<dyn std::error::Error>> {
    // Test that toasty discovers imported modules
    let mut cmd = Command::cargo_bin("toasty")?;
    
    // Build the multi_file example
    let mut example = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    example = example.join("../../examples/multi_file/main.oats");
    let example = example.canonicalize()?;
    
    // Run from workspace root
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    cmd.current_dir(workspace_root);
    
    // Run with verbose to see module discovery
    cmd.arg("--verbose")
        .arg("build")
        .arg(example.as_os_str());
    
    // Check that it discovered both modules
    cmd.assert()
        .stderr(predicate::str::contains("Found 2 module(s) to compile"))
        .stderr(predicate::str::contains("main.oats"))
        .stderr(predicate::str::contains("math.oats"));
    
    Ok(())
}

#[test]
fn single_file_module_resolution() -> Result<(), Box<dyn std::error::Error>> {
    // Test that single files work correctly
    let mut cmd = Command::cargo_bin("toasty")?;
    
    let mut example = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    example = example.join("../../examples/add.oats");
    let example = example.canonicalize()?;
    
    // Ensure runtime is built
    let mut runc = Command::new("cargo");
    runc.current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../.."));
    runc.arg("build").arg("-p").arg("runtime").arg("--release");
    runc.status()?;
    
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    cmd.current_dir(workspace_root);
    
    cmd.arg("--verbose")
        .arg("build")
        .arg(example.as_os_str());
    
    // Should discover just 1 module
    cmd.assert()
        .success()
        .stderr(predicate::str::contains("Found 1 module(s) to compile"));
    
    Ok(())
}
