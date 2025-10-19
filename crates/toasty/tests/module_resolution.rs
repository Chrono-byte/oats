use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn module_resolution_discovers_dependencies() -> Result<(), Box<dyn std::error::Error>> {
    // Build runtime and oatsc first
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");

    let mut runc = Command::new("cargo");
    runc.current_dir(&workspace_root);
    runc.arg("build").arg("-p").arg("runtime").arg("--release");
    runc.status()?;

    let mut oatsc_build = Command::new("cargo");
    oatsc_build.current_dir(&workspace_root);
    oatsc_build
        .arg("build")
        .arg("-p")
        .arg("oatsc")
        .arg("--release");
    oatsc_build.status()?;

    // Test that toasty discovers imported modules
    let mut cmd = Command::cargo_bin("toasty")?;

    // Build the multi_file example
    let mut example = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    example = example.join("../../examples/multi_file/main.oats");
    let example = example.canonicalize()?;

    // Run from workspace root
    cmd.current_dir(&workspace_root);

    // Set up environment variables for oatsc and runtime
    let oatsc_path = workspace_root.join("target/release/oatsc");
    let runtime_path = workspace_root.join("target/release/libruntime.a");
    cmd.env("OATS_OATSC_PATH", oatsc_path);
    cmd.env("OATS_RUNTIME_PATH", runtime_path);

    // Run with verbose to see module discovery
    cmd.arg("--verbose").arg("build").arg(example.as_os_str());

    // Check that it discovered both modules
    cmd.assert()
        .stderr(predicate::str::contains("Dependency graph has 2 module(s)"))
        .stderr(predicate::str::contains("main.oats"))
        .stderr(predicate::str::contains("math.oats"));

    Ok(())
}

#[test]
fn single_file_module_resolution() -> Result<(), Box<dyn std::error::Error>> {
    // Ensure runtime and oatsc are built
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");

    let mut runc = Command::new("cargo");
    runc.current_dir(&workspace_root);
    runc.arg("build").arg("-p").arg("runtime").arg("--release");
    runc.status()?;

    let mut oatsc_build = Command::new("cargo");
    oatsc_build.current_dir(&workspace_root);
    oatsc_build
        .arg("build")
        .arg("-p")
        .arg("oatsc")
        .arg("--release");
    oatsc_build.status()?;

    // Test that single files work correctly
    let mut cmd = Command::cargo_bin("toasty")?;

    let mut example = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    example = example.join("../../examples/add.oats");
    let example = example.canonicalize()?;

    cmd.current_dir(&workspace_root);

    // Set up environment variables for oatsc and runtime
    let oatsc_path = workspace_root.join("target/release/oatsc");
    let runtime_path = workspace_root.join("target/release/libruntime.a");
    cmd.env("OATS_OATSC_PATH", oatsc_path);
    cmd.env("OATS_RUNTIME_PATH", runtime_path);

    cmd.arg("--verbose").arg("build").arg(example.as_os_str());

    // Should discover just 1 module
    cmd.assert()
        .success()
        .stderr(predicate::str::contains("Dependency graph has 1 module(s)"));

    Ok(())
}
