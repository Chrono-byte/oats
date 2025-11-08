use anyhow::Result;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_add_example_end_to_end() -> Result<()> {
    let temp_dir = tempdir()?;
    let out_dir = temp_dir.path();

    // Build runtime crate
    let status = Command::new("cargo")
        .args(["build", "-p", "runtime", "--release"])
        .status()?;
    assert!(status.success(), "building runtime crate failed");

    // Build the `oatsc` compiler binary in release mode
    let status = Command::new("cargo")
        .args(["build", "-p", "oatsc", "--release"])
        .status()?;
    assert!(status.success(), "building oatsc failed");

    // Build the `oats_std` static library
    let status = Command::new("cargo")
        .args(["build", "-p", "oats_std", "--release"])
        .status()?;
    assert!(status.success(), "building oats_std failed");

    // Build the `oats_primitives` static library
    let status = Command::new("cargo")
        .args(["build", "-p", "oats_primitives", "--release"])
        .status()?;
    assert!(status.success(), "building oats_primitives failed");

    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .ok_or_else(|| anyhow::anyhow!("failed to find workspace root"))?;
    let example = workspace_root.join("examples").join("add.oats");
    let oatsc_path = workspace_root.join("target").join("release").join("oatsc");
    assert!(
        oatsc_path.exists(),
        "oatsc binary not found at {}",
        oatsc_path.display()
    );

    // Compile the example to object file using oatsc
    let obj_path = out_dir.join("add.o");
    let status = Command::new(&oatsc_path)
        .args([example.to_string_lossy().as_ref(), "--emit-object-only"])
        .env("OATS_OUT_DIR", out_dir)
        .status()?;
    assert!(status.success(), "oatsc failed to compile example");

    // Check that the object file was created
    assert!(obj_path.exists(), "object file not created");
    Ok(())
}

mod interfaces_types;
