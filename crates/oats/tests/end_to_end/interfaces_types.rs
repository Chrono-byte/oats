use anyhow::Result;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_interfaces_types_example_end_to_end() -> Result<()> {
    let temp_dir = tempdir()?;
    let out_dir = temp_dir.path();

    // Build runtime crate
    let status = Command::new("cargo")
        .args(["build", "-p", "runtime", "--release"])  
        .status()?;
    assert!(status.success(), "building runtime crate failed");

    // Build the `toasty` CLI binary
    let status = Command::new("cargo")
        .args(["build", "-p", "oats", "--bin", "toasty"])  
        .status()?;
    assert!(status.success(), "building toasty failed");

    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .expect("failed to find workspace root");
    // Run the `toasty` binary on the example
    let example = workspace_root
        .join("examples")
        .join("proper_tests")
        .join("interfaces_types.oats");
    let bin_path = workspace_root.join("target").join("debug").join("toasty");
    assert!(
        bin_path.exists(),
        "toasty binary not found at {}",
        bin_path.display()
    );

    let status = Command::new(&bin_path)
        .arg(example)
        .env("OATS_OUT_DIR", out_dir)
        .current_dir(workspace_root)
        .status()?;
    assert!(status.success(), "toasty failed to compile example");

    // Run the produced executable
    let exe_path = out_dir.join("interfaces_types");
    assert!(
        exe_path.exists(),
        "compiled executable not found: {}",
        exe_path.display()
    );
    let run_output = Command::new(&exe_path).output()?;
    assert!(run_output.status.success());
    let out = String::from_utf8_lossy(&run_output.stdout).to_string();
    // Check expected lines
    assert!(
        out.contains("User: Bob (id=1)"),
        "stdout did not contain expected user line: {}",
        out
    );
    assert!(
        out.contains("Location: 10, 20"),
        "stdout did not contain expected location line: {}",
        out
    );
    Ok(())
}
