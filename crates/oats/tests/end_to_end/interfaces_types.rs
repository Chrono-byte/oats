use anyhow::Result;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_interfaces_types_example_end_to_end() -> Result<()> {
    let temp_dir = tempdir()?;
    let out_dir = temp_dir.path();

    // Build runtime and aot_run
    let status = Command::new("cargo")
        .args(["build", "-p", "runtime", "--release"])
        .status()?;
    assert!(status.success(), "building runtime crate failed");

    // Prefer building `toasty` CLI, fall back to `aot_run` for compatibility
    let status = Command::new("cargo")
        .args(["build", "-p", "oats", "--bin", "toasty"])
        .status();
    if let Ok(s) = status {
        assert!(s.success(), "building toasty failed");
    } else {
        let status = Command::new("cargo")
            .args(["build", "-p", "oats", "--bin", "toasty"]) // prefer toasty
            .status()?;
        assert!(status.success(), "building aot_run failed");
    }

    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let workspace_root = manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .expect("failed to find workspace root");
    let example = workspace_root
        .join("examples")
        .join("proper_tests")
        .join("interfaces_types.oats");
    let toasty_bin = workspace_root.join("target").join("debug").join("toasty");
    let aot_bin = workspace_root.join("target").join("debug").join("aot_run");
    let bin_path = if toasty_bin.exists() {
        toasty_bin
    } else {
        aot_bin
    };
    assert!(
        bin_path.exists(),
        "toasty/aot_run binary not found at {}",
        bin_path.display()
    );

    let mut cmd = Command::new(bin_path);
    cmd.arg(example)
        .env("OATS_OUT_DIR", out_dir)
        .current_dir(workspace_root);
    let compile_status = cmd.status()?;
    assert!(
        compile_status.success(),
        "aot_run failed to compile example"
    );

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
