use anyhow::Result;
use std::process::Command;
use tempfile::tempdir;

#[test]
fn test_add_example_end_to_end() -> Result<()> {
    let temp_dir = tempdir()?;
    let out_dir = temp_dir.path();

    // Invoke the aot_run runner via `cargo run` to produce the executable in out_dir
    // Ensure runtime staticlib is built (aot_run links against it)
    let status = Command::new("cargo")
        .args(["build", "-p", "runtime", "--release"])
        .status()?;
    assert!(status.success(), "building runtime crate failed");

    // Build the CLI binary (prefer `toasty`, keep `aot_run` for compatibility)
    let status = Command::new("cargo")
        .args(["build", "-p", "oats", "--bin", "toasty"])
        .status();
    if let Ok(s) = status {
        assert!(s.success(), "building toasty failed");
    } else {
        // Fallback to building legacy binary
        let status = Command::new("cargo")
            .args(["build", "-p", "oats", "--bin", "aot_run"])
            .status()?;
        assert!(status.success(), "building aot_run failed");
    }

    // Resolve workspace root from this crate's manifest dir and locate target/debug/aot_run there
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    // CARGO_MANIFEST_DIR for this crate is .../crates/oats; workspace root is two levels up
    let workspace_root = manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .expect("failed to find workspace root");
    let example = workspace_root.join("examples").join("add.oats");
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
    let exe_path = out_dir.join("add");
    assert!(exe_path.exists(), "compiled executable not found");
    let run_output = Command::new(&exe_path).output()?;
    assert!(run_output.status.success());
    let out = String::from_utf8_lossy(&run_output.stdout)
        .trim()
        .to_string();
    let v: f64 = out.parse()?;
    assert!((v - 8.0).abs() < 1e-6, "expected 8.0 output, got {}", v);
    Ok(())
}

mod interfaces_types;
