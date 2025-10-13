use std::env;
use std::path::PathBuf;
use std::process::Command;

// Run `toasty build` and assert debug lines appear only when verbose or env var
#[test]
fn verbose_flag_and_env_control_output() -> Result<(), Box<dyn std::error::Error>> {
    let mut example = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    example = example.join("../../examples/add.oats");
    let example = example.canonicalize()?;

    // Build runtime first
    let mut runc = Command::new("cargo");
    runc.current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../.."));
    runc.arg("build").arg("-p").arg("runtime").arg("--release");
    runc.status()?;

    // 1) Without verbose: should NOT contain clang version or Invoking builder
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    // Ensure release binary exists
    let mut build_cmd = Command::new("cargo");
    build_cmd.current_dir(&workspace_root);
    build_cmd
        .arg("build")
        .arg("-p")
        .arg("toasty")
        .arg("--release");
    build_cmd.status()?;
    let toasty_bin = workspace_root.join("target/release/toasty");
    let mut cmd1 = Command::new(&toasty_bin);
    cmd1.current_dir(&workspace_root);
    cmd1.arg("build").arg(example.as_os_str());
    let out1 = cmd1.output()?;
    let stdout1 = String::from_utf8_lossy(&out1.stdout);
    assert!(!stdout1.contains("clang version"));
    assert!(!stdout1.contains("Invoking builder"));

    // 2) With --verbose: should contain Invoking builder or clang version
    let mut cmd2 = Command::new(&toasty_bin);
    cmd2.current_dir(&workspace_root);
    cmd2.arg("--verbose").arg("build").arg(example.as_os_str());
    let out2 = cmd2.output()?;
    let stdout2 = String::from_utf8_lossy(&out2.stdout);
    assert!(stdout2.contains("Invoking builder") || stdout2.contains("clang version"));

    // 3) With TOASTY_VERBOSE env var set: should also be verbose
    let mut cmd3 = Command::new(&toasty_bin);
    cmd3.current_dir(&workspace_root);
    cmd3.env("TOASTY_VERBOSE", "1");
    cmd3.arg("build").arg(example.as_os_str());
    let out3 = cmd3.output()?;
    let stdout3 = String::from_utf8_lossy(&out3.stdout);
    assert!(stdout3.contains("Invoking builder") || stdout3.contains("clang version"));

    Ok(())
}
