use std::env;
use std::path::PathBuf;
use std::process::Command;

// Run `toasty build` and assert debug lines appear only when verbose or env var
#[test]
fn verbose_flag_and_env_control_output() -> Result<(), Box<dyn std::error::Error>> {
    let mut example = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    example = example.join("../../examples/add.oats");
    let example = example.canonicalize()?;

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
    let oatsc_path = workspace_root.join("target/release/oatsc");
    let runtime_path = workspace_root.join("target/release/libruntime.a");

    // 1) Without verbose: should NOT contain clang version or Link command
    let mut cmd1 = Command::new(&toasty_bin);
    cmd1.current_dir(&workspace_root);
    cmd1.env("OATS_OATSC_PATH", &oatsc_path);
    cmd1.env("OATS_RUNTIME_PATH", &runtime_path);
    cmd1.arg("build").arg(example.as_os_str());
    let out1 = cmd1.output()?;
    let stderr1 = String::from_utf8_lossy(&out1.stderr);
    assert!(!stderr1.contains("Link command"));
    assert!(!stderr1.contains("Compilation order:"));

    // 2) With --verbose: should contain Link command or Compilation order
    let mut cmd2 = Command::new(&toasty_bin);
    cmd2.current_dir(&workspace_root);
    cmd2.env("OATS_OATSC_PATH", &oatsc_path);
    cmd2.env("OATS_RUNTIME_PATH", &runtime_path);
    cmd2.arg("--verbose").arg("build").arg(example.as_os_str());
    let out2 = cmd2.output()?;
    let stderr2 = String::from_utf8_lossy(&out2.stderr);
    assert!(stderr2.contains("Link command") || stderr2.contains("Compilation order"));

    // 3) With TOASTY_VERBOSE env var set: should also be verbose
    let mut cmd3 = Command::new(&toasty_bin);
    cmd3.current_dir(&workspace_root);
    cmd3.env("OATS_OATSC_PATH", &oatsc_path);
    cmd3.env("OATS_RUNTIME_PATH", &runtime_path);
    cmd3.env("TOASTY_VERBOSE", "1");
    cmd3.arg("build").arg(example.as_os_str());
    let out3 = cmd3.output()?;
    let stderr3 = String::from_utf8_lossy(&out3.stderr);
    assert!(stderr3.contains("Link command") || stderr3.contains("Compilation order"));

    Ok(())
}
