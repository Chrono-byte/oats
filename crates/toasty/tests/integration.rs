use assert_cmd::prelude::*;
use std::path::PathBuf;
use std::process::Command;

#[test]
fn build_emits_object() -> Result<(), Box<dyn std::error::Error>> {
    // Run `toasty build --emit-object-only examples/add.oats`
    let mut cmd = Command::cargo_bin("toasty")?;
    // Compute absolute path to repository examples/add.oats
    let mut example = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    // crates/toasty -> repo root is two parents up
    example = example.join("../../examples/add.oats");
    let example = example.canonicalize()?;
    // Ensure runtime staticlib is built so builder can find libruntime.a
    let mut runc = Command::new("cargo");
    runc.current_dir(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../.."));
    runc.arg("build").arg("-p").arg("runtime").arg("--release");
    runc.status()?;
    // Run from workspace root so builder's relative paths align
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let oatsc_path = workspace_root.join("target/release/oatsc");
    cmd.env("OATS_OATSC_PATH", oatsc_path);
    cmd.current_dir(workspace_root);
    cmd.arg("build")
        .arg("--emit-object-only")
        .arg(example.as_os_str());
    cmd.assert().success();
    Ok(())
}
