use std::process::Command;
use std::time::{Duration, Instant};
use std::{env, fs, path::PathBuf, thread};

#[test]
fn aot_cycle_reclaim_runs_and_exits() {
    // Determine repository root (two parents above this crate)
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let repo_root = PathBuf::from(manifest_dir)
        .parent()
        .and_then(|p| p.parent())
        .expect("repository root")
        .to_path_buf();

    // Build the aot_run binary to ensure it's available
    let status = Command::new("cargo")
        .current_dir(&repo_root)
        // Prefer building the new `toasty` binary (keeps legacy aot_run as fallback)
        .args(&["build", "-p", "oats", "--bin", "toasty"])
        .status()
        .expect("failed to spawn cargo build");
    assert!(status.success(), "cargo build -p oats failed");

    // Create a unique output directory for this test
    let out_dir = repo_root.join(format!("aot_out_test_{}", std::process::id()));
    if out_dir.exists() {
        let _ = fs::remove_dir_all(&out_dir);
    }
    fs::create_dir_all(&out_dir).expect("failed to create out dir");

    // Path to the built toasty binary
    let toasty_bin = repo_root.join("target/debug/toasty");
    assert!(
        toasty_bin.exists(),
        "toasty binary not found at {:?}",
        toasty_bin
    );
    let runner = toasty_bin;

    // Run aot_run to compile the example into the out_dir
    let example = repo_root.join("examples/cycle_reclaim.oats");
    let mut aot_cmd = Command::new(runner);
    aot_cmd
        .current_dir(&repo_root)
        .arg(example)
        .env("OATS_OUT_DIR", &out_dir);

    let status = aot_cmd.status().expect("failed to run aot_run");
    assert!(status.success(), "aot_run failed to produce AOT output");

    // Run the produced binary
    let produced = out_dir.join("cycle_reclaim");
    assert!(
        produced.exists(),
        "produced binary not found: {:?}",
        produced
    );

    let mut child = Command::new(&produced)
        .current_dir(&repo_root)
        .spawn()
        .expect("failed to spawn produced binary");

    // Wait up to 6 seconds for the process to exit
    let timeout = Duration::from_secs(6);
    let start = Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                assert!(status.success(), "produced binary exited with failure");
                break;
            }
            Ok(None) => {
                if start.elapsed() > timeout {
                    // timed out; kill and fail
                    let _ = child.kill();
                    panic!("produced binary did not exit within timeout");
                }
                thread::sleep(Duration::from_millis(100));
            }
            Err(e) => panic!("error waiting for child: {}", e),
        }
    }

    // cleanup (best-effort)
    let _ = fs::remove_dir_all(&out_dir);
}
