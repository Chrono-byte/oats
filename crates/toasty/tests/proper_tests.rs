use assert_cmd::Command;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_proper_examples() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let repo_root = manifest_dir
        .parent()
        .expect("Failed to get parent of manifest dir")
        .parent()
        .expect("Failed to get repo root directory");
    let examples_dir = repo_root.join("examples").join("proper_tests");

    // Create a temporary output directory
    let temp_out = TempDir::new().expect("Failed to create temp dir");
    let out_dir = temp_out.path();

    // Find target directory for libraries
    // assert_cmd::cargo::cargo_bin handles finding/building the binary
    let toasty_bin = assert_cmd::cargo::cargo_bin("toasty");
    let target_dir = toasty_bin.parent().expect("Failed to get target dir");

    // The libraries are expected to be in the same target directory
    let runtime_path = target_dir.join("libruntime.a");
    let std_path = target_dir.join("liboats_std.a");
    let oatsc_path = assert_cmd::cargo::cargo_bin("oatsc");

    // Ensure we are building in the same mode (debug/release) as the test
    if !runtime_path.exists() || !std_path.exists() {
        panic!(
            "Required libraries not found in {}.\nPlease run 'cargo build' or 'cargo test --workspace' to ensure all artifacts are built.",
            target_dir.display()
        );
    }

    let entries = fs::read_dir(&examples_dir).expect("Failed to read examples directory");

    let mut files = entries
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|ext| ext == "oats"))
        .collect::<Vec<_>>();

    // Sort files for deterministic test order
    files.sort();

    assert!(
        !files.is_empty(),
        "No .oats files found in {}",
        examples_dir.display()
    );

    let mut failures = Vec::new();

    for path in files {
        let mut cmd = Command::cargo_bin("toasty")
            .expect("Failed to find toasty binary. Run 'cargo build' first.");
        cmd.arg("build")
            .arg("--color")
            .arg("never")
            .arg(&path)
            .env("OATS_OUT_DIR", out_dir)
            .env("OATS_RUNTIME_PATH", &runtime_path)
            .env("OATS_STD_PATH", &std_path)
            .env("OATS_OATSC_PATH", &oatsc_path);

        let output = cmd.output().expect("Failed to execute toasty");

        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        // Strip ANSI codes if any remained (though we passed --color never)
        let clean_stdout = strip_ansi_codes(&stdout);
        let clean_stderr = strip_ansi_codes(&stderr);

        if !output.status.success()
            || clean_stdout.contains("error:")
            || clean_stderr.contains("error:")
            || clean_stdout.contains("error[")
            || clean_stderr.contains("error[")
        {
            failures.push(format!(
                "Failed to compile {}:\n--- STDOUT ---\n{}\n--- STDERR ---\n{}",
                path.display(),
                stdout,
                stderr
            ));
        }
    }

    if !failures.is_empty() {
        let count = failures.len();
        for failure in failures {
            eprintln!("{}", failure);
            eprintln!("------------------------------------------------------------");
        }
        panic!("{} proper tests failed", count);
    }
}

fn strip_ansi_codes(s: &str) -> String {
    // Simple regex-based stripper for ANSI escape codes
    let re = regex::Regex::new(r"\x1b\[[0-9;]*[A-Za-z]")
        .expect("Failed to compile ANSI escape code regex pattern");
    re.replace_all(s, "").to_string()
}
