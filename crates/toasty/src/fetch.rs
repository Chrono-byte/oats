//! Dependency fetching module
//!
//! This module provides functionality to fetch pre-built compiler binaries
//! and runtime libraries from GitHub releases, enabling toasty to work
//! standalone without requiring the full oats repository or Rust toolchain.

use crate::diagnostics::{Result, ToastyError};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

/// GitHub repository information
const GITHUB_OWNER: &str = "Chrono-byte";
const GITHUB_REPO: &str = "oats";

/// Compute SHA-256 hash of a file
fn compute_sha256(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)?;
    let mut hasher = Sha256::new();
    std::io::copy(&mut file, &mut hasher)?;
    let hash = hasher.finalize();
    Ok(format!("{:x}", hash))
}

/// Get the oats home directory for binaries and libraries
fn get_oats_home() -> Result<PathBuf> {
    let oats_home = if let Ok(dir) = std::env::var("OATS_HOME") {
        PathBuf::from(dir)
    } else if let Some(home) = dirs::home_dir() {
        home.join(".oats")
    } else {
        // Fallback to temp directory
        std::env::temp_dir().join("oats_home")
    };

    fs::create_dir_all(&oats_home)?;
    Ok(oats_home)
}

/// Detect the current platform and return the appropriate compiler artifact name
fn get_compiler_artifact_name() -> &'static str {
    let os = std::env::consts::OS;
    let arch = std::env::consts::ARCH;

    match (os, arch) {
        ("linux", "x86_64") => "oatsc-linux-x86_64",
        ("linux", "aarch64") => "oatsc-linux-aarch64",
        ("macos", "x86_64") => "oatsc-macos-x86_64",
        ("macos", "aarch64") => "oatsc-macos-aarch64",
        ("windows", "x86_64") => "oatsc-windows-x86_64.exe",
        _ => {
            eprintln!(
                "Warning: Unsupported platform {}-{}, will build locally",
                os, arch
            );
            "oatsc-unsupported"
        }
    }
}

/// Detect the current platform and return the appropriate runtime artifact name
fn get_runtime_artifact_name() -> &'static str {
    let os = std::env::consts::OS;
    let arch = std::env::consts::ARCH;

    match (os, arch) {
        ("linux", "x86_64") => "libruntime-linux-x86_64.a",
        ("linux", "aarch64") => "libruntime-linux-aarch64.a",
        ("macos", "x86_64") => "libruntime-macos-x86_64.a",
        ("macos", "aarch64") => "libruntime-macos-aarch64.a",
        _ => {
            eprintln!(
                "Warning: Unsupported platform {}-{}, will build locally",
                os, arch
            );
            "libruntime-unsupported.a"
        }
    }
}

/// Fetch the latest compiler release tag from GitHub
fn get_latest_compiler_tag() -> Result<String> {
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases",
        GITHUB_OWNER, GITHUB_REPO
    );

    let response = ureq::get(&url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch releases: {}", e)))?;

    let releases: serde_json::Value = serde_json::from_reader(response.into_reader())
        .map_err(|e| ToastyError::other(format!("Failed to parse releases JSON: {}", e)))?;

    // Find the latest compiler release (tagged as nightly or stable-*)
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str()
                && (tag == "nightly" || tag.starts_with("stable-"))
            {
                return Ok(tag.to_string());
            }
        }
    }

    Err(ToastyError::other("No compiler releases found"))
}

/// Fetch the latest runtime release tag from GitHub
fn get_latest_runtime_tag() -> Result<String> {
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases",
        GITHUB_OWNER, GITHUB_REPO
    );

    let response = ureq::get(&url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to parse releases JSON: {}", e)))?;

    let releases: serde_json::Value = serde_json::from_reader(response.into_reader())
        .map_err(|e| ToastyError::other(format!("Failed to parse releases JSON: {}", e)))?;

    // Find the latest runtime release (tagged as runtime-*)
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str()
                && tag.starts_with("runtime-")
            {
                return Ok(tag.to_string());
            }
        }
    }

    Err(ToastyError::other("No runtime releases found"))
}

/// Download compiler binary from GitHub release
fn download_compiler(tag: &str, artifact_name: &str, dest_path: &Path) -> Result<()> {
    let url = format!(
        "https://github.com/{}/{}/releases/download/{}/{}",
        GITHUB_OWNER, GITHUB_REPO, tag, artifact_name
    );

    let verbose = std::env::var("TOASTY_VERBOSE").is_ok();
    if verbose {
        eprintln!("Downloading compiler from: {}", url);
    } else {
        eprintln!("Downloading pre-built compiler binary...");
    }

    let response = ureq::get(&url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to download compiler: {}", e)))?;

    let mut file = fs::File::create(dest_path)?;
    let bytes_written = std::io::copy(&mut response.into_reader(), &mut file)?;

    // Make the binary executable on Unix systems
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = file.metadata()?.permissions();
        perms.set_mode(0o755);
        fs::set_permissions(dest_path, perms)?;
    }

    // Fetch digest from GitHub API
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/releases/tags/{}",
        GITHUB_OWNER, GITHUB_REPO, tag
    );
    let release_resp = ureq::get(&api_url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch release metadata: {}", e)))?;
    let release_json: serde_json::Value = serde_json::from_reader(release_resp.into_reader())
        .map_err(|e| ToastyError::other(format!("Failed to parse release metadata: {}", e)))?;
    let mut expected_hash = None;
    if let Some(assets) = release_json.get("assets").and_then(|a| a.as_array()) {
        for asset in assets {
            if asset.get("name").and_then(|n| n.as_str()) == Some(artifact_name)
                && let Some(digest) = asset.get("digest").and_then(|d| d.as_str())
                    && let Some(hash) = digest.strip_prefix("sha256:") {
                        expected_hash = Some(hash.to_string());
                    }
        }
    }
    let computed_hash = compute_sha256(dest_path)?;
    if let Some(expected_hash) = expected_hash {
        if computed_hash != expected_hash {
            eprintln!(
                "Warning: Hash mismatch for compiler asset (expected {}, got {}). Marking as unusable.",
                expected_hash, computed_hash
            );
            // Move unusable asset to quarantine folder
            let quarantine_dir = get_oats_home()?.join("quarantine");
            fs::create_dir_all(&quarantine_dir)?;
            let quarantine_path = quarantine_dir.join(artifact_name);
            fs::rename(dest_path, &quarantine_path)?;
            eprintln!(
                "Unusable compiler asset moved to quarantine: {}",
                quarantine_path.display()
            );
            return Err(ToastyError::HashMismatch {
                name: artifact_name.to_string(),
                expected: expected_hash,
                actual: computed_hash,
            });
        }
    } else {
        eprintln!("Warning: No digest found for compiler asset. Skipping hash verification.");
    }
    if verbose {
        eprintln!("Downloaded {} bytes", bytes_written);
    }
    Ok(())
}

/// Download runtime library from GitHub release
fn download_runtime(tag: &str, artifact_name: &str, dest_path: &Path) -> Result<()> {
    let url = format!(
        "https://github.com/{}/{}/releases/download/{}/{}",
        GITHUB_OWNER, GITHUB_REPO, tag, artifact_name
    );

    let verbose = std::env::var("TOASTY_VERBOSE").is_ok();
    if verbose {
        eprintln!("Downloading runtime from: {}", url);
    } else {
        eprintln!("Downloading pre-built runtime library...");
    }

    let response = ureq::get(&url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to download runtime: {}", e)))?;

    let mut file = fs::File::create(dest_path)?;
    let bytes_written = std::io::copy(&mut response.into_reader(), &mut file)?;

    // Fetch digest from GitHub API
    let api_url = format!(
        "https://api.github.com/repos/{}/{}/releases/tags/{}",
        GITHUB_OWNER, GITHUB_REPO, tag
    );
    let release_resp = ureq::get(&api_url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch release metadata: {}", e)))?;
    let release_json: serde_json::Value = serde_json::from_reader(release_resp.into_reader())
        .map_err(|e| ToastyError::other(format!("Failed to parse release metadata: {}", e)))?;
    let mut expected_hash = None;
    if let Some(assets) = release_json.get("assets").and_then(|a| a.as_array()) {
        for asset in assets {
            if asset.get("name").and_then(|n| n.as_str()) == Some(artifact_name)
                && let Some(digest) = asset.get("digest").and_then(|d| d.as_str())
                    && let Some(hash) = digest.strip_prefix("sha256:") {
                        expected_hash = Some(hash.to_string());
                    }
        }
    }
    let computed_hash = compute_sha256(dest_path)?;
    if let Some(expected_hash) = expected_hash {
        if computed_hash != expected_hash {
            eprintln!(
                "Warning: Hash mismatch for runtime asset (expected {}, got {}). Marking as unusable.",
                expected_hash, computed_hash
            );
            // Move unusable asset to quarantine folder
            let quarantine_dir = get_oats_home()?.join("quarantine");
            fs::create_dir_all(&quarantine_dir)?;
            let quarantine_path = quarantine_dir.join(artifact_name);
            fs::rename(dest_path, &quarantine_path)?;
            eprintln!(
                "Unusable runtime asset moved to quarantine: {}",
                quarantine_path.display()
            );
            return Err(ToastyError::HashMismatch {
                name: artifact_name.to_string(),
                expected: expected_hash,
                actual: computed_hash,
            });
        }
    } else {
        eprintln!("Warning: No digest found for runtime asset. Skipping hash verification.");
    }
    if verbose {
        eprintln!("Downloaded {} bytes", bytes_written);
    }
    Ok(())
}

/// Try to fetch pre-built compiler from GitHub releases
pub fn try_fetch_compiler() -> Option<PathBuf> {
    let verbose = std::env::var("TOASTY_VERBOSE").is_ok();

    // Check if we should skip remote fetch
    if std::env::var("OATS_NO_REMOTE_COMPILER").is_ok() {
        if verbose {
            eprintln!("Skipping remote compiler fetch (OATS_NO_REMOTE_COMPILER is set)");
        }
        return None;
    }

    let artifact_name = get_compiler_artifact_name();
    if artifact_name == "oatsc-unsupported" {
        if verbose {
            eprintln!("Platform not supported for pre-built compiler, will use local build");
        }
        return None; // Unsupported platform, use local
    }

    if verbose {
        eprintln!(
            "Attempting to fetch pre-built compiler for {}",
            artifact_name
        );
    }

    // Get cache directory
    let oats_home = match get_oats_home() {
        Ok(dir) => {
            if verbose {
                eprintln!("Compiler cache directory: {}", dir.display());
            }
            dir
        }
        Err(e) => {
            eprintln!("Warning: Failed to get cache directory: {}", e);
            return None;
        }
    };

    // Get latest compiler tag
    let tag = match get_latest_compiler_tag() {
        Ok(t) => {
            if verbose {
                eprintln!("Latest compiler release: {}", t);
            }
            t
        }
        Err(e) => {
            if verbose {
                eprintln!("Warning: Failed to fetch latest compiler tag: {}", e);
            }
            return None;
        }
    };

    // Check if we already have this version cached
    let cached_path = oats_home.join(&tag).join(artifact_name);
    if cached_path.exists() {
        eprintln!("Using cached compiler: {}", cached_path.display());
        return Some(cached_path);
    }

    // Download the compiler
    if verbose {
        eprintln!("Cache miss, downloading compiler...");
    }
    if let Some(parent) = cached_path.parent() {
        fs::create_dir_all(parent).ok();
    }
    match download_compiler(&tag, artifact_name, &cached_path) {
        Ok(_) => {
            eprintln!(
                "Successfully downloaded compiler to: {}",
                cached_path.display()
            );
            Some(cached_path)
        }
        Err(e) => {
            eprintln!("Warning: Failed to download compiler: {}", e);
            None
        }
    }
}

/// Try to fetch pre-built runtime from GitHub releases
pub fn try_fetch_runtime() -> Option<PathBuf> {
    let verbose = std::env::var("TOASTY_VERBOSE").is_ok();

    // Check if we should skip remote fetch
    if std::env::var("OATS_NO_REMOTE_RUNTIME").is_ok() {
        if verbose {
            eprintln!("Skipping remote runtime fetch (OATS_NO_REMOTE_RUNTIME is set)");
        }
        return None;
    }

    let artifact_name = get_runtime_artifact_name();
    if artifact_name == "libruntime-unsupported.a" {
        if verbose {
            eprintln!("Platform not supported for pre-built runtime, will build locally");
        }
        return None; // Unsupported platform, build locally
    }

    if verbose {
        eprintln!(
            "Attempting to fetch pre-built runtime for {}",
            artifact_name
        );
    }

    // Get cache directory
    let oats_home = match get_oats_home() {
        Ok(dir) => {
            if verbose {
                eprintln!("Runtime cache directory: {}", dir.display());
            }
            dir
        }
        Err(e) => {
            eprintln!("Warning: Failed to get cache directory: {}", e);
            return None;
        }
    };

    // Get latest runtime tag
    let tag = match get_latest_runtime_tag() {
        Ok(t) => {
            if verbose {
                eprintln!("Latest runtime release: {}", t);
            }
            t
        }
        Err(e) => {
            if verbose {
                eprintln!("Warning: Failed to fetch latest runtime tag: {}", e);
            }
            return None;
        }
    };

    // Check if we already have this version cached
    let cached_path = oats_home.join(&tag).join(artifact_name);
    if cached_path.exists() {
        eprintln!("Using cached runtime: {}", cached_path.display());
        return Some(cached_path);
    }

    // Download the runtime
    if verbose {
        eprintln!("Cache miss, downloading runtime...");
    }
    if let Some(parent) = cached_path.parent() {
        fs::create_dir_all(parent).ok();
    }
    match download_runtime(&tag, artifact_name, &cached_path) {
        Ok(_) => {
            eprintln!(
                "Successfully downloaded runtime to: {}",
                cached_path.display()
            );
            Some(cached_path)
        }
        Err(e) => {
            eprintln!("Warning: Failed to download runtime: {}", e);
            None
        }
    }
}

/// List all available compiler versions from GitHub releases
pub fn list_available_compilers() -> Result<Vec<String>> {
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases",
        GITHUB_OWNER, GITHUB_REPO
    );

    let response = ureq::get(&url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch releases: {}", e)))?;

    let releases: serde_json::Value = serde_json::from_reader(response.into_reader())
        .map_err(|e| ToastyError::other(format!("Failed to parse releases JSON: {}", e)))?;

    let mut versions = Vec::new();
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str()
                && (tag == "nightly" || tag.starts_with("stable-"))
            {
                versions.push(tag.to_string());
            }
        }
    }

    Ok(versions)
}

/// List all available runtime versions from GitHub releases
pub fn list_available_runtimes() -> Result<Vec<String>> {
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases",
        GITHUB_OWNER, GITHUB_REPO
    );

    let response = ureq::get(&url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch releases: {}", e)))?;

    let releases: serde_json::Value = serde_json::from_reader(response.into_reader())
        .map_err(|e| ToastyError::other(format!("Failed to parse releases JSON: {}", e)))?;

    let mut versions = Vec::new();
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str()
                && tag.starts_with("runtime-")
            {
                versions.push(tag.to_string());
            }
        }
    }

    Ok(versions)
}

/// Install a specific compiler version
pub fn install_compiler_version(version: &str) -> Result<()> {
    let artifact_name = get_compiler_artifact_name();
    if artifact_name == "oatsc-unsupported" {
        return Err(ToastyError::other(
            "Unsupported platform for pre-built compiler",
        ));
    }

    let oats_home = get_oats_home()?;
    let cached_path = oats_home.join(version).join(artifact_name);

    // Check if already installed
    if cached_path.exists() {
        eprintln!("Compiler version {} is already installed.", version);
        return Ok(());
    }

    // Download the compiler
    if let Some(parent) = cached_path.parent() {
        fs::create_dir_all(parent)?;
    }
    download_compiler(version, artifact_name, &cached_path)?;

    eprintln!("Successfully installed compiler version {}", version);
    Ok(())
}

/// Install a specific runtime version
pub fn install_runtime_version(version: &str) -> Result<()> {
    let artifact_name = get_runtime_artifact_name();
    if artifact_name == "libruntime-unsupported.a" {
        return Err(ToastyError::other(
            "Unsupported platform for pre-built runtime",
        ));
    }

    let oats_home = get_oats_home()?;
    let cached_path = oats_home.join(version).join(artifact_name);

    // Check if already installed
    if cached_path.exists() {
        eprintln!("Runtime version {} is already installed.", version);
        return Ok(());
    }

    // Download the runtime
    if let Some(parent) = cached_path.parent() {
        fs::create_dir_all(parent)?;
    }
    download_runtime(version, artifact_name, &cached_path)?;

    eprintln!("Successfully installed runtime version {}", version);
    Ok(())
}

/// Switch to using a specific compiler version
pub fn use_compiler_version(version: &str) -> Result<()> {
    let artifact_name = get_compiler_artifact_name();
    if artifact_name == "oatsc-unsupported" {
        return Err(ToastyError::other(
            "Unsupported platform for pre-built compiler",
        ));
    }

    let oats_home = get_oats_home()?;
    let compiler_path = oats_home.join(version).join(artifact_name);

    if !compiler_path.exists() {
        return Err(ToastyError::other(format!(
            "Compiler version {} is not installed. Install it first with 'toasty compiler install {}'",
            version, version
        )));
    }

    // Store the selected version in a config file
    let config_path = oats_home.join("current_version");
    fs::write(&config_path, version)?;

    eprintln!("Switched to compiler version {}", version);
    Ok(())
}

/// Switch to using a specific runtime version
pub fn use_runtime_version(version: &str) -> Result<()> {
    let artifact_name = get_runtime_artifact_name();
    if artifact_name == "libruntime-unsupported.a" {
        return Err(ToastyError::other(
            "Unsupported platform for pre-built runtime",
        ));
    }

    let oats_home = get_oats_home()?;
    let runtime_path = oats_home.join(version).join(artifact_name);

    if !runtime_path.exists() {
        return Err(ToastyError::other(format!(
            "Runtime version {} is not installed. Install it first with 'toasty runtime install {}'",
            version, version
        )));
    }

    // Store the selected version in a config file
    let config_path = oats_home.join("current_runtime_version");
    fs::write(&config_path, version)?;

    eprintln!("Switched to runtime version {}", version);
    Ok(())
}

/// Get the currently selected compiler version
pub fn current_compiler_version() -> Result<String> {
    let oats_home = get_oats_home()?;
    let config_path = oats_home.join("current_version");

    if config_path.exists() {
        let version = fs::read_to_string(&config_path)?;
        Ok(version.trim().to_string())
    } else {
        // If no version is selected, try to get the latest
        match get_latest_compiler_tag() {
            Ok(tag) => Ok(format!("{} (latest)", tag)),
            Err(_) => Ok("none selected".to_string()),
        }
    }
}

/// Get the currently selected runtime version
pub fn current_runtime_version() -> Result<String> {
    let oats_home = get_oats_home()?;
    let config_path = oats_home.join("current_runtime_version");

    if config_path.exists() {
        let version = fs::read_to_string(&config_path)?;
        Ok(version.trim().to_string())
    } else {
        // If no version is selected, try to get the latest
        match get_latest_runtime_tag() {
            Ok(tag) => Ok(format!("{} (latest)", tag)),
            Err(_) => Ok("none selected".to_string()),
        }
    }
}

/// Get the path to the currently selected compiler
pub fn get_selected_compiler_path() -> Option<PathBuf> {
    let oats_home = match get_oats_home() {
        Ok(dir) => dir,
        Err(_) => return None,
    };

    let config_path = oats_home.join("current_version");
    let version = match fs::read_to_string(&config_path) {
        Ok(v) => v.trim().to_string(),
        Err(_) => return None,
    };

    let artifact_name = get_compiler_artifact_name();
    if artifact_name == "oatsc-unsupported" {
        return None;
    }

    let compiler_path = oats_home.join(version).join(artifact_name);
    if compiler_path.exists() {
        Some(compiler_path)
    } else {
        None
    }
}

/// Get the path to the currently selected runtime
pub fn get_selected_runtime_path() -> Option<PathBuf> {
    let oats_home = match get_oats_home() {
        Ok(dir) => dir,
        Err(_) => return None,
    };

    let config_path = oats_home.join("current_runtime_version");
    let version = match fs::read_to_string(&config_path) {
        Ok(v) => v.trim().to_string(),
        Err(_) => return None,
    };

    let artifact_name = get_runtime_artifact_name();
    if artifact_name == "libruntime-unsupported.a" {
        return None;
    }

    let runtime_path = oats_home.join(version).join(artifact_name);
    if runtime_path.exists() {
        Some(runtime_path)
    } else {
        None
    }
}

/// Uninstall a specific compiler version
pub fn uninstall_compiler_version(version: &str) -> Result<()> {
    let artifact_name = get_compiler_artifact_name();
    if artifact_name == "oatsc-unsupported" {
        return Err(ToastyError::other(
            "Unsupported platform for pre-built compiler",
        ));
    }

    let oats_home = get_oats_home()?;
    let compiler_path = oats_home.join(version).join(artifact_name);

    // Check if installed
    if !compiler_path.exists() {
        eprintln!("Compiler version {} is not installed.", version);
        return Ok(());
    }

    // Remove the compiler binary
    fs::remove_file(&compiler_path)?;

    // Remove the version directory if it's empty
    if let Some(version_dir) = compiler_path.parent()
        && version_dir.read_dir()?.next().is_none()
    {
        fs::remove_dir(version_dir)?;
    }

    // If this was the currently selected version, clear the selection
    let config_path = oats_home.join("current_version");
    if config_path.exists() {
        let current = fs::read_to_string(&config_path)?;
        if current.trim() == version {
            fs::remove_file(&config_path)?;
            eprintln!(
                "Cleared current version selection since {} was uninstalled.",
                version
            );
        }
    }

    eprintln!("Successfully uninstalled compiler version {}", version);
    Ok(())
}

/// Uninstall a specific runtime version
pub fn uninstall_runtime_version(version: &str) -> Result<()> {
    let artifact_name = get_runtime_artifact_name();
    if artifact_name == "libruntime-unsupported.a" {
        return Err(ToastyError::other(
            "Unsupported platform for pre-built runtime",
        ));
    }

    let oats_home = get_oats_home()?;
    let runtime_path = oats_home.join(version).join(artifact_name);

    // Check if installed
    if !runtime_path.exists() {
        eprintln!("Runtime version {} is not installed.", version);
        return Ok(());
    }

    // Remove the runtime library
    fs::remove_file(&runtime_path)?;

    // Remove the version directory if it's empty
    if let Some(version_dir) = runtime_path.parent()
        && version_dir.read_dir()?.next().is_none()
    {
        fs::remove_dir(version_dir)?;
    }

    // If this was the currently selected version, clear the selection
    let config_path = oats_home.join("current_runtime_version");
    if config_path.exists() {
        let current = fs::read_to_string(&config_path)?;
        if current.trim() == version {
            fs::remove_file(&config_path)?;
            eprintln!(
                "Cleared current runtime version selection since {} was uninstalled.",
                version
            );
        }
    }

    eprintln!("Successfully uninstalled runtime version {}", version);
    Ok(())
}
