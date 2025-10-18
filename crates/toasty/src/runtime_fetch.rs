/// Runtime and compiler fetcher module - downloads pre-built runtime libraries and oatsc compiler from GitHub releases
///
/// This module provides functionality to fetch pre-built runtime static libraries
/// and oatsc compiler binaries from GitHub releases, enabling toasty to work
/// standalone without requiring the full oats repository or Rust toolchain.
use anyhow::Result;
use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

/// GitHub repository information
const GITHUB_OWNER: &str = "Chrono-byte";
const GITHUB_REPO: &str = "oats";

/// Get the cache directory for runtime libraries and compiler
fn get_cache_dir() -> Result<PathBuf> {
    let cache_dir = if let Ok(dir) = std::env::var("OATS_CACHE_DIR") {
        PathBuf::from(dir)
    } else if let Some(home_dir) = dirs::home_dir() {
        home_dir.join(".oats")
    } else {
        // Fallback to temp directory
        std::env::temp_dir().join("oats_cache")
    };

    fs::create_dir_all(&cache_dir)?;
    Ok(cache_dir)
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

/// Detect the current platform and return the appropriate oatsc binary name
fn get_oatsc_artifact_name() -> &'static str {
    let os = std::env::consts::OS;
    let arch = std::env::consts::ARCH;

    match (os, arch) {
        ("linux", "x86_64") => "oatsc-linux-x86_64",
        ("linux", "aarch64") => "oatsc-linux-aarch64",
        ("macos", "x86_64") => "oatsc-macos-x86_64",
        ("macos", "aarch64") => "oatsc-macos-aarch64",
        _ => {
            eprintln!(
                "Warning: Unsupported platform {}-{} for oatsc binary",
                os, arch
            );
            "oatsc-unsupported"
        }
    }
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
        .map_err(|e| anyhow::anyhow!("Failed to fetch releases: {}", e))?;

    let releases: serde_json::Value = serde_json::from_reader(response.into_reader())?;

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

    anyhow::bail!("No runtime releases found")
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
        .map_err(|e| anyhow::anyhow!("Failed to download runtime: {}", e))?;

    let mut file = fs::File::create(dest_path)?;
    let bytes_written = std::io::copy(&mut response.into_reader(), &mut file)?;

    if verbose {
        eprintln!("Downloaded {} bytes", bytes_written);
    }

    Ok(())
}

/// Try to fetch pre-built runtime from GitHub releases
///
/// Returns the path to the runtime library if successful.
/// If fetching fails, returns None and caller should fall back to local build.
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
    let cache_dir = match get_cache_dir() {
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
    let cached_path = cache_dir.join(&tag).join(artifact_name);
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

/// Try to fetch pre-built oatsc compiler from GitHub releases
///
/// Returns the path to the oatsc compiler if successful.
/// If fetching fails, returns None and caller should fall back to system oatsc.
pub fn try_fetch_oatsc() -> Option<PathBuf> {
    let verbose = std::env::var("TOASTY_VERBOSE").is_ok();

    // Check if we should skip remote fetch
    if std::env::var("OATS_NO_REMOTE_COMPILER").is_ok() {
        if verbose {
            eprintln!("Skipping remote compiler fetch (OATS_NO_REMOTE_COMPILER is set)");
        }
        return None;
    }

    let artifact_name = get_oatsc_artifact_name();
    if artifact_name == "oatsc-unsupported" {
        if verbose {
            eprintln!("Platform not supported for pre-built oatsc compiler");
        }
        return None;
    }

    if verbose {
        eprintln!(
            "Attempting to fetch pre-built oatsc compiler for {}",
            artifact_name
        );
    }

    // Get cache directory
    let cache_dir = match get_cache_dir() {
        Ok(dir) => {
            if verbose {
                eprintln!("Oats cache directory: {}", dir.display());
            }
            dir
        }
        Err(e) => {
            eprintln!("Warning: Failed to get cache directory: {}", e);
            return None;
        }
    };

    // Get latest runtime tag (reusing the same release)
    let tag = match get_latest_runtime_tag() {
        Ok(t) => {
            if verbose {
                eprintln!("Latest release: {}", t);
            }
            t
        }
        Err(e) => {
            if verbose {
                eprintln!("Warning: Failed to fetch latest release tag: {}", e);
            }
            return None;
        }
    };

    // Check if we already have this version cached
    let cached_path = cache_dir.join(&tag).join(artifact_name);
    if cached_path.exists() {
        if verbose {
            eprintln!("Using cached oatsc compiler: {}", cached_path.display());
        }
        return Some(cached_path);
    }

    // Download the compiler
    if verbose {
        eprintln!("Cache miss, downloading oatsc compiler...");
    }
    if let Some(parent) = cached_path.parent() {
        fs::create_dir_all(parent).ok();
    }
    match download_runtime(&tag, artifact_name, &cached_path) {
        Ok(_) => {
            // Make the binary executable
            if let Ok(metadata) = fs::metadata(&cached_path) {
                let mut permissions = metadata.permissions();
                permissions.set_mode(0o755);
                fs::set_permissions(&cached_path, permissions).ok();
            }

            if verbose {
                eprintln!(
                    "Successfully downloaded oatsc compiler to: {}",
                    cached_path.display()
                );
            }
            Some(cached_path)
        }
        Err(e) => {
            eprintln!("Warning: Failed to download oatsc compiler: {}", e);
            None
        }
    }
}

#[test]
fn test_fetch_runtime() -> Result<(), Box<dyn std::error::Error>> {
    let artifact_name = get_runtime_artifact_name();
    let is_supported = artifact_name != "libruntime-unsupported.a";
    let has_no_remote = std::env::var("OATS_NO_REMOTE_RUNTIME").is_ok();

    let result = try_fetch_runtime();

    if !is_supported || has_no_remote {
        assert!(
            result.is_none(),
            "Should return None on unsupported platform or when OATS_NO_REMOTE_RUNTIME is set"
        );
    } else {
        // On supported platform, it should try to fetch
        if let Some(path) = result {
            assert!(path.exists(), "Fetched runtime should exist");
            assert!(path.is_file(), "Should be a file");
            let metadata = fs::metadata(&path)?;
            assert!(
                metadata.len() > 1000,
                "Runtime library should be reasonably sized"
            );
            // Check it's in cache dir
            let cache_dir = get_cache_dir()?;
            assert!(path.starts_with(cache_dir), "Should be in cache directory");
        } else {
            // Fetch failed (e.g., no releases, network issues), which is acceptable for this test
            println!("Fetch failed on supported platform, but that's ok");
        }
    }
    Ok(())
}

#[test]
fn test_fetch_oatsc() -> Result<(), Box<dyn std::error::Error>> {
    let artifact_name = get_oatsc_artifact_name();
    let is_supported = artifact_name != "oatsc-unsupported";
    let has_no_remote = std::env::var("OATS_NO_REMOTE_COMPILER").is_ok();

    let result = try_fetch_oatsc();

    if !is_supported || has_no_remote {
        assert!(
            result.is_none(),
            "Should return None on unsupported platform or when OATS_NO_REMOTE_COMPILER is set"
        );
    } else {
        // On supported platform, it should try to fetch
        if let Some(path) = result {
            assert!(path.exists(), "Fetched oatsc compiler should exist");
            assert!(path.is_file(), "Should be a file");
            let metadata = fs::metadata(&path)?;
            assert!(
                metadata.len() > 1000,
                "Oatsc compiler binary should be reasonably sized"
            );
            // Check it's in cache dir
            let cache_dir = get_cache_dir()?;
            assert!(path.starts_with(cache_dir), "Should be in cache directory");
        } else {
            // Fetch failed (e.g., no releases, network issues), which is acceptable for this test
            println!("Fetch failed on supported platform, but that's ok");
        }
    }
    Ok(())
}
