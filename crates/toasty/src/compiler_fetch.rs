/// Compiler fetcher module - downloads pre-built oatsc compiler from GitHub releases
///
/// This module provides functionality to fetch pre-built oatsc compiler binaries
/// from GitHub releases, enabling toasty to work standalone without requiring
/// the full oats repository or Rust toolchain.
use anyhow::Result;
use std::fs;
use std::path::{Path, PathBuf};

/// GitHub repository information
const GITHUB_OWNER: &str = "Chrono-byte";
const GITHUB_REPO: &str = "oats";

/// Get the cache directory for compiler binaries
fn get_cache_dir() -> Result<PathBuf> {
    let cache_dir = if let Ok(dir) = std::env::var("OATS_CACHE_DIR") {
        PathBuf::from(dir)
    } else if let Some(cache_home) = dirs::cache_dir() {
        cache_home.join("oats")
    } else {
        // Fallback to temp directory
        std::env::temp_dir().join("oats_cache")
    };

    fs::create_dir_all(&cache_dir)?;
    Ok(cache_dir)
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

/// Fetch the latest compiler release tag from GitHub
fn get_latest_compiler_tag() -> Result<String> {
    let url = format!(
        "https://api.github.com/repos/{}/{}/releases",
        GITHUB_OWNER, GITHUB_REPO
    );

    let response = ureq::get(&url)
        .set("User-Agent", "oats-compiler")
        .call()
        .map_err(|e| anyhow::anyhow!("Failed to fetch releases: {}", e))?;

    let releases: serde_json::Value = serde_json::from_reader(response.into_reader())?;

    // Find the latest compiler release (tagged as compiler-*)
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str()
                && tag.starts_with("compiler-")
            {
                return Ok(tag.to_string());
            }
        }
    }

    anyhow::bail!("No compiler releases found")
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
        .map_err(|e| anyhow::anyhow!("Failed to download compiler: {}", e))?;

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

    if verbose {
        eprintln!("Downloaded {} bytes", bytes_written);
    }

    Ok(())
}

/// Try to fetch pre-built compiler from GitHub releases
///
/// Returns the path to the compiler binary if successful.
/// If fetching fails, returns None and caller should fall back to local build.
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
    let cache_dir = match get_cache_dir() {
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
    let cached_path = cache_dir.join(&tag).join(artifact_name);
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

#[test]
fn test_fetch_compiler() -> Result<(), Box<dyn std::error::Error>> {
    let artifact_name = get_compiler_artifact_name();
    let is_supported = artifact_name != "oatsc-unsupported";
    let has_no_remote = std::env::var("OATS_NO_REMOTE_COMPILER").is_ok();

    let result = try_fetch_compiler();

    if !is_supported || has_no_remote {
        assert!(
            result.is_none(),
            "Should return None on unsupported platform or when OATS_NO_REMOTE_COMPILER is set"
        );
    } else {
        // On supported platform, it should try to fetch
        if let Some(path) = result {
            assert!(path.exists(), "Fetched compiler should exist");
            assert!(path.is_file(), "Should be a file");
            let metadata = fs::metadata(&path)?;
            assert!(
                metadata.len() > 1000,
                "Compiler binary should be reasonably sized"
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