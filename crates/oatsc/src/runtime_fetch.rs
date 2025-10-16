/// Runtime fetcher module - downloads pre-built runtime libraries from GitHub releases
///
/// This module provides functionality to fetch pre-built runtime static libraries
/// from GitHub releases, enabling toasty to work standalone without requiring
/// the full oats repository or Rust toolchain.

use anyhow::Result;
use std::path::{Path, PathBuf};
use std::fs;

/// GitHub repository information
const GITHUB_OWNER: &str = "Chrono-byte";
const GITHUB_REPO: &str = "oats";

/// Get the cache directory for runtime libraries
fn get_cache_dir() -> Result<PathBuf> {
    let cache_dir = if let Ok(dir) = std::env::var("OATS_RUNTIME_CACHE") {
        PathBuf::from(dir)
    } else if let Some(cache_home) = dirs::cache_dir() {
        cache_home.join("oats").join("runtime")
    } else {
        // Fallback to temp directory
        std::env::temp_dir().join("oats_runtime_cache")
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
            eprintln!("Warning: Unsupported platform {}-{}, will build locally", os, arch);
            "libruntime-unsupported.a"
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
    
    let releases: serde_json::Value = response.into_json()?;
    
    // Find the latest runtime release (tagged as runtime-*)
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str() {
                if tag.starts_with("runtime-") {
                    return Ok(tag.to_string());
                }
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
        eprintln!("Attempting to fetch pre-built runtime for {}", artifact_name);
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
    fs::create_dir_all(cached_path.parent().unwrap()).ok()?;
    match download_runtime(&tag, artifact_name, &cached_path) {
        Ok(_) => {
            eprintln!("Successfully downloaded runtime to: {}", cached_path.display());
            Some(cached_path)
        }
        Err(e) => {
            eprintln!("Warning: Failed to download runtime: {}", e);
            None
        }
    }
}
