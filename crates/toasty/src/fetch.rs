//! Dependency fetching module
//!
//! This module provides functionality to fetch pre-built compiler binaries
//! and runtime libraries from GitHub releases, enabling toasty to work
//! standalone without requiring the full oats repository or Rust toolchain.

use crate::diagnostics::{Result, ToastyError};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

/// Types of artifacts that can be fetched
#[derive(Debug, Clone, Copy)]
enum ArtifactType {
    Compiler,
    Runtime,
}

/// GitHub repository information
const GITHUB_OWNER: &str = "Chrono-byte";
const GITHUB_REPO: &str = "oats";

// Note: ureq 3.1 API - using global functions directly

/// Compute SHA-256 hash of a file
fn compute_sha256(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)?;
    let mut hasher = Sha256::new();
    std::io::copy(&mut file, &mut hasher)?;
    let hash = hasher.finalize();
    Ok(format!("{hash:x}"))
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

/// Detect the current platform and return the appropriate artifact name
fn get_artifact_name(artifact_type: ArtifactType) -> &'static str {
    let os = std::env::consts::OS;
    let arch = std::env::consts::ARCH;

    match artifact_type {
        ArtifactType::Compiler => match (os, arch) {
            ("linux", "x86_64") => "oatsc-linux-x86_64",
            ("linux", "aarch64") => "oatsc-linux-aarch64",
            ("macos", "x86_64") => "oatsc-macos-x86_64",
            ("macos", "aarch64") => "oatsc-macos-aarch64",
            ("windows", "x86_64") => "oatsc-windows-x86_64.exe",
            _ => {
                eprintln!("Warning: Unsupported platform {os}-{arch}, will build locally");
                "oatsc-unsupported"
            }
        },
        ArtifactType::Runtime => match (os, arch) {
            ("linux", "x86_64") => "libruntime-linux-x86_64.a",
            ("linux", "aarch64") => "libruntime-linux-aarch64.a",
            ("macos", "x86_64") => "libruntime-macos-x86_64.a",
            ("macos", "aarch64") => "libruntime-macos-aarch64.a",
            _ => {
                eprintln!("Warning: Unsupported platform {os}-{arch}, will build locally");
                "libruntime-unsupported.a"
            }
        },
    }
}

/// Fetch the latest release tag for a given artifact type from GitHub
fn get_latest_tag(artifact_type: ArtifactType) -> Result<String> {
    let url = format!("https://api.github.com/repos/{GITHUB_OWNER}/{GITHUB_REPO}/releases");

    let response = ureq::get(&url)
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch releases: {e}")))?;

    let body = response
        .into_body()
        .read_to_string()
        .map_err(|e| ToastyError::other(format!("Failed to read response body: {e}")))?;
    let releases: serde_json::Value = serde_json::from_str(&body)
        .map_err(|e| ToastyError::other(format!("Failed to parse releases JSON: {e}")))?;

    // Find the latest release matching the artifact type
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str() {
                let matches = match artifact_type {
                    ArtifactType::Compiler => tag == "nightly" || tag.starts_with("stable-"),
                    ArtifactType::Runtime => tag.starts_with("runtime-"),
                };
                if matches {
                    return Ok(tag.to_string());
                }
            }
        }
    }

    let error_msg = match artifact_type {
        ArtifactType::Compiler => "No compiler releases found".to_string(),
        ArtifactType::Runtime => "No runtime releases found".to_string(),
    };
    Err(ToastyError::other(error_msg))
}

/// Download an artifact from GitHub release
fn download_artifact(
    tag: &str,
    artifact_name: &str,
    dest_path: &Path,
    show_progress: bool,
    make_executable: bool,
) -> Result<()> {
    let url = format!(
        "https://github.com/{GITHUB_OWNER}/{GITHUB_REPO}/releases/download/{tag}/{artifact_name}"
    );

    let verbose = std::env::var("TOASTY_VERBOSE").is_ok();
    if verbose {
        eprintln!("Downloading {artifact_name} from: {url}");
    } else {
        eprintln!(
            "Downloading pre-built {}...",
            if artifact_name.contains("oatsc") {
                "compiler binary"
            } else if artifact_name.contains("runtime") {
                "runtime library"
            } else {
                "artifact"
            }
        );
    }

    let response = ureq::get(&url)
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to download {artifact_name}: {e}")))?;

    let total_size = response
        .headers()
        .get("content-length")
        .and_then(|h| h.to_str().ok())
        .and_then(|s| s.parse::<u64>().ok());

    // Read entire body into memory
    let body_string = response
        .into_body()
        .read_to_string()
        .map_err(|e| ToastyError::other(format!("Failed to read response body: {e}")))?;
    let body_bytes = body_string.as_bytes();
    let bytes_written = body_bytes.len() as u64;

    // Show progress if requested
    if show_progress && total_size == Some(bytes_written) {
        eprintln!("Download progress: 100%");
    }

    fs::write(dest_path, body_bytes)?;

    // Make the binary executable on Unix systems, if requested
    if make_executable {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let mut perms = fs::metadata(dest_path)?.permissions();
            perms.set_mode(0o755);
            fs::set_permissions(dest_path, perms)?;
        }
    }

    // Fetch digest from GitHub API for verification
    let api_url =
        format!("https://api.github.com/repos/{GITHUB_OWNER}/{GITHUB_REPO}/releases/tags/{tag}");
    let agent = ureq::agent();
    let release_resp = agent
        .get(&api_url)
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch release metadata: {e}")))?;
    let body = String::new();
    release_resp
        .into_body()
        .read_to_string()
        .map_err(|e| ToastyError::other(format!("Failed to read response body: {e}")))?;
    let release_json: serde_json::Value = serde_json::from_str(&body)
        .map_err(|e| ToastyError::other(format!("Failed to parse release metadata: {e}")))?;
    let mut expected_hash = None;
    if let Some(assets) = release_json.get("assets").and_then(|a| a.as_array()) {
        for asset in assets {
            if asset.get("name").and_then(|n| n.as_str()) == Some(artifact_name)
                && let Some(digest) = asset.get("digest").and_then(|d| d.as_str())
                && let Some(hash) = digest.strip_prefix("sha256:")
            {
                expected_hash = Some(hash.to_string());
            }
        }
    }
    let computed_hash = compute_sha256(dest_path)?;
    if let Some(expected_hash) = expected_hash {
        if computed_hash != expected_hash {
            eprintln!(
                "Warning: Hash mismatch for {artifact_name} asset (expected {expected_hash}, got {computed_hash}). Marking as unusable."
            );
            // Move unusable asset to quarantine folder
            let quarantine_dir = get_oats_home()?.join("quarantine");
            fs::create_dir_all(&quarantine_dir)?;
            let quarantine_path = quarantine_dir.join(artifact_name);
            fs::rename(dest_path, &quarantine_path)?;
            eprintln!(
                "Unusable {} asset moved to quarantine: {}",
                artifact_name,
                quarantine_path.display()
            );
            return Err(ToastyError::HashMismatch {
                name: artifact_name.to_string(),
                expected: expected_hash,
                actual: computed_hash,
            });
        }
    } else {
        eprintln!(
            "Warning: No digest found for {artifact_name} asset. Skipping hash verification."
        );
    }
    if verbose {
        eprintln!("Downloaded {bytes_written} bytes");
    }
    Ok(())
}

/// Download compiler binary from GitHub release
fn download_compiler(tag: &str, artifact_name: &str, dest_path: &Path) -> Result<()> {
    download_artifact(tag, artifact_name, dest_path, true, true)
}

/// Download runtime library from GitHub release
fn download_runtime(tag: &str, artifact_name: &str, dest_path: &Path) -> Result<()> {
    download_artifact(tag, artifact_name, dest_path, false, false)
}

/// Try to fetch pre-built artifact from GitHub releases
fn try_fetch_artifact(artifact_type: ArtifactType, skip_env_var: &str) -> Option<PathBuf> {
    let verbose = std::env::var("TOASTY_VERBOSE").is_ok();

    // Check if remote fetch should be skipped
    if std::env::var(skip_env_var).is_ok() {
        if verbose {
            eprintln!(
                "Skipping remote {} fetch ({} is set)",
                match artifact_type {
                    ArtifactType::Compiler => "compiler",
                    ArtifactType::Runtime => "runtime",
                },
                skip_env_var
            );
        }
        return None;
    }

    let artifact_name = get_artifact_name(artifact_type);
    let is_unsupported = match artifact_type {
        ArtifactType::Compiler => artifact_name == "oatsc-unsupported",
        ArtifactType::Runtime => artifact_name == "libruntime-unsupported.a",
    };

    if is_unsupported {
        if verbose {
            eprintln!(
                "Platform not supported for pre-built {}, will use local build",
                match artifact_type {
                    ArtifactType::Compiler => "compiler",
                    ArtifactType::Runtime => "runtime",
                }
            );
        }
        return None; // Unsupported platform, use local
    }

    if verbose {
        eprintln!(
            "Attempting to fetch pre-built {} for {}",
            match artifact_type {
                ArtifactType::Compiler => "compiler",
                ArtifactType::Runtime => "runtime",
            },
            artifact_name
        );
    }

    // Get cache directory
    let oats_home = match get_oats_home() {
        Ok(dir) => {
            if verbose {
                eprintln!(
                    "{} cache directory: {}",
                    match artifact_type {
                        ArtifactType::Compiler => "Compiler",
                        ArtifactType::Runtime => "Runtime",
                    },
                    dir.display()
                );
            }
            dir
        }
        Err(e) => {
            eprintln!("Warning: Failed to get cache directory: {e}");
            return None;
        }
    };

    // Get latest tag
    let tag = match get_latest_tag(artifact_type) {
        Ok(t) => {
            if verbose {
                eprintln!(
                    "Latest {} release: {}",
                    match artifact_type {
                        ArtifactType::Compiler => "compiler",
                        ArtifactType::Runtime => "runtime",
                    },
                    t
                );
            }
            t
        }
        Err(e) => {
            if verbose {
                eprintln!(
                    "Warning: Failed to fetch latest {} tag: {}",
                    match artifact_type {
                        ArtifactType::Compiler => "compiler",
                        ArtifactType::Runtime => "runtime",
                    },
                    e
                );
            }
            return None;
        }
    };

    // Check if this version is already cached
    let cached_path = oats_home.join(&tag).join(artifact_name);
    if cached_path.exists() {
        eprintln!(
            "Using cached {}: {}",
            match artifact_type {
                ArtifactType::Compiler => "compiler",
                ArtifactType::Runtime => "runtime",
            },
            cached_path.display()
        );
        return Some(cached_path);
    }

    // Download the artifact
    if verbose {
        eprintln!(
            "Cache miss, downloading {}...",
            match artifact_type {
                ArtifactType::Compiler => "compiler",
                ArtifactType::Runtime => "runtime",
            }
        );
    }
    if let Some(parent) = cached_path.parent() {
        fs::create_dir_all(parent).ok();
    }
    let download_result = match artifact_type {
        ArtifactType::Compiler => download_compiler(&tag, artifact_name, &cached_path),
        ArtifactType::Runtime => download_runtime(&tag, artifact_name, &cached_path),
    };

    match download_result {
        Ok(()) => {
            eprintln!(
                "Successfully downloaded {} to: {}",
                match artifact_type {
                    ArtifactType::Compiler => "compiler",
                    ArtifactType::Runtime => "runtime",
                },
                cached_path.display()
            );
            Some(cached_path)
        }
        Err(e) => {
            eprintln!(
                "Warning: Failed to download {}: {}",
                match artifact_type {
                    ArtifactType::Compiler => "compiler",
                    ArtifactType::Runtime => "runtime",
                },
                e
            );
            None
        }
    }
}

/// Try to fetch pre-built compiler from GitHub releases
#[must_use]
pub fn try_fetch_compiler() -> Option<PathBuf> {
    try_fetch_artifact(ArtifactType::Compiler, "OATS_NO_REMOTE_COMPILER")
}

/// Try to fetch pre-built runtime from GitHub releases
#[must_use]
pub fn try_fetch_runtime() -> Option<PathBuf> {
    try_fetch_artifact(ArtifactType::Runtime, "OATS_NO_REMOTE_RUNTIME")
}

/// List all available versions for a given artifact type from GitHub releases
fn list_available_versions(artifact_type: ArtifactType) -> Result<Vec<String>> {
    let url = format!("https://api.github.com/repos/{GITHUB_OWNER}/{GITHUB_REPO}/releases");

    let response = ureq::get(&url)
        .call()
        .map_err(|e| ToastyError::other(format!("Failed to fetch releases: {e}")))?;

    let body = response
        .into_body()
        .read_to_string()
        .map_err(|e| ToastyError::other(format!("Failed to read response body: {e}")))?;
    let releases: serde_json::Value = serde_json::from_str(&body)
        .map_err(|e| ToastyError::other(format!("Failed to parse releases JSON: {e}")))?;

    let mut versions = Vec::new();
    if let Some(releases_array) = releases.as_array() {
        for release in releases_array {
            if let Some(tag) = release["tag_name"].as_str() {
                let matches = match artifact_type {
                    ArtifactType::Compiler => tag == "nightly" || tag.starts_with("stable-"),
                    ArtifactType::Runtime => tag.starts_with("runtime-"),
                };
                if matches {
                    versions.push(tag.to_string());
                }
            }
        }
    }

    Ok(versions)
}

/// List all available compiler versions from GitHub releases
pub fn list_available_compilers() -> Result<Vec<String>> {
    list_available_versions(ArtifactType::Compiler)
}

/// List all available runtime versions from GitHub releases
pub fn list_available_runtimes() -> Result<Vec<String>> {
    list_available_versions(ArtifactType::Runtime)
}

/// Install a specific compiler version
pub fn install_compiler_version(version: &str) -> Result<()> {
    install_artifact_version(ArtifactType::Compiler, version)
}

/// Install a specific runtime version
pub fn install_runtime_version(version: &str) -> Result<()> {
    install_artifact_version(ArtifactType::Runtime, version)
}

/// Uninstall a specific version of an artifact
fn uninstall_artifact_version(artifact_type: ArtifactType, version: &str) -> Result<()> {
    let artifact_name = get_artifact_name(artifact_type);
    let is_unsupported = match artifact_type {
        ArtifactType::Compiler => artifact_name == "oatsc-unsupported",
        ArtifactType::Runtime => artifact_name == "libruntime-unsupported.a",
    };

    if is_unsupported {
        return Err(ToastyError::other(format!(
            "Unsupported platform for pre-built {}",
            match artifact_type {
                ArtifactType::Compiler => "compiler",
                ArtifactType::Runtime => "runtime",
            }
        )));
    }

    let oats_home = get_oats_home()?;
    let artifact_path = oats_home.join(version).join(artifact_name);

    // Check if installed
    if !artifact_path.exists() {
        eprintln!(
            "{} version {} is not installed.",
            match artifact_type {
                ArtifactType::Compiler => "Compiler",
                ArtifactType::Runtime => "Runtime",
            },
            version
        );
        return Ok(());
    }

    // Remove the artifact
    fs::remove_file(&artifact_path)?;

    // Remove the version directory if it's empty
    if let Some(version_dir) = artifact_path.parent()
        && version_dir.read_dir()?.next().is_none()
    {
        fs::remove_dir(version_dir)?;
    }

    // If this was the currently selected version, clear the selection
    let config_path = oats_home.join(match artifact_type {
        ArtifactType::Compiler => "current_version",
        ArtifactType::Runtime => "current_runtime_version",
    });
    if config_path.exists() {
        let current = fs::read_to_string(&config_path)?;
        if current.trim() == version {
            fs::remove_file(&config_path)?;
            eprintln!(
                "Cleared current {} version selection since {} was uninstalled.",
                match artifact_type {
                    ArtifactType::Compiler => "compiler",
                    ArtifactType::Runtime => "runtime",
                },
                version
            );
        }
    }

    eprintln!(
        "Successfully uninstalled {} version {}",
        match artifact_type {
            ArtifactType::Compiler => "compiler",
            ArtifactType::Runtime => "runtime",
        },
        version
    );
    Ok(())
}

/// Uninstall a specific compiler version
pub fn uninstall_compiler_version(version: &str) -> Result<()> {
    uninstall_artifact_version(ArtifactType::Compiler, version)
}

/// Uninstall a specific runtime version
pub fn uninstall_runtime_version(version: &str) -> Result<()> {
    uninstall_artifact_version(ArtifactType::Runtime, version)
}

/// Get the path to the currently selected version of an artifact
fn get_selected_artifact_path(artifact_type: ArtifactType) -> Option<PathBuf> {
    let Ok(oats_home) = get_oats_home() else {
        return None;
    };

    let config_path = oats_home.join(match artifact_type {
        ArtifactType::Compiler => "current_version",
        ArtifactType::Runtime => "current_runtime_version",
    });
    let version = match fs::read_to_string(&config_path) {
        Ok(v) => v.trim().to_string(),
        Err(_) => return None,
    };

    let artifact_name = get_artifact_name(artifact_type);
    let is_unsupported = match artifact_type {
        ArtifactType::Compiler => artifact_name == "oatsc-unsupported",
        ArtifactType::Runtime => artifact_name == "libruntime-unsupported.a",
    };

    if is_unsupported {
        return None;
    }

    let artifact_path = oats_home.join(version).join(artifact_name);
    if artifact_path.exists() {
        Some(artifact_path)
    } else {
        None
    }
}

/// Get the path to the currently selected compiler
#[must_use]
pub fn get_selected_compiler_path() -> Option<PathBuf> {
    get_selected_artifact_path(ArtifactType::Compiler)
}

/// Get the path to the currently selected runtime
#[must_use]
pub fn get_selected_runtime_path() -> Option<PathBuf> {
    get_selected_artifact_path(ArtifactType::Runtime)
}

/// Get the currently selected version of an artifact
fn current_artifact_version(artifact_type: ArtifactType) -> Result<String> {
    let oats_home = get_oats_home()?;
    let config_path = oats_home.join(match artifact_type {
        ArtifactType::Compiler => "current_version",
        ArtifactType::Runtime => "current_runtime_version",
    });

    if config_path.exists() {
        let version = fs::read_to_string(&config_path)?;
        Ok(version.trim().to_string())
    } else {
        // If no version is selected, try to get the latest
        match get_latest_tag(artifact_type) {
            Ok(tag) => Ok(format!("{tag} (latest)")),
            Err(_) => Ok("none selected".to_string()),
        }
    }
}

/// Get the currently selected compiler version
pub fn current_compiler_version() -> Result<String> {
    current_artifact_version(ArtifactType::Compiler)
}

/// Get the currently selected runtime version
pub fn current_runtime_version() -> Result<String> {
    current_artifact_version(ArtifactType::Runtime)
}

/// Switch to using a specific version of an artifact
fn use_artifact_version(artifact_type: ArtifactType, version: &str) -> Result<()> {
    let artifact_name = get_artifact_name(artifact_type);
    let is_unsupported = match artifact_type {
        ArtifactType::Compiler => artifact_name == "oatsc-unsupported",
        ArtifactType::Runtime => artifact_name == "libruntime-unsupported.a",
    };

    if is_unsupported {
        return Err(ToastyError::other(format!(
            "Unsupported platform for pre-built {}",
            match artifact_type {
                ArtifactType::Compiler => "compiler",
                ArtifactType::Runtime => "runtime",
            }
        )));
    }

    let oats_home = get_oats_home()?;
    let artifact_path = oats_home.join(version).join(artifact_name);

    if !artifact_path.exists() {
        return Err(ToastyError::other(format!(
            "{} version {} is not installed. Install it first with 'toasty {} install {}'",
            match artifact_type {
                ArtifactType::Compiler => "Compiler",
                ArtifactType::Runtime => "Runtime",
            },
            version,
            match artifact_type {
                ArtifactType::Compiler => "compiler",
                ArtifactType::Runtime => "runtime",
            },
            version
        )));
    }

    // Store the selected version in a config file
    let config_path = oats_home.join(match artifact_type {
        ArtifactType::Compiler => "current_version",
        ArtifactType::Runtime => "current_runtime_version",
    });
    fs::write(&config_path, version)?;

    eprintln!(
        "Switched to {} version {}",
        match artifact_type {
            ArtifactType::Compiler => "compiler",
            ArtifactType::Runtime => "runtime",
        },
        version
    );
    Ok(())
}

/// Switch to using a specific compiler version
pub fn use_compiler_version(version: &str) -> Result<()> {
    use_artifact_version(ArtifactType::Compiler, version)
}

/// Switch to using a specific runtime version
pub fn use_runtime_version(version: &str) -> Result<()> {
    use_artifact_version(ArtifactType::Runtime, version)
}

/// Install a specific version of an artifact
fn install_artifact_version(artifact_type: ArtifactType, version: &str) -> Result<()> {
    let artifact_name = get_artifact_name(artifact_type);
    let is_unsupported = match artifact_type {
        ArtifactType::Compiler => artifact_name == "oatsc-unsupported",
        ArtifactType::Runtime => artifact_name == "libruntime-unsupported.a",
    };

    if is_unsupported {
        return Err(ToastyError::other(format!(
            "Unsupported platform for pre-built {}",
            match artifact_type {
                ArtifactType::Compiler => "compiler",
                ArtifactType::Runtime => "runtime",
            }
        )));
    }

    let oats_home = get_oats_home()?;
    let cached_path = oats_home.join(version).join(artifact_name);

    // Check if already installed
    if cached_path.exists() {
        eprintln!(
            "{} version {} is already installed.",
            match artifact_type {
                ArtifactType::Compiler => "Compiler",
                ArtifactType::Runtime => "Runtime",
            },
            version
        );
        return Ok(());
    }

    // Download the artifact
    if let Some(parent) = cached_path.parent() {
        fs::create_dir_all(parent)?;
    }
    let download_result = match artifact_type {
        ArtifactType::Compiler => download_compiler(version, artifact_name, &cached_path),
        ArtifactType::Runtime => download_runtime(version, artifact_name, &cached_path),
    };

    download_result?;

    eprintln!(
        "Successfully installed {} version {}",
        match artifact_type {
            ArtifactType::Compiler => "compiler",
            ArtifactType::Runtime => "runtime",
        },
        version
    );
    Ok(())
}
