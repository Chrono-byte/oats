//! Oats.toml project manifest parsing
//!
//! This module handles parsing and validation of Oats.toml project manifests,
//! providing a structured representation of project configuration.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

/// Represents a complete Oats project manifest
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub package: Package,
    #[serde(default)]
    pub dependencies: HashMap<String, Dependency>,
    #[serde(default)]
    pub build: BuildConfig,
}

/// Package metadata section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    #[serde(default)]
    pub authors: Vec<String>,
    #[serde(default)]
    pub description: Option<String>,
    #[serde(default)]
    pub license: Option<String>,
    /// Entry point for the package (e.g., "src/main.oats" or "src/lib.oats")
    #[serde(default = "default_entry_point")]
    pub entry: String,
}

fn default_entry_point() -> String {
    "src/main.oats".to_string()
}

/// Dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Dependency {
    /// Simple version string (for future external dependencies)
    Version(String),
    /// Detailed dependency specification
    Detailed(DependencySpec),
}

/// Detailed dependency specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DependencySpec {
    /// Local path dependency
    pub path: Option<String>,
    /// Version requirement (future use)
    pub version: Option<String>,
}

/// Build configuration section
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct BuildConfig {
    #[serde(rename = "target-dir")]
    #[serde(default = "default_target_dir")]
    pub target_dir: String,
    #[serde(default = "default_profile")]
    pub profile: String,
    #[serde(default)]
    pub profiles: HashMap<String, Profile>,
}

/// Build profile configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Profile {
    #[serde(rename = "opt-level")]
    #[serde(default)]
    pub opt_level: String,
    #[serde(default = "default_debug")]
    pub debug: bool,
    #[serde(default)]
    pub lto: Option<String>,
}

fn default_target_dir() -> String {
    "target".to_string()
}

fn default_profile() -> String {
    "dev".to_string()
}

fn default_debug() -> bool {
    true
}

/// Basic semver validation (major.minor.patch with optional pre-release and build)
fn is_valid_semver(version: &str) -> bool {
    // Split into core version and optional pre-release/build
    let mut parts = version.splitn(2, |c| c == '-' || c == '+');
    let core = parts.next().unwrap();
    let suffix = parts.next();

    // Core must be major.minor.patch
    let version_parts: Vec<&str> = core.split('.').collect();
    if version_parts.len() != 3 {
        return false;
    }
    for part in version_parts {
        if part.is_empty() || !part.chars().all(|c| c.is_ascii_digit()) {
            return false;
        }
        if part.len() > 1 && part.starts_with('0') {
            return false; // no leading zero unless 0
        }
    }

    // Suffix must be alphanumeric with dots, dashes, plus
    if let Some(suf) = suffix {
        if suf.is_empty()
            || !suf
                .chars()
                .all(|c| c.is_alphanumeric() || c == '.' || c == '-' || c == '+')
        {
            return false;
        }
    }

    true
}

impl Manifest {
    /// Parse a manifest from a TOML string
    pub fn from_toml(content: &str) -> Result<Self> {
        let manifest: Self = toml::from_str(content)
            .map_err(|e| anyhow::anyhow!("Failed to parse Oats.toml: {}", e))?;
        manifest.validate()?;
        Ok(manifest)
    }

    /// Load manifest from a file path
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| anyhow::anyhow!("Failed to read {}: {}", path.display(), e))?;
        Self::from_toml(&content)
    }

    /// Find and load manifest from current directory or parent directories
    pub fn discover() -> Result<Option<(Self, std::path::PathBuf)>> {
        let mut current = std::env::current_dir()?;

        loop {
            let manifest_path = current.join("Oats.toml");
            if manifest_path.exists() {
                let manifest = Self::from_file(&manifest_path)?;
                return Ok(Some((manifest, manifest_path)));
            }

            if !current.pop() {
                break;
            }
        }

        Ok(None)
    }

    /// Validate the manifest
    pub fn validate(&self) -> Result<()> {
        // Validate package name
        if self.package.name.is_empty() {
            anyhow::bail!("Package name cannot be empty");
        }

        if !self
            .package
            .name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            anyhow::bail!(
                "Package name must contain only alphanumeric characters, hyphens, and underscores"
            );
        }

        // Validate version (semver format)
        if self.package.version.is_empty() {
            anyhow::bail!("Package version cannot be empty");
        }

        // Validate entry point is not empty
        if self.package.entry.is_empty() {
            anyhow::bail!("Package entry point cannot be empty");
        }

        if !is_valid_semver(&self.package.version) {
            anyhow::bail!("Package version must be a valid semantic version (e.g., 1.0.0)");
        }

        // Validate dependencies
        for (name, dep) in &self.dependencies {
            match dep {
                Dependency::Detailed(spec) => {
                    if spec.path.is_none() && spec.version.is_none() {
                        anyhow::bail!(
                            "Dependency '{}' must specify either 'path' or 'version'",
                            name
                        );
                    }
                    if let Some(version) = &spec.version {
                        if !is_valid_semver(version) {
                            anyhow::bail!(
                                "Dependency '{}' version '{}' is not a valid semantic version",
                                name,
                                version
                            );
                        }
                    }
                }
                Dependency::Version(version) => {
                    if !is_valid_semver(version) {
                        anyhow::bail!(
                            "Dependency '{}' version '{}' is not a valid semantic version",
                            name,
                            version
                        );
                    }
                }
            }
        }

        Ok(())
    }

    /// Get the absolute path to the entry point for this package
    pub fn entry_point(&self, manifest_dir: &Path) -> std::path::PathBuf {
        manifest_dir.join(&self.package.entry)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_manifest() {
        let toml = r#"
            [package]
            name = "test-project"
            version = "0.1.0"

            [dependencies]
            my-lib = { path = "./lib" }
        "#;

        let manifest = Manifest::from_toml(toml).unwrap();
        assert_eq!(manifest.package.name, "test-project");
        assert_eq!(manifest.package.version, "0.1.0");
        assert_eq!(manifest.dependencies.len(), 1);
    }

    #[test]
    fn test_validate_package_name() {
        let manifest = Manifest {
            package: Package {
                name: "invalid name!".to_string(),
                version: "0.1.0".to_string(),
                authors: vec![],
                description: None,
                license: None,
                entry: "src/main.oats".to_string(),
            },
            dependencies: HashMap::new(),
            build: BuildConfig::default(),
        };

        assert!(manifest.validate().is_err());
    }

    #[test]
    fn test_validate_invalid_version() {
        let manifest = Manifest {
            package: Package {
                entry: "src/main.oats".to_string(),
                name: "test".to_string(),
                version: "invalid".to_string(),
                authors: vec![],
                description: None,
                license: None,
            },
            dependencies: HashMap::new(),
            build: BuildConfig::default(),
        };

        assert!(manifest.validate().is_err());
    }
}
