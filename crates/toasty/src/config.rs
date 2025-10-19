//! Configuration management
//!
//! This module handles loading and managing toasty configuration from
//! ~/.oats/config.toml and environment variables.

use crate::diagnostics::{Result, ToastyError};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Global configuration for toasty
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct Config {
    /// Path to the Oats compiler binary
    pub compiler_path: Option<String>,
    /// Path to the Oats runtime library
    pub runtime_path: Option<String>,
    /// Path to the Oats standard library
    pub std_path: Option<String>,
    /// Default output directory
    pub out_dir: Option<String>,
    /// Default linker to use
    pub linker: Option<String>,
}

impl Config {
    /// Load configuration from ~/.oats/config.toml and environment variables
    pub fn load() -> Result<Self> {
        let mut config = Self::load_from_file().unwrap_or_default();

        // Override with environment variables if set
        if let Ok(path) = std::env::var("OATS_OATSC_PATH") {
            config.compiler_path = Some(path);
        }
        if let Ok(path) = std::env::var("OATS_RUNTIME_PATH") {
            config.runtime_path = Some(path);
        }
        if let Ok(path) = std::env::var("OATS_STD_PATH") {
            config.std_path = Some(path);
        }
        if let Ok(dir) = std::env::var("OATS_OUT_DIR") {
            config.out_dir = Some(dir);
        }
        if let Ok(linker) = std::env::var("OATS_LINKER") {
            config.linker = Some(linker);
        }

        Ok(config)
    }

    /// Load configuration from ~/.oats/config.toml
    fn load_from_file() -> Result<Self> {
        let config_path = Self::config_path();
        if !config_path.exists() {
            return Ok(Self::default());
        }

        let content =
            std::fs::read_to_string(&config_path).map_err(|e| ToastyError::io(&config_path, e))?;

        let config: Self = toml::from_str(&content).map_err(|e| {
            ToastyError::other(format!(
                "Failed to parse config file {}: {}",
                config_path.display(),
                e
            ))
        })?;

        Ok(config)
    }

    /// Get the path to the configuration file
    fn config_path() -> PathBuf {
        let mut path = dirs::home_dir().unwrap_or_else(|| PathBuf::from("."));
        path.push(".oats");
        path.push("config.toml");
        path
    }

    /// Save the current configuration to file
    pub fn save(&self) -> Result<()> {
        let config_path = Self::config_path();

        // Ensure directory exists
        if let Some(parent) = config_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| ToastyError::io(parent, e))?;
        }

        let content = toml::to_string_pretty(self)
            .map_err(|e| ToastyError::other(format!("Failed to serialize config: {}", e)))?;

        std::fs::write(&config_path, content).map_err(|e| ToastyError::io(&config_path, e))?;

        Ok(())
    }
}
