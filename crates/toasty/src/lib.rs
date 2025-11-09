//! Toasty - Oats Project Manager
//!
//! A command-line tool for building and managing Oats projects.
//! Provides package management, dependency resolution, and compilation orchestration.

pub mod build;
pub mod cli;
pub mod cli_flags;
pub mod commands;
pub mod compiler;
pub mod config;
pub mod diagnostics;
pub mod env;
pub mod fetch;
pub mod linker;
pub mod preflight;
pub mod project;
