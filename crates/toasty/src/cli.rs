//! Command-line interface definitions
//!
//! This module contains all clap struct definitions and argument parsing logic
//! for the toasty CLI tool. It separates the interface definition from the
//! business logic that executes the commands.

use clap::{Parser, Subcommand};

/// Compilation options for invoking oatsc
#[derive(Debug, Clone)]
pub struct CompileOptions {
    pub src_file: String,
    pub extern_oats: std::collections::HashMap<String, String>,
    pub package_root: Option<std::path::PathBuf>,
    pub extern_pkg: std::collections::HashMap<String, String>,
    pub out_dir: Option<String>,
    pub out_name: Option<String>,
    pub linker: Option<String>,
    pub emit_object_only: bool,
    pub link_runtime: bool,
    pub opt_level: Option<String>,
    pub lto: Option<String>,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub build_profile: Option<String>,
}

impl CompileOptions {
    pub fn new(src_file: String) -> Self {
        Self {
            src_file,
            extern_oats: std::collections::HashMap::new(),
            package_root: None,
            extern_pkg: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: false,
            link_runtime: true,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }

    pub fn with_modules(entry_point: String) -> Self {
        Self {
            src_file: entry_point,
            extern_oats: std::collections::HashMap::new(),
            package_root: None,
            extern_pkg: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: false,
            link_runtime: true,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }
}

#[derive(Parser)]
#[command(name = "toasty", about = "Oats Project Manager", version = env!("CARGO_PKG_VERSION"))]
pub struct Cli {
    /// Print verbose debug information even in release builds
    #[arg(long = "verbose")]
    pub verbose: bool,
    #[command(subcommand)]
    pub cmd: Commands,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Build a .oats file into a native executable (like `cargo build`)
    Build {
        /// Path to the source .oats file to compile. If omitted, OATS_SRC_FILE env var is used.
        src: Option<String>,

        /// Output directory for compilation artifacts (defaults to current directory or OATS_OUT_DIR)
        #[arg(short, long)]
        out_dir: Option<String>,
        /// Override the produced executable name (without path)
        #[arg(long = "out-name")]
        out_name: Option<String>,

        /// Explicit linker to use for final binary (sets OATS_LINKER)
        #[arg(long)]
        linker: Option<String>,
        /// Build in release mode (enables optimizations/LTO)
        #[arg(long)]
        release: bool,

        /// Emit object only and skip final host linking (sets OATS_EMIT_OBJECT_ONLY)
        #[arg(long = "emit-object-only")]
        emit_object_only: bool,

        /// Skip linking the runtime library (for custom linking)
        #[arg(long = "no-link-runtime")]
        no_link_runtime: bool,

        /// Set opt level: none, default, aggressive (also OATS_OPT_LEVEL)
        #[arg(long = "opt-level")]
        opt_level: Option<String>,

        /// LTO mode: none, thin, full (also OATS_LTO)
        #[arg(long = "lto")]
        lto: Option<String>,

        /// Target triple to pass to LLVM (sets OATS_TARGET_TRIPLE)
        #[arg(long = "target-triple")]
        target_triple: Option<String>,

        /// Target CPU to use for TargetMachine (sets OATS_TARGET_CPU)
        #[arg(long = "target-cpu")]
        target_cpu: Option<String>,

        /// Target features to enable (sets OATS_TARGET_FEATURES)
        #[arg(long = "target-features")]
        target_features: Option<String>,
        /// Suppress progress output
        #[arg(long = "quiet")]
        quiet: bool,
        /// Force color output: auto, always, never
        #[arg(long = "color")]
        color: Option<String>,
    },

    /// Build then run the produced executable (like `cargo run`)
    Run {
        /// Path to the source .oats file to compile. If omitted, OATS_SRC_FILE env var is used.
        src: Option<String>,

        /// Arguments to pass to the executed program (after `--`)
        #[arg(last = true)]
        args: Vec<String>,
        /// Override the produced executable name (without path)
        #[arg(long = "out-name")]
        out_name: Option<String>,
        /// Suppress progress output
        #[arg(long = "quiet")]
        quiet: bool,
        /// Force color output: auto, always, never
        #[arg(long = "color")]
        color: Option<String>,
    },

    /// Create a new Oats project (like `cargo new`)
    New {
        /// Name of the project to create
        name: String,
        /// Create a library project instead of executable
        #[arg(long)]
        lib: bool,
        /// Suppress progress output
        #[arg(long)]
        quiet: bool,
    },

    /// Manage oatsc compiler versions
    Compiler {
        #[command(subcommand)]
        action: CompilerCommands,
    },

    /// Manage Oats runtime library versions
    Runtime {
        #[command(subcommand)]
        action: RuntimeCommands,
    },
}

#[derive(Subcommand)]
pub enum CompilerCommands {
    /// List available compiler versions
    List,
    /// Install a specific compiler version
    Install {
        /// Version tag to install (e.g., stable, nightly, or compiler-v1.0.0)
        version: String,
    },
    /// Switch to a specific compiler version
    Use {
        /// Version tag to use (e.g., stable, nightly, or compiler-v1.0.0)
        version: String,
    },
    /// Show current compiler version
    Current,
    /// Uninstall a specific compiler version
    Uninstall {
        /// Version tag to uninstall (e.g., stable, nightly, or compiler-v1.0.0)
        version: String,
    },
}

#[derive(Subcommand)]
pub enum RuntimeCommands {
    /// List available runtime versions
    List,
    /// Install a specific runtime version
    Install {
        /// Version tag to install (e.g., runtime-v1.0.0)
        version: String,
    },
    /// Switch to a specific runtime version
    Use {
        /// Version tag to use (e.g., runtime-v1.0.0)
        version: String,
    },
    /// Show current runtime version
    Current,
    /// Uninstall a specific runtime version
    Uninstall {
        /// Version tag to uninstall (e.g., runtime-v1.0.0)
        version: String,
    },
}
