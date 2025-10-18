//! Main entry point for the Oats AOT compiler.
//!
//! This module implements the primary compilation driver that orchestrates
//! the complete pipeline from TypeScript/Oats source files to native executables.
//! The compiler supports both single-file and package-based compilation modes.
//!
//! # Compilation Pipeline
//!
//! 1. **Module Resolution**: Discovers and loads all modules from the package
//! 2. **Parsing**: Converts source text to AST using the deno_ast TypeScript parser
//! 3. **Type Analysis**: Performs type checking and validates function signatures
//! 4. **Code Generation**: Emits LLVM IR for all functions and top-level constructs
//! 5. **Object Compilation**: Compiles IR to native object files via LLVM
//! 6. **Metadata Generation**: Generates .oats.meta files with package exports
//!
//! # Package-Based Compilation
//!
//! In package mode, oatsc compiles a single package as a unit:
//! - Accepts a package root directory via --package-root
//! - Loads external dependencies via --extern-pkg flags
//! - Restricts file access to the package directory
//! - Generates object file and metadata for the package
//!
//! # Usage
//!
//! ```bash
//! # Legacy single-file mode
//! oatsc main.oats
//!
//! # Package mode
//! oatsc --package-root /path/to/package --extern-pkg mylib=/path/to/mylib.oats.meta -o output.o
//! ```

use anyhow::Result;
use clap::Parser;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "oatsc", about = "Oats AOT Compiler", version = env!("CARGO_PKG_VERSION"))]
struct Cli {
    /// Source file to compile (legacy single-file mode)
    #[arg(conflicts_with = "package_root")]
    src_file: Option<String>,

    /// Package root directory containing Oats.toml (package mode)
    #[arg(long = "package-root")]
    package_root: Option<PathBuf>,

    /// External package dependencies: <pkg_name>=<meta_file>
    /// Format: --extern-pkg mylib=/path/to/mylib.oats.meta
    #[arg(long = "extern-pkg", value_name = "PKG=META")]
    extern_pkg: Vec<String>,

    /// Output file path (object file in package mode)
    #[arg(short = 'o', long = "output")]
    output: Option<PathBuf>,

    /// Legacy extern-oats flags for file-based compilation
    #[arg(long = "extern-oats", value_name = "PATH=META")]
    extern_oats: Vec<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    // Determine compilation mode
    if let Some(package_root) = cli.package_root {
        // Package-based compilation mode
        compile_package(package_root, cli.extern_pkg, cli.output)
    } else if let Some(src_file) = cli.src_file {
        // Legacy single-file compilation mode
        compile_single_file(src_file, cli.extern_oats)
    } else {
        anyhow::bail!("Either <SRC_FILE> or --package-root must be specified");
    }
}

/// Compile a package in package mode
fn compile_package(
    package_root: PathBuf,
    extern_pkgs: Vec<String>,
    output: Option<PathBuf>,
) -> Result<()> {
    // Parse extern-pkg flags
    let mut extern_packages = std::collections::HashMap::new();
    for extern_str in extern_pkgs {
        if let Some((pkg_name, meta_path)) = extern_str.split_once('=') {
            extern_packages.insert(pkg_name.to_string(), meta_path.to_string());
        } else {
            anyhow::bail!("--extern-pkg argument must be in format: <pkg_name>=<meta_file>");
        }
    }

    // TODO: Implement package compilation
    // 1. Load package manifest from package_root/Oats.toml
    // 2. Resolve entry point from manifest
    // 3. Compile package with extern_packages
    // 4. Generate .oats.meta file with exports
    // 5. Write object file to output path

    eprintln!("Package mode compilation not fully implemented yet");
    eprintln!("  Package root: {}", package_root.display());
    eprintln!("  External packages: {:?}", extern_packages);
    eprintln!("  Output: {:?}", output);

    anyhow::bail!("Package mode compilation is under development")
}

/// Compile a single file in legacy mode
fn compile_single_file(src_file: String, extern_oats: Vec<String>) -> Result<()> {
    // Parse --extern-oats flags
    let mut extern_map = std::collections::HashMap::new();
    for extern_str in extern_oats {
        if let Some((import_path, meta_file)) = extern_str.split_once('=') {
            extern_map.insert(import_path.to_string(), meta_file.to_string());
        } else {
            anyhow::bail!("--extern-oats argument must be in format: <import_path>=<meta_file>");
        }
    }

    let mut options = oatsc::CompileOptions::new(src_file);
    options.extern_oats = extern_map;

    oatsc::compile(options)?;
    Ok(())
}
