//! Main entry point for the Oats AOT compiler.
//!
//! This module implements the primary compilation driver that orchestrates
//! the complete pipeline from Oats source files to native executables.
//!
//! # Compilation Pipeline
//!
//! 1. **Parsing**: Converts source text to AST using the oats_parser
//! 2. **Type Analysis**: Performs type checking and validates function signatures
//! 3. **Code Generation**: Emits LLVM IR for all functions and top-level constructs
//! 4. **Object Compilation**: Compiles IR to native object files via LLVM
//! 5. **Linking**: Links object files with runtime to produce executables
//!
//! # Usage
//!
//! ```bash
//! # Compile a single file
//! oatsc main.oats
//!
//! # Compile with external module dependencies
//! oatsc main.oats --extern-oats "./module=/path/to/module.oats.meta"
//!
//! # Emit object file only (for library compilation)
//! oatsc lib.oats --emit-object-only
//! ```
//!
//! Note: Package-level orchestration (manifest parsing, dependency resolution,
//! multi-file compilation) is handled by `toasty`, not `oatsc`.

use anyhow::Result;
use clap::Parser;

#[derive(Parser)]
#[command(name = "oatsc", about = "Oats AOT Compiler", version = env!("CARGO_PKG_VERSION"))]
struct Cli {
    /// Source file to compile
    src_file: String,

    /// External module dependencies: <import_path>=<meta_file>
    /// Format: --extern-oats "./module=/path/to/module.oats.meta"
    #[arg(long = "extern-oats", value_name = "PATH=META")]
    extern_oats: Vec<String>,

    /// Override the output executable/library name
    #[arg(long = "out-name")]
    out_name: Option<String>,

    /// Emit object file only, do not link
    #[arg(long = "emit-object-only")]
    emit_object_only: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    compile_file(
        cli.src_file,
        cli.extern_oats,
        cli.emit_object_only,
        cli.out_name,
    )
}

/// Compile a source file
fn compile_file(
    src_file: String,
    extern_oats: Vec<String>,
    emit_object_only: bool,
    out_name: Option<String>,
) -> Result<()> {
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
    options.emit_object_only = emit_object_only;
    options.out_name = out_name;

    oatsc::compile(options)?;
    Ok(())
}
