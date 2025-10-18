//! Main entry point for the Oats AOT compiler.
//!
//! This module implements the primary compilation driver that orchestrates
//! the complete pipeline from TypeScript/Oats source files to native executables.
//! The compiler supports transitive module loading, type checking, LLVM code
//! generation, and native linking with the Oats runtime library.
//!
//! # Compilation Pipeline
//!
//! 1. **Module Resolution**: Discovers and loads all modules transitively from the entry point
//! 2. **Parsing**: Converts source text to AST using the deno_ast TypeScript parser
//! 3. **Type Analysis**: Performs type checking and validates function signatures
//! 4. **Code Generation**: Emits LLVM IR for all functions and top-level constructs
//! 5. **Object Compilation**: Compiles IR to native object files via LLVM
//! 6. **Runtime Linking**: Links object files with the Oats runtime to produce executables
//!
//! # Module System
//!
//! The compiler implements a simple module system supporting relative imports:
//! - Resolves `./module` and `../module` style imports
//! - Supports `.ts`, `.oats`, and extension-less files
//! - Attempts `index.oats` fallbacks for directory imports
//! - Maintains a dependency graph to prevent cycles and duplicates
//!
//! # Usage
//!
//! The compiler accepts source files via command-line arguments or environment variables:
//! ```bash
//! cargo run -- main.oats                    # Via argument
//! OATS_SRC_FILE=main.oats cargo run         # Via environment variable
//! ```

use anyhow::Result;

fn main() -> Result<()> {
    // Parse command-line arguments
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        anyhow::bail!("Usage: {} <source_file> [--extern-oats <import_path>=<meta_file>]...", args[0]);
    }

    let src_path = &args[1];

    // Parse --extern-oats flags
    let mut extern_oats = std::collections::HashMap::new();
    let mut i = 2;
    while i < args.len() {
        if args[i] == "--extern-oats" {
            if i + 1 >= args.len() {
                anyhow::bail!("--extern-oats requires an argument");
            }
            let extern_arg = &args[i + 1];
            if let Some((import_path, meta_file)) = extern_arg.split_once('=') {
                extern_oats.insert(import_path.to_string(), meta_file.to_string());
            } else {
                anyhow::bail!("--extern-oats argument must be in format: <import_path>=<meta_file>");
            }
            i += 2;
        } else {
            anyhow::bail!("Unknown argument: {}", args[i]);
        }
    }

    // For now, continue with single-file compilation
    // TODO: Phase 2 - use extern_oats for import resolution instead of filesystem access
    let mut options = oatsc::CompileOptions::new(src_path.clone());
    options.extern_oats = extern_oats;

    // Use the library API for compilation
    oatsc::compile(options)?;

    Ok(())
}
