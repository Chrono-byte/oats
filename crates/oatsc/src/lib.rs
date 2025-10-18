pub mod builder;
pub mod codegen;
pub mod diagnostics;
pub mod parser;
pub mod rta;
pub mod runtime_fetch;
pub mod types;

/// Compilation options for the Oats compiler.
///
/// This structure provides a clean API for configuring the compiler,
/// separating it from CLI argument parsing and build orchestration logic.
/// It follows the rustc model where the compiler accepts explicit options
/// rather than discovering project structure.
#[derive(Debug, Clone)]
pub struct CompileOptions {
    /// Path to the root source file to compile (for single-file mode)
    pub src_file: String,

    /// Additional source files to compile together (for multi-module mode)
    pub additional_src_files: Vec<String>,

    /// External module dependencies: maps import paths to metadata file paths
    /// Format: --extern-oats "./module.oats=/path/to/module.oats.meta"
    pub extern_oats: std::collections::HashMap<String, String>,

    /// Output directory for compilation artifacts
    pub out_dir: Option<String>,

    /// Override the output executable/library name
    pub out_name: Option<String>,

    /// Explicit linker to use for final binary
    pub linker: Option<String>,

    /// Whether to emit object file only (skip linking)
    pub emit_object_only: bool,

    /// Optimization level: none, default, aggressive
    pub opt_level: Option<String>,

    /// LTO mode: none, thin, full
    pub lto: Option<String>,

    /// Target triple for cross-compilation
    pub target_triple: Option<String>,

    /// Target CPU for code generation
    pub target_cpu: Option<String>,

    /// Target features to enable
    pub target_features: Option<String>,

    /// Build profile: debug or release
    pub build_profile: Option<String>,
}

impl CompileOptions {
    /// Create a new CompileOptions for single-file compilation
    pub fn new(src_file: String) -> Self {
        Self {
            src_file,
            additional_src_files: Vec::new(),
            extern_oats: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: false,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }

    /// Create a new CompileOptions for multi-module compilation
    pub fn with_modules(src_file: String, additional_src_files: Vec<String>) -> Self {
        Self {
            src_file,
            additional_src_files,
            extern_oats: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: false,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }
}

/// Compile a source file with the given options.
///
/// This is the primary API for invoking the Oats compiler programmatically.
/// It handles the complete compilation pipeline from source to executable/object.
///
/// # Arguments
/// * `options` - Compilation options specifying source file and build parameters
///
/// # Returns
/// Path to the compiled output file on success
pub fn compile(options: CompileOptions) -> anyhow::Result<Option<String>> {
    builder::compile_with_options(options)
}
