use anyhow::Context;
use std::collections::{HashMap, VecDeque};

/// Resolves relative import specifiers to absolute file paths.
///
/// This function implements the module resolution algorithm for relative imports,
/// supporting common TypeScript/JavaScript conventions including file extension
/// inference and index file fallbacks for directory imports.
///
/// # Arguments
/// * `from` - Absolute path of the importing module
/// * `spec` - Relative import specifier (e.g., "./module", "../utils")
///
/// # Returns
/// Canonical absolute path string if a matching file is found, `None` otherwise
///
/// # Resolution Strategy
/// 1. **Direct file matching**: Tries `.oats`, and extension-less variants
/// 2. **Directory resolution**: Attempts `index.oats` for directories
/// 3. **Index fallbacks**: Additional index file patterns for compatibility
pub fn resolve_relative_import(from: &str, spec: &str) -> Option<String> {
    let base = std::path::Path::new(from)
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));
    let candidate = base.join(spec);
    let exts = [".oats", ""]; // Prioritize .oats, then raw

    // Attempt direct file resolution with extension inference
    for ext in &exts {
        let mut c = candidate.clone();
        if c.extension().is_none() && !ext.is_empty() {
            c.set_extension(ext.trim_start_matches('.'));
        }
        if c.exists()
            && let Ok(cabs) = std::fs::canonicalize(&c)
        {
            return Some(cabs.to_string_lossy().to_string());
        }
    }

    // Handle directory imports with index file resolution
    if candidate.exists() && candidate.is_dir() {
        if let Some(index_path) = resolve_index_file(&candidate) {
            return Some(index_path);
        }
    }

    // Additional index file resolution for edge cases
    if let Some(index_path) = resolve_index_file(&candidate) {
        return Some(index_path);
    }

    None
}

/// Attempts to resolve a directory to an index file.
/// Returns the canonical path if an index file is found, None otherwise.
fn resolve_index_file(dir_path: &std::path::Path) -> Option<String> {
    for idx in &["index.oats", "index"] {
        let index_path = dir_path.join(idx);
        if index_path.exists()
            && let Ok(cabs) = std::fs::canonicalize(&index_path)
        {
            return Some(cabs.to_string_lossy().to_string());
        }
    }
    None
}

/// Performs transitive module loading starting from an entry point.
///
/// This function discovers and loads all modules transitively from the entry point,
/// resolving relative imports and maintaining a dependency graph to prevent cycles
/// and duplicate processing.
///
/// # Arguments
/// * `entry_path` - Path to the entry point source file
/// * `verbose` - If true, print debug information about module discovery
///
/// # Returns
/// A map of canonicalized absolute paths to parsed modules
pub fn load_modules(
    entry_path: &str,
) -> anyhow::Result<HashMap<String, oatsc::parser::ParsedModule>> {
    load_modules_with_verbosity(entry_path, false)
}

/// Performs transitive module loading with optional verbose output.
pub fn load_modules_with_verbosity(
    entry_path: &str,
    verbose: bool,
) -> anyhow::Result<HashMap<String, oatsc::parser::ParsedModule>> {
    // Validate entry path exists and is readable
    if !std::path::Path::new(entry_path).exists() {
        anyhow::bail!("Entry file does not exist: {}", entry_path);
    }

    let mut modules: HashMap<String, oatsc::parser::ParsedModule> = HashMap::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    // Initialize module loading with the entry point file
    let entry_abs = std::fs::canonicalize(entry_path)
        .with_context(|| format!("Failed to canonicalize entry path: {}", entry_path))?;
    let entry_str = entry_abs.to_string_lossy().to_string();
    queue.push_back(entry_str.clone());

    // PHASE 1: Transitive module loading and dependency resolution
    while let Some(path) = queue.pop_front() {
        if modules.contains_key(&path) {
            continue; // Skip already processed modules
        }

        if verbose {
            eprintln!("Loading module: {}", path);
        }

        let src = std::fs::read_to_string(&path)
            .with_context(|| format!("Failed to read source file: {}", path))?;
        let parsed = oatsc::parser::parse_oats_module(&src, Some(&path))
            .with_context(|| format!("Failed to parse module: {}", path))?;

        // Discover and enqueue relative imports from this module
        for item_ref in parsed.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::Import(import_decl) = module_decl
            {
                let src_val = import_decl.src.value.to_string();
                // Process only relative import paths (absolute imports are not supported)
                if (src_val.starts_with("./") || src_val.starts_with("../"))
                    && let Some(fpath) = resolve_relative_import(&path, &src_val)
                    && !modules.contains_key(&fpath)
                {
                    if verbose {
                        eprintln!("  Discovered dependency: {}", fpath);
                    }
                    queue.push_back(fpath);
                }
            }
        }
        modules.insert(path.clone(), parsed);
    }

    Ok(modules)
}
