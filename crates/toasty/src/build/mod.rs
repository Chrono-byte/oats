//! Package-based build orchestration
//!
//! This module implements the build orchestrator for package-based compilation,
//! managing dependency resolution, parallel compilation, and final linking.

use crate::diagnostics::{Result, ToastyError};
use crate::project::{
    NodeIndex, PackageGraph, PackageNode, build_package_graph, topological_sort_packages,
};
use petgraph::visit::EdgeRef;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Compilation options for invoking oatsc
#[derive(Debug, Clone)]
pub struct CompileOptions {
    pub src_file: String,
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
    pub fn for_package(package_root: std::path::PathBuf) -> Self {
        Self {
            src_file: String::new(), // Will be set later
            package_root: Some(package_root),
            extern_pkg: std::collections::HashMap::new(),
            out_dir: None,
            out_name: None,
            linker: None,
            emit_object_only: true,
            link_runtime: false,
            opt_level: None,
            lto: None,
            target_triple: None,
            target_cpu: None,
            target_features: None,
            build_profile: None,
        }
    }
}

/// Build result for a single package
#[derive(Debug, Clone)]
pub struct PackageBuildResult {
    /// Name of the package
    pub name: String,
    /// Path to the compiled object file
    pub object_file: PathBuf,
    /// Path to the package metadata file
    pub meta_file: PathBuf,
}

/// Build configuration
pub struct BuildConfig {
    pub verbose: bool,
    pub quiet: bool,
    pub release: bool,
    pub out_dir: Option<String>,
    pub out_name: Option<String>,
    pub linker: Option<String>,
    pub opt_level: Option<String>,
    pub lto: Option<String>,
    pub target_triple: Option<String>,
    pub target_cpu: Option<String>,
    pub target_features: Option<String>,
    pub no_link_runtime: bool,
}

/// Orchestrate a package-based build starting from a root manifest
pub fn build_package_project(manifest_path: &Path, config: BuildConfig) -> Result<PathBuf> {
    // Build package dependency graph
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Building package dependency graph...");
    }

    let (graph, node_map, root_idx) = build_package_graph(manifest_path, config.verbose)?;

    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Package graph has {} package(s)", graph.node_count());
    }

    // Perform topological sort to get build order
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Computing build order...");
    }

    let build_order = topological_sort_packages(&graph, &node_map)?;

    if config.verbose {
        eprintln!("Build order:");
        for (i, (name, _)) in build_order.iter().enumerate() {
            eprintln!("  {}. {}", i + 1, name);
        }
    }

    // Build each package in order
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Compiling packages...");
    }

    let mut build_results = HashMap::new();

    for (i, (pkg_name, pkg_idx)) in build_order.iter().enumerate() {
        if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
            eprintln!(
                "Compiling package {}/{}: {}",
                i + 1,
                build_order.len(),
                pkg_name
            );
        }

        // Change to package directory for compilation
        let pkg_node = &graph[*pkg_idx];
        let original_dir = std::env::current_dir()?;
        std::env::set_current_dir(&pkg_node.root_dir)?;

        let result = compile_package(&graph, *pkg_idx, &build_results, &config)?;
        build_results.insert(pkg_name.clone(), result);

        // Restore original directory
        std::env::set_current_dir(original_dir)?;
    }

    // Link all packages together
    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Linking final executable...");
    }

    let root_pkg_name = &graph[root_idx].name;
    link_packages(root_pkg_name, &build_results, &config)
}

/// Compile a single package
fn compile_package(
    graph: &PackageGraph,
    pkg_idx: NodeIndex,
    built_deps: &HashMap<String, PackageBuildResult>,
    config: &BuildConfig,
) -> Result<PackageBuildResult> {
    let pkg_node = &graph[pkg_idx];
    let pkg_name = &pkg_node.name;

    if config.verbose {
        eprintln!("  Package root: {}", pkg_node.root_dir.display());
        eprintln!("  Entry point: {}", pkg_node.entry_point().display());
    }

    // Configure output
    let out_dir = config.out_dir.as_deref().unwrap_or("target");
    let obj_filename = format!("{}_pkg.o", pkg_name.replace('-', "_"));
    let obj_path = PathBuf::from(out_dir).join(&obj_filename);

    // Ensure output directory exists for incremental build checks
    if let Some(parent) = obj_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| ToastyError::io(parent, e))?;
        // Create CACHEDIR.TAG if this is a target directory
        if parent.ends_with("target") {
            create_cache_dir_tag(parent.to_string_lossy().as_ref())?;
        }
    }

    // Check for incremental build opportunity
    if let Ok(obj_metadata) = std::fs::metadata(&obj_path) {
        let obj_mtime = obj_metadata
            .modified()
            .unwrap_or(std::time::SystemTime::UNIX_EPOCH);

        // Check if source files are older than object file
        let mut needs_rebuild = false;

        // Check entry point
        if let Ok(src_metadata) = std::fs::metadata(pkg_node.entry_point())
            && let Ok(src_mtime) = src_metadata.modified()
            && src_mtime > obj_mtime
        {
            needs_rebuild = true;
        }

        // Check manifest if it exists
        let manifest_path = pkg_node.root_dir.join("Oats.toml");
        if let Ok(manifest_metadata) = std::fs::metadata(&manifest_path)
            && let Ok(manifest_mtime) = manifest_metadata.modified()
            && manifest_mtime > obj_mtime
        {
            needs_rebuild = true;
        }

        // Check dependency metadata files
        for edge in graph.edges(pkg_idx) {
            let dep_idx = edge.target();
            let dep_node = &graph[dep_idx];
            if let Some(dep_result) = built_deps.get(&dep_node.name)
                && let Ok(dep_metadata) = std::fs::metadata(&dep_result.meta_file)
                && let Ok(dep_mtime) = dep_metadata.modified()
                && dep_mtime > obj_mtime
            {
                needs_rebuild = true;
            }
        }

        if !needs_rebuild {
            if config.verbose {
                eprintln!("  Skipping compilation - {} is up to date", obj_filename);
            }

            // Generate metadata file (may still need updating)
            let meta_file = PathBuf::from(format!(
                "{}/{}_pkg.oats.meta",
                out_dir,
                pkg_name.replace('-', "_")
            ));

            // Check if metadata needs updating
            let mut meta_needs_update = true;
            if let Ok(meta_metadata) = std::fs::metadata(&meta_file)
                && let Ok(meta_mtime) = meta_metadata.modified()
                && meta_mtime >= obj_mtime
            {
                meta_needs_update = false;
            }

            if meta_needs_update {
                // Ensure output directory exists
                if let Some(parent) = meta_file.parent() {
                    std::fs::create_dir_all(parent).map_err(|e| ToastyError::io(&meta_file, e))?;
                }

                // Try to load manifest for package information
                let manifest_info = if let Ok(manifest) =
                    crate::project::Manifest::from_file(&pkg_node.root_dir.join("Oats.toml"))
                {
                    format!(
                        "name = \"{}\"\nversion = \"{}\"\nentry = \"{}\"\n",
                        manifest.package.name, manifest.package.version, manifest.package.entry
                    )
                } else {
                    format!("name = \"{}\"\n", pkg_name)
                };

                std::fs::write(&meta_file, manifest_info)
                    .map_err(|e| ToastyError::io(&meta_file, e))?;
            }

            return Ok(PackageBuildResult {
                name: pkg_name.clone(),
                object_file: obj_path.canonicalize().unwrap_or(obj_path),
                meta_file: meta_file.canonicalize().unwrap_or(meta_file),
            });
        }
    }

    // Compile package using multi-module approach
    let compiled_modules = compile_package_modules(pkg_node, built_deps, config)?;

    // Link all modules in this package together
    link_package_modules(&compiled_modules, &obj_path, config)?;

    // Generate metadata file with package information
    let meta_file = PathBuf::from(format!(
        "{}/{}_pkg.oats.meta",
        out_dir,
        pkg_name.replace('-', "_")
    ));

    // Ensure output directory exists
    if let Some(parent) = meta_file.parent() {
        std::fs::create_dir_all(parent).map_err(|e| ToastyError::io(&meta_file, e))?;
    }

    // Try to load manifest for package information
    let manifest_info = if let Ok(manifest) =
        crate::project::Manifest::from_file(&pkg_node.root_dir.join("Oats.toml"))
    {
        format!(
            "name = \"{}\"\nversion = \"{}\"\nentry = \"{}\"\n",
            manifest.package.name, manifest.package.version, manifest.package.entry
        )
    } else {
        format!("name = \"{}\"\n", pkg_name)
    };

    std::fs::write(&meta_file, manifest_info).map_err(|e| ToastyError::io(&meta_file, e))?;

    Ok(PackageBuildResult {
        name: pkg_name.clone(),
        object_file: obj_path.canonicalize().unwrap_or(obj_path),
        meta_file: meta_file.canonicalize().unwrap_or(meta_file),
    })
}

/// Compile all modules within a single package
fn compile_package_modules(
    pkg_node: &PackageNode,
    built_deps: &HashMap<String, PackageBuildResult>,
    config: &BuildConfig,
) -> Result<HashMap<String, PathBuf>> {
    use crate::project::{build_dependency_graph, topological_sort};

    let pkg_name = &pkg_node.name;
    let entry_point = pkg_node.entry_point();

    // Build dependency graph starting from entry point
    let (dep_graph, node_indices, _entry_node) =
        build_dependency_graph(&entry_point.to_string_lossy(), config.verbose)?;

    // Perform topological sort to get compilation order
    let compilation_order = topological_sort(&dep_graph, &node_indices)?;

    let mut compiled_modules = HashMap::new();

    for (i, module_path) in compilation_order.iter().enumerate() {
        if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
            eprintln!(
                "    Compiling module {}/{}: {}",
                i + 1,
                compilation_order.len(),
                module_path
            );
        }

        let mut options = crate::cli::CompileOptions::new(module_path.clone());

        // Set up extern-pkg for package dependencies
        for (dep_name, dep_result) in built_deps {
            // Make meta file path relative to current package root
            let current_pkg_root = pkg_node.root_dir.as_path();
            let meta_path = pathdiff::diff_paths(&dep_result.meta_file, current_pkg_root)
                .unwrap_or_else(|| dep_result.meta_file.clone())
                .to_string_lossy()
                .to_string();
            options.extern_pkg.insert(dep_name.clone(), meta_path);
        }

        // Set up extern_oats for previously compiled modules in this package
        for (j, prev_module_path) in compilation_order.iter().enumerate() {
            if j >= i {
                break; // Only look at previously compiled modules
            }

            // Extract exported symbols from the previous module
            if let Ok(exported_symbols) = extract_exported_symbols(prev_module_path)
                && !exported_symbols.is_empty()
            {
                let symbol_list = exported_symbols.join(",");
                let module_name = format!(
                    "./{}",
                    std::path::Path::new(prev_module_path)
                        .file_stem()
                        .unwrap_or_default()
                        .to_string_lossy()
                );
                options.extern_oats.insert(module_name, symbol_list);
            }
        }

        // Configure output for this module
        let out_dir = config.out_dir.as_deref().unwrap_or("target");
        std::fs::create_dir_all(out_dir).map_err(|e| ToastyError::io(out_dir, e))?;

        // Create CACHEDIR.TAG to mark this as a cache directory
        create_cache_dir_tag(out_dir)?;

        options.out_dir = Some(out_dir.to_string());
        options.out_name = Some(format!("{}_mod_{}", pkg_name.replace('-', "_"), i));
        options.emit_object_only = true;
        options.link_runtime = false; // Don't link runtime for individual modules

        // Apply build settings
        options.opt_level = config.opt_level.clone();
        options.lto = config.lto.clone();
        options.target_triple = config.target_triple.clone();
        options.target_cpu = config.target_cpu.clone();
        options.target_features = config.target_features.clone();
        options.build_profile = if config.release {
            Some("release".to_string())
        } else {
            None
        };

        // Compile this module
        let compile_result = crate::compiler::invoke_oatsc(&options)?;
        if compile_result.is_some() {
            // Move generated files to target directory
            let source_stem = std::path::Path::new(module_path)
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();

            let ll_file = std::path::Path::new(&source_stem).with_extension("ll");
            let o_file = std::path::Path::new(&source_stem).with_extension("o");
            let target_ll = std::path::Path::new(out_dir).join(format!("{}.ll", source_stem));
            let target_o = std::path::Path::new(out_dir).join(format!("{}.o", source_stem));

            // Move .ll file if it exists
            if ll_file.exists() {
                std::fs::rename(&ll_file, &target_ll).map_err(|e| ToastyError::io(&ll_file, e))?;
            }

            // Move .o file if it exists
            if o_file.exists() {
                std::fs::rename(&o_file, &target_o).map_err(|e| ToastyError::io(&o_file, e))?;
            }

            // Return the path to the moved .o file
            compiled_modules.insert(module_path.clone(), target_o);
        }
    }

    Ok(compiled_modules)
}

/// Link all modules within a package into a single object file
fn link_package_modules(
    compiled_modules: &HashMap<String, PathBuf>,
    output_path: &Path,
    config: &BuildConfig,
) -> Result<()> {
    if compiled_modules.is_empty() {
        return Ok(());
    }

    // If only one module, just copy it
    if compiled_modules.len() == 1 {
        let (_module_path, obj_path) = compiled_modules.iter().next().unwrap();
        std::fs::copy(obj_path, output_path).map_err(|e| ToastyError::io(output_path, e))?;
        return Ok(());
    }

    // Link multiple modules together
    let linker_cmd = config.linker.as_deref().unwrap_or("clang");
    let mut link_cmd = std::process::Command::new(linker_cmd);

    // Ensure output directory exists
    if let Some(parent) = output_path.parent() {
        std::fs::create_dir_all(parent).map_err(|e| ToastyError::io(parent, e))?;
    }

    // Create relocatable object file
    link_cmd.arg("-r");
    link_cmd.arg("-o").arg(output_path);

    for obj_path in compiled_modules.values() {
        link_cmd.arg(obj_path);
    }

    if config.verbose {
        eprintln!("Package link command: {:?}", link_cmd);
    }

    let output = link_cmd
        .output()
        .map_err(|e| ToastyError::other(format!("Failed to run linker: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(ToastyError::LinkerFailed {
            code: output.status.code(),
            stderr,
        });
    }

    Ok(())
}

/// Link all compiled packages into final executable
fn link_packages(
    root_pkg_name: &str,
    build_results: &HashMap<String, PackageBuildResult>,
    config: &BuildConfig,
) -> Result<PathBuf> {
    let out_dir = config.out_dir.as_deref().unwrap_or(".");
    let exe_name = config
        .out_name
        .as_deref()
        .unwrap_or(&build_results[root_pkg_name].name);
    let exe_path = PathBuf::from(out_dir).join(exe_name);

    // Ensure runtime is available
    let runtime_lib = if let Ok(runtime_path) = std::env::var("OATS_RUNTIME_PATH") {
        // Use explicitly specified runtime path
        let runtime_lib = PathBuf::from(runtime_path);
        if !runtime_lib.exists() {
            return Err(ToastyError::io(
                &runtime_lib,
                std::io::Error::new(std::io::ErrorKind::NotFound, "Runtime library not found"),
            ));
        }
        runtime_lib
    } else {
        // Look for runtime in current directory or standard locations
        let runtime_lib = PathBuf::from("libruntime.a");

        if !runtime_lib.exists() {
            return Err(ToastyError::io(
                &runtime_lib,
                std::io::Error::new(std::io::ErrorKind::NotFound, "Runtime library not found"),
            ));
        }
        runtime_lib
    };

    // Ensure std is available
    let std_lib = if let Ok(std_path) = std::env::var("OATS_STD_PATH") {
        // Use explicitly specified std path
        let std_lib = PathBuf::from(std_path);
        if !std_lib.exists() {
            return Err(ToastyError::io(
                &std_lib,
                std::io::Error::new(std::io::ErrorKind::NotFound, "Std library not found"),
            ));
        }
        std_lib
    } else {
        // Look for std in current directory or standard locations
        let std_lib = PathBuf::from("liboats_std.a");
        if !std_lib.exists() {
            let std_lib = PathBuf::from("target/release/liboats_std.a");
            if !std_lib.exists() {
                return Err(ToastyError::io(
                    &std_lib,
                    std::io::Error::new(std::io::ErrorKind::NotFound, "Std library not found"),
                ));
            }
            std_lib
        } else {
            std_lib
        }
    };

    // Build link command
    let mut link_cmd = Command::new("clang");
    link_cmd.arg("-o").arg(&exe_path);

    // Add runtime library if requested
    if !config.no_link_runtime && std::env::var("OATS_STD_PATH").is_err() {
        link_cmd.arg(&runtime_lib);
    }

    // Add std library
    link_cmd.arg(&std_lib);

    // Add all object files in dependency order
    for result in build_results.values() {
        link_cmd.arg(&result.object_file);
    }

    // Add linker if specified
    if let Some(linker) = &config.linker {
        link_cmd.arg("-fuse-ld").arg(linker);
    }

    if config.verbose {
        eprintln!("Link command: {:?}", link_cmd);
    }

    let output = link_cmd
        .output()
        .map_err(|e| ToastyError::other(format!("Failed to run clang linker: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        return Err(ToastyError::LinkerFailed {
            code: output.status.code(),
            stderr,
        });
    }

    if !config.quiet && (config.verbose || cfg!(debug_assertions)) {
        eprintln!("Build finished: {}", exe_path.display());
    }

    Ok(exe_path)
}

/// Create a CACHEDIR.TAG file in the specified directory to mark it as a cache directory.
/// This follows the CACHEDIR.TAG specification: https://bford.info/cachedir/
fn create_cache_dir_tag(dir_path: &str) -> Result<()> {
    use std::io::Write;

    let tag_path = std::path::Path::new(dir_path).join("CACHEDIR.TAG");

    // Only create if it doesn't already exist
    if !tag_path.exists() {
        let mut file =
            std::fs::File::create(&tag_path).map_err(|e| ToastyError::io(&tag_path, e))?;

        // Write the standard CACHEDIR.TAG content
        writeln!(file, "Signature: 8a477f597d28d172789f06886806bc55")
            .map_err(|e| ToastyError::io(&tag_path, e))?;
        writeln!(
            file,
            "# This file is a cache directory tag created by toasty."
        )
        .map_err(|e| ToastyError::io(&tag_path, e))?;
        writeln!(
            file,
            "# For information about cache directory tags see https://bford.info/cachedir/"
        )
        .map_err(|e| ToastyError::io(&tag_path, e))?;
    }

    Ok(())
}

/// Extract exported function names from an Oats source file
fn extract_exported_symbols(file_path: &str) -> Result<Vec<String>> {
    use std::fs;

    let content = fs::read_to_string(file_path).map_err(|e| ToastyError::io(file_path, e))?;

    let mut exported_symbols = Vec::new();

    // Simple regex-like parsing for "export function name("
    for line in content.lines() {
        let line = line.trim();
        if line.starts_with("export function ")
            && let Some(start) = line.find("export function ")
        {
            let after_export = &line[start + "export function ".len()..];
            if let Some(end) = after_export.find('(') {
                let function_name = after_export[..end].trim();
                exported_symbols.push(function_name.to_string());
            }
        }
    }

    Ok(exported_symbols)
}
