//! Project and package management module
//!
//! This module provides unified operations for understanding Oats project
//! structure, dependencies, and source files. It combines manifest parsing,
//! package dependency resolution, and module resolution into a cohesive API.

use crate::error::{Diagnostic, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::path::{Path, PathBuf};

/// Dependency graph for Oats modules
pub type DependencyGraph = petgraph::Graph<String, (), petgraph::Directed>;

/// Node index in the dependency graph
pub type NodeIndex = petgraph::graph::NodeIndex;

/// Package dependency graph
pub type PackageGraph = petgraph::Graph<PackageNode, (), petgraph::Directed>;

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
    let mut parts = version.splitn(2, ['-', '+']);
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
    if let Some(suf) = suffix
        && (suf.is_empty()
            || !suf
                .chars()
                .all(|c| c.is_alphanumeric() || c == '.' || c == '-' || c == '+'))
    {
        return false;
    }

    true
}

/// Represents a package node in the dependency graph
#[derive(Debug, Clone)]
pub struct PackageNode {
    /// Package name
    pub name: String,
    /// Absolute path to the package root directory (where Oats.toml is located)
    pub root_dir: PathBuf,
    /// Parsed manifest
    pub manifest: Manifest,
}

impl PackageNode {
    /// Get the absolute path to the package entry point
    pub fn entry_point(&self) -> PathBuf {
        self.manifest.entry_point(&self.root_dir)
    }
}

impl Manifest {
    /// Parse a manifest from a TOML string
    pub fn from_toml(content: &str) -> Result<Self> {
        let manifest: Self = toml::from_str(content)?;
        manifest.validate()?;
        Ok(manifest)
    }

    /// Load manifest from a file path
    pub fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Self::from_toml(&content)
    }

    /// Find and load manifest from current directory or parent directories
    pub fn discover() -> Result<Option<(Self, PathBuf)>> {
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
            return Err(Diagnostic::new("Package name cannot be empty"));
        }

        if !self
            .package
            .name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        {
            return Err(Diagnostic::new(
                "Package name must contain only alphanumeric characters, hyphens, and underscores",
            ));
        }

        // Validate version (semver format)
        if self.package.version.is_empty() {
            return Err(Diagnostic::new("Package version cannot be empty"));
        }

        // Validate entry point is not empty
        if self.package.entry.is_empty() {
            return Err(Diagnostic::new("Package entry point cannot be empty"));
        }

        if !is_valid_semver(&self.package.version) {
            return Err(Diagnostic::new(
                "Package version must be a valid semantic version (e.g., 1.0.0)",
            ));
        }

        // Validate dependencies
        for (name, dep) in &self.dependencies {
            match dep {
                Dependency::Detailed(spec) => {
                    if spec.path.is_none() && spec.version.is_none() {
                        return Err(Diagnostic::new(format!(
                            "Dependency '{}' must specify either 'path' or 'version'",
                            name
                        )));
                    }
                    if let Some(version) = &spec.version
                        && !is_valid_semver(version)
                    {
                        return Err(Diagnostic::new(format!(
                            "Dependency '{}' version '{}' is not a valid semantic version",
                            name, version
                        )));
                    }
                }
                Dependency::Version(version) => {
                    if !is_valid_semver(version) {
                        return Err(Diagnostic::new(format!(
                            "Dependency '{}' version '{}' is not a valid semantic version",
                            name, version
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    /// Get the absolute path to the entry point for this package
    pub fn entry_point(&self, manifest_dir: &Path) -> PathBuf {
        manifest_dir.join(&self.package.entry)
    }
}

/// Resolves relative import specifiers to absolute file paths.
///
/// This function implements the module resolution algorithm for relative imports,
/// supporting common TypeScript/JavaScript conventions including file extension
/// inference and index file fallbacks for directory imports.
///
/// # Arguments
/// * `from` - Absolute path of the importing module
/// * `spec` - Relative import specifier (e.g., "./module", "../utils")
/// * `project_root` - Canonical absolute path of the project root directory
///
/// # Returns
/// Canonical absolute path string if a matching file is found, `None` otherwise
///
/// # Resolution Strategy
/// 1. **Direct file matching**: Tries `.oats`, and extension-less variants
/// 2. **Directory resolution**: Attempts `index.oats` for directories
/// 3. **Index fallbacks**: Additional index file patterns for compatibility
/// 4. **Security check**: Ensures resolved path is within project root
pub fn resolve_relative_import(from: &str, spec: &str, project_root: &Path) -> Option<String> {
    // Canonicalize the project root to ensure consistent path comparison
    let canonical_root =
        std::fs::canonicalize(project_root).unwrap_or_else(|_| project_root.to_path_buf());

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
            && cabs.starts_with(&canonical_root)
        {
            return Some(cabs.to_string_lossy().to_string());
        }
    }

    // Handle directory imports with index file resolution
    if candidate.exists()
        && candidate.is_dir()
        && let Some(index_path) = resolve_index_file(&candidate)
        && let Ok(index_cabs) = std::fs::canonicalize(&index_path)
        && index_cabs.starts_with(&canonical_root)
    {
        return Some(index_path);
    }

    // Additional index file resolution for edge cases
    if let Some(index_path) = resolve_index_file(&candidate)
        && let Ok(index_cabs) = std::fs::canonicalize(&index_path)
        && index_cabs.starts_with(&canonical_root)
    {
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
pub fn load_modules(entry_path: &str) -> Result<HashMap<String, oatsc::parser::ParsedModule>> {
    load_modules_with_verbosity(entry_path, false)
}

/// Performs transitive module loading with optional verbose output.
pub fn load_modules_with_verbosity(
    entry_path: &str,
    verbose: bool,
) -> Result<HashMap<String, oatsc::parser::ParsedModule>> {
    // Validate entry path exists and is readable
    if !std::path::Path::new(entry_path).exists() {
        return Err(Diagnostic::new(format!(
            "Entry file not found: {}",
            entry_path
        )));
    }

    let mut modules: HashMap<String, oatsc::parser::ParsedModule> = HashMap::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    // Initialize module loading with the entry point file
    let entry_abs = std::fs::canonicalize(entry_path)?;
    let entry_str = entry_abs.to_string_lossy().to_string();
    let project_root = entry_abs.parent().unwrap_or(&entry_abs);
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
            .map_err(|e| Diagnostic::new(format!("Failed to read file {}: {}", path, e)))?;
        let (parsed_opt, parse_diags) = oatsc::parser::parse_oats_module(&src, Some(&path))
            .map_err(|e| Diagnostic::new(format!("Failed to parse module {}: {}", path, e)))?;

        // Check if parsing was successful
        let parsed = match parsed_opt {
            Some(pm) => pm,
            None => {
                // Emit diagnostics for parsing failure
                for diag in parse_diags {
                    eprintln!("Parse error in {}: {}", path, diag.message);
                }
                return Err(Diagnostic::new(format!("Failed to parse module {}", path)));
            }
        };

        // Discover and enqueue relative imports from this module
        for item_ref in parsed.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::Import(import_decl) = module_decl
            {
                let src_val = import_decl.src.value.to_string();
                // Process only relative import paths (absolute imports are not supported)
                if (src_val.starts_with("./") || src_val.starts_with("../"))
                    && let Some(fpath) = resolve_relative_import(&path, &src_val, project_root)
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

/// Build a dependency graph starting from an entry point.
///
/// This function discovers all module dependencies and builds a directed graph
/// where an edge A -> B means module A imports from module B (A depends on B).
///
/// # Arguments
/// * `entry_path` - Path to the entry point source file
/// * `verbose` - If true, print debug information about graph construction
///
/// # Returns
/// A tuple of (dependency graph, node index map, entry node index)
pub fn build_dependency_graph(
    entry_path: &str,
    verbose: bool,
) -> Result<(DependencyGraph, HashMap<String, NodeIndex>, NodeIndex)> {
    let mut graph = DependencyGraph::new();
    let mut node_indices = HashMap::new();

    // Validate entry path exists
    if !std::path::Path::new(entry_path).exists() {
        return Err(Diagnostic::new(format!(
            "Entry file not found: {}",
            entry_path
        )));
    }

    // Canonicalize entry path
    let entry_abs = std::fs::canonicalize(entry_path)?;
    let entry_str = entry_abs.to_string_lossy().to_string();
    let project_root = entry_abs.parent().unwrap_or(&entry_abs);

    // Add entry node
    let entry_node = graph.add_node(entry_str.clone());
    node_indices.insert(entry_str.clone(), entry_node);

    // Queue for BFS traversal
    let mut queue: VecDeque<String> = VecDeque::new();
    queue.push_back(entry_str.clone());

    // PHASE 1: Build dependency graph
    while let Some(current_path) = queue.pop_front() {
        if verbose {
            eprintln!("Analyzing dependencies for: {}", current_path);
        }

        // Parse the current file to discover imports
        let source = std::fs::read_to_string(&current_path)
            .map_err(|e| Diagnostic::new(format!("Failed to read file {}: {}", current_path, e)))?;
        let (parsed_opt, parse_diags) =
            oatsc::parser::parse_oats_module(&source, Some(&current_path)).map_err(|e| {
                Diagnostic::new(format!("Failed to parse module {}: {}", current_path, e))
            })?;

        // Check if parsing was successful
        let parsed = match parsed_opt {
            Some(pm) => pm,
            None => {
                // Emit diagnostics for parsing failure
                for diag in parse_diags {
                    eprintln!("Parse error in {}: {}", current_path, diag.message);
                }
                continue;
            }
        };

        // Get current node
        let current_node = *node_indices.get(&current_path).unwrap();

        // Discover and enqueue relative imports from this module
        for item_ref in parsed.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::Import(import_decl) = module_decl
            {
                let import_src = import_decl.src.value.to_string();
                // Process only relative import paths
                if (import_src.starts_with("./") || import_src.starts_with("../"))
                    && let Some(imported_path) =
                        resolve_relative_import(&current_path, &import_src, project_root)
                {
                    // Add dependency edge: current -> imported (current depends on imported)
                    let imported_node =
                        if let Some(&existing_node) = node_indices.get(&imported_path) {
                            existing_node
                        } else {
                            let new_node = graph.add_node(imported_path.clone());
                            node_indices.insert(imported_path.clone(), new_node);
                            queue.push_back(imported_path.clone());
                            if verbose {
                                eprintln!("  Discovered new dependency: {}", imported_path);
                            }
                            new_node
                        };

                    // Add edge: current depends on imported
                    graph.add_edge(current_node, imported_node, ());
                }
            }
        }
    }

    let entry_node_index = *node_indices.get(&entry_str).unwrap();
    Ok((graph, node_indices, entry_node_index))
}

/// Perform topological sort on the dependency graph.
///
/// Returns a vector of module paths in compilation order (dependencies first).
/// If there are cycles, returns an error with the cycle information.
pub fn topological_sort(
    graph: &DependencyGraph,
    node_indices: &HashMap<String, NodeIndex>,
) -> Result<Vec<String>> {
    // Create reverse mapping from node index to path
    let mut index_to_path = HashMap::new();
    for (path, &node) in node_indices {
        index_to_path.insert(node, path.clone());
    }

    // Perform topological sort
    match petgraph::algo::toposort(graph, None) {
        Ok(sorted_nodes) => {
            // Convert back to paths and reverse (we want dependencies first)
            let mut sorted_paths: Vec<String> = sorted_nodes
                .into_iter()
                .filter_map(|node| index_to_path.get(&node).cloned())
                .collect();
            sorted_paths.reverse(); // Dependencies come first
            Ok(sorted_paths)
        }
        Err(cycle) => {
            // Try to extract cycle information
            let cycle_info = format!("Cycle detected involving node {:?}", cycle.node_id());
            Err(Diagnostic::new(format!("Dependency cycle: {}", cycle_info)))
        }
    }
}

/// Build a package dependency graph starting from a root package
///
/// # Arguments
/// * `root_manifest_path` - Path to the root Oats.toml file
/// * `verbose` - If true, print debug information
///
/// # Returns
/// A tuple of (graph, node map by package name, root node index)
pub fn build_package_graph(
    root_manifest_path: &Path,
    verbose: bool,
) -> Result<(PackageGraph, HashMap<String, NodeIndex>, NodeIndex)> {
    let mut graph = PackageGraph::new();
    let mut node_map: HashMap<String, NodeIndex> = HashMap::new();
    let mut path_to_node: HashMap<PathBuf, NodeIndex> = HashMap::new();

    // Load root manifest
    let root_manifest = Manifest::from_file(root_manifest_path)?;

    root_manifest.validate()?;

    let root_dir = root_manifest_path
        .parent()
        .ok_or(Diagnostic::new(
            "Manifest path has no parent directory".to_string(),
        ))?
        .to_path_buf();

    if verbose {
        eprintln!("Building package graph from: {}", root_dir.display());
        eprintln!("  Root package: {}", root_manifest.package.name);
    }

    // Create root node
    let root_node = PackageNode {
        name: root_manifest.package.name.clone(),
        root_dir: root_dir.clone(),
        manifest: root_manifest.clone(),
    };

    let root_idx = graph.add_node(root_node);
    node_map.insert(root_manifest.package.name.clone(), root_idx);
    path_to_node.insert(root_dir.clone(), root_idx);

    // Queue for BFS traversal: (node_index, manifest_dir)
    let mut queue = std::collections::VecDeque::new();
    queue.push_back((root_idx, root_dir));

    // Build dependency graph
    while let Some((current_idx, current_dir)) = queue.pop_front() {
        // Clone the manifest to avoid borrowing issues
        let current_manifest = graph[current_idx].manifest.clone();
        let current_name = current_manifest.package.name.clone();

        if verbose {
            eprintln!("Processing package: {}", current_name);
        }

        // Process each dependency
        for (dep_name, dep_spec) in &current_manifest.dependencies {
            if verbose {
                eprintln!("  Checking dependency: {}", dep_name);
            }

            // Resolve dependency path
            let dep_path = match dep_spec {
                Dependency::Detailed(spec) => {
                    if let Some(ref path) = spec.path {
                        current_dir.join(path)
                    } else {
                        return Err(Diagnostic::new(format!(
                            "Dependency '{}' in package '{}' has no path specified (external dependencies not yet supported)",
                            dep_name, current_name
                        )));
                    }
                }
                Dependency::Version(_) => {
                    return Err(Diagnostic::new(format!(
                        "External dependencies not yet supported (dependency '{}' in package '{}')",
                        dep_name, current_name
                    )));
                }
            };

            // Canonicalize the dependency path
            let dep_dir = if dep_path.exists() {
                std::fs::canonicalize(&dep_path)?
            } else {
                return Err(Diagnostic::new(format!(
                    "Dependency path does not exist: {} (from package '{}')",
                    dep_path.display(),
                    current_name
                )));
            };

            // Look for Oats.toml in dependency directory
            let dep_manifest_path = dep_dir.join("Oats.toml");
            if !dep_manifest_path.exists() {
                return Err(Diagnostic::new(format!(
                    "No Oats.toml found in dependency '{}' at: {}",
                    dep_name,
                    dep_dir.display()
                )));
            }

            // Check if we've already processed this package
            let dep_idx = if let Some(&existing_idx) = path_to_node.get(&dep_dir) {
                // Verify the package name matches
                let existing_node = &graph[existing_idx];
                if existing_node.name != *dep_name {
                    return Err(Diagnostic::new(format!(
                        "Dependency name mismatch: package at {} is named '{}' but referenced as '{}'",
                        dep_dir.display(),
                        existing_node.name,
                        dep_name
                    )));
                }
                existing_idx
            } else {
                // Load and validate the dependency manifest
                let dep_manifest = Manifest::from_file(&dep_manifest_path)?;

                dep_manifest.validate()?;

                // Verify the package name matches
                if dep_manifest.package.name != *dep_name {
                    return Err(Diagnostic::new(format!(
                        "Package name mismatch: Oats.toml at {} declares name '{}' but is referenced as '{}'",
                        dep_dir.display(),
                        dep_manifest.package.name,
                        dep_name
                    )));
                }

                if verbose {
                    eprintln!(
                        "    Added new package: {} at {}",
                        dep_name,
                        dep_dir.display()
                    );
                }

                // Create new node
                let dep_node = PackageNode {
                    name: dep_manifest.package.name.clone(),
                    root_dir: dep_dir.clone(),
                    manifest: dep_manifest,
                };

                let new_idx = graph.add_node(dep_node);
                node_map.insert(dep_name.clone(), new_idx);
                path_to_node.insert(dep_dir.clone(), new_idx);

                // Queue for processing
                queue.push_back((new_idx, dep_dir));

                new_idx
            };

            // Add edge: current depends on dep
            graph.add_edge(current_idx, dep_idx, ());
        }
    }

    if verbose {
        eprintln!("Package graph built with {} packages", graph.node_count());
    }

    Ok((graph, node_map, root_idx))
}

/// Perform topological sort on the package graph to determine build order
///
/// Returns packages in dependency-first order (dependencies before dependents)
pub fn topological_sort_packages(
    graph: &PackageGraph,
    node_map: &HashMap<String, NodeIndex>,
) -> Result<Vec<(String, NodeIndex)>> {
    match petgraph::algo::toposort(graph, None) {
        Ok(sorted_nodes) => {
            // Create reverse mapping
            let mut index_to_name = HashMap::new();
            for (name, &idx) in node_map {
                index_to_name.insert(idx, name.clone());
            }

            // Convert to (name, index) pairs and reverse (dependencies first)
            let mut sorted: Vec<(String, NodeIndex)> = sorted_nodes
                .into_iter()
                .filter_map(|idx| index_to_name.get(&idx).map(|name| (name.clone(), idx)))
                .collect();

            sorted.reverse(); // Dependencies come first
            Ok(sorted)
        }
        Err(cycle) => Err(Diagnostic::new(format!(
            "Circular package dependency detected involving node {:?}",
            cycle.node_id()
        ))),
    }
}
