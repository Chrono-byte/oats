use anyhow::Context;
use std::collections::{HashMap, VecDeque};

/// Dependency graph for Oats modules
pub type DependencyGraph = petgraph::Graph<String, (), petgraph::Directed>;

/// Node index in the dependency graph
pub type NodeIndex = petgraph::graph::NodeIndex;

/// Resolves relative import specifiers to absolute file paths.
///
/// This function implements the module resolution algorithm for relative imports,
/// supporting common Oats/JavaScript conventions including file extension
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
pub fn resolve_relative_import(
    from: &str,
    spec: &str,
    project_root: &std::path::Path,
) -> Option<String> {
    // Canonicalize the project root to ensure consistent path comparison
    let canonical_root = std::fs::canonicalize(project_root).unwrap_or_else(|_| project_root.to_path_buf());

    let base = std::path::Path::new(from)
        .parent()
        .unwrap_or_else(|| std::path::Path::new("."));
    let candidate = base.join(spec);

    // SECURITY: Canonicalize the candidate path to resolve any .. sequences
    let canonical_candidate = std::fs::canonicalize(&candidate).ok()?;

    // SECURITY: Strictly check that the canonical candidate path starts with the canonical root
    if !canonical_candidate.starts_with(&canonical_root) {
        return None; // Reject paths outside the project root
    }

    let exts = [".oats", ""]; // Prioritize .oats, then raw

    // Attempt direct file resolution with extension inference
    for ext in &exts {
        let mut c = canonical_candidate.clone();
        if c.extension().is_none() && !ext.is_empty() {
            c.set_extension(ext.trim_start_matches('.'));
        }
        if c.exists() {
            return Some(c.to_string_lossy().to_string());
        }
    }

    // Handle directory imports with index file resolution
    if canonical_candidate.exists() && canonical_candidate.is_dir() {
        if let Some(index_path) = resolve_index_file(&canonical_candidate) {
            return Some(index_path);
        }
    }

    // Additional index file resolution for edge cases
    if let Some(index_path) = resolve_index_file(&canonical_candidate) {
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
            .with_context(|| format!("Failed to read source file: {}", path))?;
        let parsed = oatsc::parser::parse_oats_module(&src, Some(&path))
            .with_context(|| format!("Failed to parse module: {}", path))?;

        // Discover and enqueue relative imports from this module
        // Note: oats_ast doesn't yet support import statements in the AST
        // TODO: Add import statement support to oats_ast and update this code
        // For now, we skip import discovery
        // use oats_ast::*;
        // for stmt in &parsed.parsed.body {
        //     // Import statements would be handled here when added to oats_ast
        // }
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
) -> anyhow::Result<(DependencyGraph, HashMap<String, NodeIndex>, NodeIndex)> {
    let mut graph = DependencyGraph::new();
    let mut node_indices = HashMap::new();

    // Validate entry path exists
    if !std::path::Path::new(entry_path).exists() {
        anyhow::bail!("Entry file does not exist: {}", entry_path);
    }

    // Canonicalize entry path
    let entry_abs = std::fs::canonicalize(entry_path)
        .with_context(|| format!("Failed to canonicalize entry path: {}", entry_path))?;
    let entry_str = entry_abs.to_string_lossy().to_string();
    let project_root = entry_abs.parent().unwrap_or(&entry_abs);

    // Add entry node
    let entry_node = graph.add_node(entry_str.clone());
    node_indices.insert(entry_str.clone(), entry_node);

    // Queue for BFS traversal
    let mut queue = VecDeque::new();
    queue.push_back(entry_str.clone());

    // PHASE 1: Build dependency graph
    while let Some(current_path) = queue.pop_front() {
        if verbose {
            eprintln!("Analyzing dependencies for: {}", current_path);
        }

        // Parse the current file to discover imports
        let source = std::fs::read_to_string(&current_path)
            .with_context(|| format!("Failed to read source file: {}", current_path))?;
        let parsed = oatsc::parser::parse_oats_module(&source, Some(&current_path))
            .with_context(|| format!("Failed to parse module: {}", current_path))?;

        // Get current node
        let current_node = *node_indices
            .get(&current_path)
            .with_context(|| format!(
                "Internal error: node not found in dependency graph for path: {}",
                current_path
            ))?;

        // Discover and enqueue relative imports from this module
        // Note: oats_ast doesn't yet support import statements in the AST
        // TODO: Add import statement support to oats_ast and update this code
        // For now, we skip import discovery
        // use oats_ast::*;
        // for stmt in &parsed.parsed.body {
        //     // Import statements would be handled here when added to oats_ast
        // }
        let _ = parsed; // Suppress unused warning
        /*
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
        */
    }

    let entry_node_index = *node_indices
        .get(&entry_str)
        .with_context(|| format!(
            "Internal error: entry node not found in dependency graph for path: {}",
            entry_str
        ))?;
    Ok((graph, node_indices, entry_node_index))
}

/// Perform topological sort on the dependency graph.
///
/// Returns a vector of module paths in compilation order (dependencies first).
/// If there are cycles, returns an error with the cycle information.
pub fn topological_sort(
    graph: &DependencyGraph,
    node_indices: &HashMap<String, NodeIndex>,
) -> anyhow::Result<Vec<String>> {
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
            anyhow::bail!("Dependency cycle detected: {}", cycle_info);
        }
    }
}
