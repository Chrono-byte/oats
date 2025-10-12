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
//! - Attempts `index.ts`/`index.oats` fallbacks for directory imports
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

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{OatsType, SymbolTable, check_function_strictness};

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

// Check for required external binaries and return a helpful error if missing.
fn preflight_check() -> anyhow::Result<()> {
    use std::process::Command;

    // Check rustc
    match Command::new("rustc").arg("--version").status() {
        Ok(s) if s.success() => {}
        Ok(_) => anyhow::bail!("`rustc` present but returned non-zero when invoked with --version"),
        Err(e) => {
            if e.kind() == std::io::ErrorKind::NotFound {
                anyhow::bail!(
                    "`rustc` not found in PATH; please install Rust toolchain (rustup) or ensure `rustc` is available"
                )
            } else {
                return Err(e.into());
            }
        }
    }

    // Try clang candidates (unversioned or common versioned names)
    let clang_candidates = ["clang", "clang-18", "clang-17"];
    let mut any_ok = false;
    for &c in &clang_candidates {
        match Command::new(c).arg("--version").status() {
            Ok(s) if s.success() => {
                any_ok = true;
                break;
            }
            Ok(_) => {
                // Found binary but it returned non-zero; continue looking
            }
            Err(_e) => {
                // not found; try next
            }
        }
    }
    if !any_ok {
        anyhow::bail!(
            "`clang` not found in PATH (tried clang, clang-18, clang-17). Please install clang or add a symlink to a versioned clang binary."
        );
    }

    Ok(())
}

fn main() -> Result<()> {
    // Preflight dependency check to fail fast with a clear message on CI
    preflight_check()?;
    // Resolve source file location from command-line arguments or environment
    let args: Vec<String> = std::env::args().collect();
    let src_path = if args.len() > 1 {
        args[1].clone()
    } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
        p
    } else {
        anyhow::bail!(
            "No source file provided. Pass path as first arg or set OATS_SRC_FILE env var."
        );
    };

    // ARCHITECTURE: Transitive module loading with dependency resolution.
    // The compiler loads and parses modules transitively, resolving relative imports
    // and maintaining a dependency graph to prevent cycles and duplicate processing.
    // Module paths are canonicalized to absolute paths for consistent keying.
    use std::collections::{HashMap, VecDeque};
    let mut modules: HashMap<String, parser::ParsedModule> = HashMap::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    // Initialize module loading with the entry point file
    let entry_abs = std::fs::canonicalize(&src_path)?;
    let entry_str = entry_abs.to_string_lossy().to_string();
    queue.push_back(entry_str.clone());

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
    /// 1. **Direct file matching**: Tries `.ts`, `.oats`, and extension-less variants
    /// 2. **Directory resolution**: Attempts `index.ts`/`index.oats` for directories
    /// 3. **Index fallbacks**: Additional index file patterns for compatibility
    fn resolve_relative_import(from: &str, spec: &str) -> Option<String> {
        let base = std::path::Path::new(from)
            .parent()
            .unwrap_or_else(|| std::path::Path::new("."));
        let candidate = base.join(spec);
        let exts = [".oats", ".ts", ""]; // Prioritize .ts, then .oats, then raw

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
            for idx in &["index.ts", "index.oats", "index"] {
                let c = candidate.join(idx);
                if c.exists()
                    && let Ok(cabs) = std::fs::canonicalize(&c)
                {
                    return Some(cabs.to_string_lossy().to_string());
                }
            }
        }

        // Additional index file resolution for edge cases
        for idx in &["index.ts", "index.oats", "index"] {
            let c = candidate.join(idx);
            if c.exists()
                && let Ok(cabs) = std::fs::canonicalize(&c)
            {
                return Some(cabs.to_string_lossy().to_string());
            }
        }

        None
    }

    // PHASE 1: Transitive module loading and dependency resolution
    while let Some(path) = queue.pop_front() {
        if modules.contains_key(&path) {
            continue; // Skip already processed modules
        }
        let src = std::fs::read_to_string(&path)?;
        let parsed = parser::parse_oats_module(&src, Some(&path))?;

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
                    queue.push_back(fpath);
                }
            }
        }
        modules.insert(path.clone(), parsed);
    }

    // PHASE 2: Symbol table construction and entry point validation
    // Extract the entry module and prepare for symbol resolution across all loaded modules
    let parsed_mod = modules
        .get(&entry_str)
        .ok_or_else(|| anyhow::anyhow!("entry module missing after load"))?;

    // REQUIREMENT: User programs must export a `main` function as the entry point.
    // Additionally, build a comprehensive symbol table by collecting exported symbols
    // from all loaded modules to enable cross-module type resolution and nominal
    // type references during the type checking phase.
    let mut func_decl_opt: Option<deno_ast::swc::ast::Function> = None;
    let mut pre_symbols = SymbolTable::new();

    // PHASE 2A: First pass - collect exported type declarations across all modules.
    // This enables cross-module type resolution for imported symbols and establishes
    // the foundation for nominal type checking throughout the compilation unit.
    let mut exports_map: std::collections::HashMap<
        String,
        std::collections::HashMap<String, OatsType>,
    > = std::collections::HashMap::new();

    // Maintain field information for type aliases that resolve to object literal types,
    // enabling their registration as nominal structs for proper code generation.
    let mut alias_fields: std::collections::HashMap<String, Vec<(String, OatsType)>> =
        std::collections::HashMap::new();
    for (mkey, parsed_module) in modules.iter() {
        let mut emap: std::collections::HashMap<String, OatsType> =
            std::collections::HashMap::new();
        let pm = &parsed_module.parsed;
        for item_ref in pm.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            {
                // Handle exported declarations
                match &decl.decl {
                    deno_ast::swc::ast::Decl::Class(c) => {
                        let name = c.ident.sym.to_string();
                        emap.insert(name.clone(), OatsType::NominalStruct(name));
                    }
                    deno_ast::swc::ast::Decl::Fn(f) => {
                        let name = f.ident.sym.to_string();
                        if name == "main" {
                            func_decl_opt = Some((*f.function).clone());
                        }
                        // Functions are not stored in the type export map at
                        // present.
                    }
                    deno_ast::swc::ast::Decl::TsInterface(iface) => {
                        let name = iface.id.sym.to_string();
                        emap.insert(name.clone(), OatsType::NominalStruct(name));
                    }
                    deno_ast::swc::ast::Decl::TsTypeAlias(type_alias) => {
                        let name = type_alias.id.sym.to_string();
                        // If the alias maps directly to a known OatsType, record it.
                        if let Some(mapped) = oats::types::map_ts_type(&type_alias.type_ann) {
                            emap.insert(name.clone(), mapped);
                        } else {
                            // If it's an object literal type (TsTypeLit), extract properties
                            use deno_ast::swc::ast;
                            if let ast::TsType::TsTypeLit(typelit) = &*type_alias.type_ann {
                                let mut fields: Vec<(String, OatsType)> = Vec::new();
                                for member in &typelit.members {
                                    if let ast::TsTypeElement::TsPropertySignature(prop) = member {
                                        // prop.key is an Expr boxed; match Identifier expressions
                                        if let ast::Expr::Ident(id) = &*prop.key {
                                            let fname = id.sym.to_string();
                                            if let Some(type_ann) = &prop.type_ann
                                                && let Some(mapped) =
                                                    oats::types::map_ts_type(&type_ann.type_ann)
                                            {
                                                fields.push((fname, mapped));
                                                continue;
                                            }
                                            fields.push((fname, OatsType::Number));
                                        }
                                    }
                                }
                                if !fields.is_empty() {
                                    // Record as a nominal struct export and stash its fields
                                    emap.insert(
                                        name.clone(),
                                        OatsType::NominalStruct(name.clone()),
                                    );
                                    alias_fields.insert(name.clone(), fields);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        if !emap.is_empty() {
            exports_map.insert(mkey.clone(), emap);
        }
    }

    // Second pass: resolve imports to exported types when possible, otherwise
    // fall back to a nominal placeholder as before.
    for (mkey, parsed_module) in modules.iter() {
        let pm = &parsed_module.parsed;
        for item_ref in pm.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::Import(import_decl) = module_decl
            {
                // Resolve import source to a canonicalized path (if relative)
                let src_val = import_decl.src.value.to_string();
                let mut resolved_mod: Option<String> = None;
                if src_val.starts_with("./") || src_val.starts_with("../") {
                    resolved_mod = resolve_relative_import(mkey, &src_val);
                }

                for spec in &import_decl.specifiers {
                    match spec {
                        deno_ast::swc::ast::ImportSpecifier::Named(named) => {
                            let local = named.local.sym.to_string();
                            // Determine the imported name (it may be `imported as local`)
                            let imported_name = if let Some(imported) = &named.imported {
                                match imported {
                                    deno_ast::swc::ast::ModuleExportName::Ident(id) => {
                                        id.sym.to_string()
                                    }
                                    deno_ast::swc::ast::ModuleExportName::Str(s) => {
                                        s.value.to_string()
                                    }
                                }
                            } else {
                                // No explicit imported name -> use local as the exported name
                                local.clone()
                            };
                            // Try to lookup in resolved_mod's exports
                            if let Some(rm) = &resolved_mod
                                && let Some(em) = exports_map.get(rm)
                                && let Some(exported_ty) = em.get(&imported_name)
                            {
                                pre_symbols.insert(local.clone(), exported_ty.clone());
                                continue;
                            }
                            // Fallback: placeholder nominal type
                            pre_symbols.insert(local.clone(), OatsType::NominalStruct(local));
                        }
                        deno_ast::swc::ast::ImportSpecifier::Default(d) => {
                            let local = d.local.sym.to_string();
                            // Default import: try to map to an `default` export if present
                            if let Some(rm) = &resolved_mod
                                && let Some(em) = exports_map.get(rm)
                                && let Some(exported_ty) = em.get("default")
                            {
                                pre_symbols.insert(local.clone(), exported_ty.clone());
                                continue;
                            }
                            pre_symbols.insert(local.clone(), OatsType::NominalStruct(local));
                        }
                        deno_ast::swc::ast::ImportSpecifier::Namespace(ns) => {
                            let local = ns.local.sym.to_string();
                            pre_symbols.insert(local.clone(), OatsType::NominalStruct(local));
                        }
                    }
                }
            }
            // Also capture top-level exported TS constructs that appear as statements
            if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref {
                if let deno_ast::swc::ast::Stmt::Decl(deno_ast::swc::ast::Decl::TsInterface(
                    interface_decl,
                )) = stmt
                {
                    let name = interface_decl.id.sym.to_string();
                    pre_symbols.insert(name.clone(), OatsType::NominalStruct(name));
                }
                if let deno_ast::swc::ast::Stmt::Decl(deno_ast::swc::ast::Decl::TsTypeAlias(
                    type_alias,
                )) = stmt
                {
                    let name = type_alias.id.sym.to_string();
                    if let Some(mapped) = oats::types::map_ts_type(&type_alias.type_ann) {
                        pre_symbols.insert(name.clone(), mapped);
                    }
                }
            }
        }
    }

    let func_decl = func_decl_opt.ok_or_else(|| {
        anyhow::anyhow!(
            "No exported `main` function found. Please export `function main(...)` in your script."
        )
    })?;

    // Type check: start with pre-collected symbols (imports/classes)
    let mut symbols = pre_symbols;
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    // LLVM setup
    let context = Context::create();
    let module = context.create_module("oats");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();
    let codegen = CodeGen {
        context: &context,
        module,
        builder,
        next_str_id: std::cell::Cell::new(0),
        string_literals: std::cell::RefCell::new(std::collections::HashMap::new()),
        f64_t: context.f64_type(),
        i64_t: context.i64_type(),
        i32_t: context.i32_type(),
        bool_t: context.bool_type(),
        i8ptr_t: context.ptr_type(inkwell::AddressSpace::default()),
        fn_print_f64: std::cell::RefCell::new(None),
        fn_print_str: std::cell::RefCell::new(None),
        fn_strlen: std::cell::RefCell::new(None),
        fn_malloc: std::cell::RefCell::new(None),
        fn_memcpy: std::cell::RefCell::new(None),
        fn_free: std::cell::RefCell::new(None),
        fn_array_alloc: std::cell::RefCell::new(None),
        fn_rc_inc: std::cell::RefCell::new(None),
        fn_rc_dec: std::cell::RefCell::new(None),
        fn_number_to_string: std::cell::RefCell::new(None),
        fn_union_box_f64: std::cell::RefCell::new(None),
        fn_union_box_ptr: std::cell::RefCell::new(None),
        fn_union_unbox_f64: std::cell::RefCell::new(None),
        fn_union_unbox_ptr: std::cell::RefCell::new(None),
        fn_rc_weak_inc: std::cell::RefCell::new(None),
        fn_rc_weak_dec: std::cell::RefCell::new(None),
        fn_rc_weak_upgrade: std::cell::RefCell::new(None),
        fn_union_get_discriminant: std::cell::RefCell::new(None),
        const_items: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_globals: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_interns: std::cell::RefCell::new(std::collections::HashMap::new()),
        current_escape_info: std::cell::RefCell::new(None),
        current_class_parent: std::cell::RefCell::new(None),
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
        closure_local_rettype: std::cell::RefCell::new(std::collections::HashMap::new()),
        last_expr_origin_local: std::cell::RefCell::new(None),
        async_await_live_sets: std::cell::RefCell::new(None),
        async_local_name_to_slot: std::cell::RefCell::new(None),
        async_resume_blocks: std::cell::RefCell::new(None),
        async_cont_blocks: std::cell::RefCell::new(None),
        async_poll_function: std::cell::RefCell::new(None),
        async_await_counter: std::cell::Cell::new(0),
        async_param_count: std::cell::Cell::new(0),
        async_local_slot_count: std::cell::Cell::new(0),
        async_poll_locals: std::cell::RefCell::new(None),
        source: &parsed_mod.source,
        mut_var_decls: parsed_mod.mut_var_decls.clone(),
        current_function_return_type: std::cell::RefCell::new(None),
        last_expr_is_boxed_union: std::cell::Cell::new(false),
        global_function_signatures: std::cell::RefCell::new(std::collections::HashMap::new()),
        symbol_table: std::cell::RefCell::new(symbols),
        nested_generic_fns: std::cell::RefCell::new(HashMap::new()),
        monomorphized_map: std::cell::RefCell::new(HashMap::new()),
    };

    // Merge collected alias_fields from earlier passes into codegen.class_fields so
    // downstream lowering can treat type aliases with object literal shapes as
    // nominal structs (best-effort for Phase 1 structural support).
    for (name, fields) in alias_fields.into_iter() {
        codegen.class_fields.borrow_mut().insert(name, fields);
    }

    // Helper: infer a simple OatsType from an expression (literals and simple arrays)
    fn infer_from_expr(e: &deno_ast::swc::ast::Expr) -> Option<OatsType> {
        use deno_ast::swc::ast;
        use deno_ast::swc::ast::Expr;
        match e {
            Expr::Lit(lit) => match lit {
                ast::Lit::Num(_) => Some(OatsType::Number),
                ast::Lit::Str(_) => Some(OatsType::String),
                ast::Lit::Bool(_) => Some(OatsType::Boolean),
                _ => None,
            },
            Expr::Array(arr) => {
                if let Some(Some(first)) = arr.elems.first() {
                    infer_from_expr(&first.expr).map(|et| OatsType::Array(Box::new(et)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // Populate class_fields for exported classes by scanning all loaded
    // modules so cross-file class declarations are captured.
    for (_mkey, parsed_module) in modules.iter() {
        let pm = &parsed_module.parsed;
        for item_ref in pm.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
            {
                let class_name = c.ident.sym.to_string();
                let mut fields: Vec<(String, OatsType)> = Vec::new();
                use deno_ast::swc::ast::{ClassMember, Expr, MemberProp, Stmt};
                // Collect explicit property declarations
                for member in &c.class.body {
                    if let ClassMember::ClassProp(prop) = member
                        && let deno_ast::swc::ast::PropName::Ident(id) = &prop.key
                    {
                        let fname = id.sym.to_string();
                        if !fields.iter().any(|(n, _)| n == &fname) {
                            fields.push((fname, OatsType::Number));
                        }
                    }
                }
                // Record constructor parameter properties (e.g., `public x: number`)
                for member in &c.class.body {
                    if let ClassMember::Constructor(cons) = member {
                        for param in &cons.params {
                            use deno_ast::swc::ast::{ParamOrTsParamProp, TsParamPropParam};
                            if let ParamOrTsParamProp::TsParamProp(ts_param) = param
                                && let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                            {
                                let fname = binding_ident.id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ty = binding_ident
                                        .type_ann
                                        .as_ref()
                                        .and_then(|ann| oats::types::map_ts_type(&ann.type_ann))
                                        .unwrap_or(OatsType::Number);
                                    fields.push((fname, ty));
                                }
                            }
                        }
                    }
                }

                // Scan constructor ASTs for `this.<ident> = <expr>` assignments
                for member in &c.class.body {
                    if let ClassMember::Constructor(cons) = member
                        && let Some(body) = &cons.body
                    {
                        for stmt in &body.stmts {
                            if let Stmt::Expr(expr_stmt) = stmt
                                && let Expr::Assign(assign) = &*expr_stmt.expr
                                && let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                    &assign.left
                            {
                                // Match a simple member assignment like `this.x = ...`
                                if let deno_ast::swc::ast::SimpleAssignTarget::Member(mem) =
                                    simple_target
                                    && matches!(&*mem.obj, Expr::This(_))
                                    && let MemberProp::Ident(ident) = &mem.prop
                                {
                                    let name = ident.sym.to_string();
                                    let inferred =
                                        infer_from_expr(&assign.right).unwrap_or(OatsType::Number);
                                    if fields.iter().all(|(n, _)| n != &name) {
                                        fields.push((name, inferred));
                                    }
                                }
                            }
                        }
                    }
                }
                if !fields.is_empty() {
                    codegen.class_fields.borrow_mut().insert(class_name, fields);
                }
            }
        }
    }

    // Also process top-level interface declarations to populate class_fields
    for (_mkey, parsed_module) in modules.iter() {
        let pm = &parsed_module.parsed;
        for item_ref in pm.program_ref().body() {
            if let deno_ast::ModuleItemRef::Stmt(deno_ast::swc::ast::Stmt::Decl(
                deno_ast::swc::ast::Decl::TsInterface(iface),
            )) = item_ref
            {
                let name = iface.id.sym.to_string();
                let mut fields: Vec<(String, OatsType)> = Vec::new();
                for member in &iface.body.body {
                    if let deno_ast::swc::ast::TsTypeElement::TsPropertySignature(prop) = member {
                        // prop.key is an Expr boxed; match Identifier expressions
                        if let deno_ast::swc::ast::Expr::Ident(id) = &*prop.key {
                            let fname = id.sym.to_string();
                            if let Some(type_ann) = &prop.type_ann
                                && let Some(mapped) = oats::types::map_ts_type(&type_ann.type_ann)
                            {
                                fields.push((fname, mapped));
                                continue;
                            }
                            // default to Number when type not specified or not mappable
                            fields.push((fname, OatsType::Number));
                        }
                    }
                }
                if !fields.is_empty() {
                    codegen.class_fields.borrow_mut().insert(name, fields);
                }
            }
        }
    }

    // Generate IR
    // Emit class methods/constructors as top-level functions so they can be
    // linked and called. We lower each ClassDecl's members into functions
    // named `<Class>_<method>` and `<Class>_ctor` for constructors.
    // Generate IR for exported classes found in all modules and use the
    // originating module's source for diagnostics.
    for (_mkey, parsed_module) in modules.iter() {
        let pm = &parsed_module.parsed;
        for item_ref in pm.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
            {
                let class_name = c.ident.sym.to_string();
                // `ClassDecl` contains an inner `class: Class` field; iterate
                // over `c.class.body` to access members.
                for member in &c.class.body {
                    use deno_ast::swc::ast::ClassMember;
                    match member {
                        ClassMember::Method(m) => {
                            // method name
                            let mname = match &m.key {
                                deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                                deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                                _ => continue,
                            };
                            // Try to type-check the method function using the symbol table
                            // stored inside `codegen` (symbols was moved into codegen earlier).
                            if let Ok(sig) = {
                                let mut symbols_ref = codegen.symbol_table.borrow_mut();
                                check_function_strictness(&m.function, &mut symbols_ref)
                            } {
                                // Prepend `this` as the first param (nominal struct pointer)
                                let mut params = Vec::new();
                                params
                                    .push(oats::types::OatsType::NominalStruct(class_name.clone()));
                                params.extend(sig.params.into_iter());
                                let ret = sig.ret;
                                let fname = format!("{}_{}", class_name, mname);
                                codegen
                                    .gen_function_ir(
                                        &fname,
                                        &m.function,
                                        &params,
                                        &ret,
                                        Some("this"),
                                    )
                                    .map_err(|d| {
                                        oats::diagnostics::emit_diagnostic(
                                            &d,
                                            Some(parsed_module.source.as_str()),
                                        );
                                        anyhow::anyhow!("{}", d.message)
                                    })?;
                            } else {
                                // If strict check failed (e.g., missing return annotation), try to emit with Void return
                                if let Ok(sig2) = {
                                    let mut symbols_ref = codegen.symbol_table.borrow_mut();
                                    check_function_strictness(&m.function, &mut symbols_ref)
                                } {
                                    let mut params = Vec::new();
                                    params.push(oats::types::OatsType::NominalStruct(
                                        class_name.clone(),
                                    ));
                                    params.extend(sig2.params.into_iter());
                                    let fname = format!("{}_{}", class_name, mname);
                                    codegen
                                        .gen_function_ir(
                                            &fname,
                                            &m.function,
                                            &params,
                                            &oats::types::OatsType::Void,
                                            Some("this"),
                                        )
                                        .map_err(|d| {
                                            oats::diagnostics::emit_diagnostic(
                                                &d,
                                                Some(parsed_module.source.as_str()),
                                            );
                                            anyhow::anyhow!("{}", d.message)
                                        })?;
                                }
                            }
                        }
                        ClassMember::Constructor(ctor) => {
                            // Emit full constructor with field initialization
                            let fields = codegen
                                .class_fields
                                .borrow()
                                .get(&class_name)
                                .cloned()
                                .unwrap_or_default();
                            if let Err(d) =
                                codegen.gen_constructor_ir(&class_name, ctor, &fields, None)
                            {
                                oats::diagnostics::emit_diagnostic(
                                    &d,
                                    Some(parsed_module.source.as_str()),
                                );
                                return Err(anyhow::anyhow!(d.message));
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    // Emit the user's exported `main` under an internal symbol name to avoid
    // conflicting with the C runtime entrypoint. The script must export
    // `main`, but we generate `oats_main` as the emitted symbol the host
    // runtime will call.
    codegen
        .gen_function_ir(
            "oats_main",
            &func_decl,
            &func_sig.params,
            &func_sig.ret,
            None,
        )
        .map_err(|d| {
            oats::diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
            anyhow::anyhow!("{}", d.message)
        })?;

    // Print IR
    println!("{}", codegen.module.print_to_string().to_string());

    Ok(())
}
