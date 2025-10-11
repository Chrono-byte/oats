use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{OatsType, SymbolTable, check_function_strictness};

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

fn main() -> Result<()> {
    // Read source from first CLI arg or OATS_SRC_FILE env var
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

    // src_root was previously used for resolution; keep note for future
    // enhancements. Currently unused.

    // Load and parse modules transitively. We support simple relative imports
    // Currently resolves relative module paths (./foo, ../bar). Modules are
    // keyed by canonicalized absolute paths.
    // paths to avoid duplicates and cycles.
    use std::collections::{HashMap, VecDeque};
    let mut modules: HashMap<String, parser::ParsedModule> = HashMap::new();
    let mut queue: VecDeque<String> = VecDeque::new();

    // Start with entry file
    let entry_abs = std::fs::canonicalize(&src_path)?;
    let entry_str = entry_abs.to_string_lossy().to_string();
    queue.push_back(entry_str.clone());

    // Helper: resolve a relative import specifier against a module path.
    // Tries common extensions and index file fallbacks. Returns a canonical
    // absolute path string when a file is found.
    fn resolve_relative_import(from: &str, spec: &str) -> Option<String> {
        let base = std::path::Path::new(from)
            .parent()
            .unwrap_or_else(|| std::path::Path::new("."));
        let candidate = base.join(spec);
        let exts = [".ts", ".oats", ""]; // prefer .ts then .oats then raw

        // Try direct file with extensions
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

        // If candidate is a directory or bare module, try index file fallbacks
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

        // Also try appending /index.* to the original spec (in case spec had ext)
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

    while let Some(path) = queue.pop_front() {
        if modules.contains_key(&path) {
            continue; // already loaded
        }
        let src = std::fs::read_to_string(&path)?;
        let parsed = parser::parse_oats_module(&src, Some(&path))?;
        // enqueue discovered relative imports found in this module
        // look for Import declarations and resolve relative specifiers
        for item_ref in parsed.parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
                && let deno_ast::swc::ast::ModuleDecl::Import(import_decl) = module_decl
            {
                let src_val = import_decl.src.value.to_string();
                // Only relative paths are handled by this code path.
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

    // For downstream logic we pick the entry parsed module and also have all
    // other parsed modules available in `modules` for symbol collection.
    let parsed_mod = modules
        .get(&entry_str)
        .ok_or_else(|| anyhow::anyhow!("entry module missing after load"))?;
    // `parsed_mod` is available if callers need the entry module parsed AST.

    // Require the user script to export a `main` function as the program entrypoint
    // Also pre-populate the symbol table with imported names and exported class
    // declarations so nominal type references can be resolved during typecheck.
    // We also scan all loaded modules to register exported classes, interfaces,
    // and type aliases so cross-file nominal references can be resolved.
    let mut func_decl_opt: Option<deno_ast::swc::ast::Function> = None;
    let mut pre_symbols = SymbolTable::new();

    // First pass: collect exported type-like symbols per module (classes,
    // interfaces, and type aliases) so imports can be resolved to their
    // real types when possible.
    let mut exports_map: std::collections::HashMap<
        String,
        std::collections::HashMap<String, OatsType>,
    > = std::collections::HashMap::new();
    // Collect fields for top-level type aliases that are object literal types
    // so we can register them as nominal structs for lowering.
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
        current_function_return_type: std::cell::RefCell::new(None),
        last_expr_is_boxed_union: std::cell::Cell::new(false),
        global_function_signatures: std::cell::RefCell::new(std::collections::HashMap::new()),
        symbol_table: std::cell::RefCell::new(symbols),
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
                                check_function_strictness(&m.function, &mut *symbols_ref)
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
                                    check_function_strictness(&m.function, &mut *symbols_ref)
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
                            if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields) {
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
