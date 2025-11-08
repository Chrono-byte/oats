use anyhow::Result;
use std::fs::File;
use std::io::Write;
use std::process::Command;

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;

use crate::codegen::CodeGen;
use crate::diagnostics;
use crate::parser;
use crate::types::{SymbolTable, check_function_strictness};

use inkwell::OptimizationLevel;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};

/// Compiles a source file with the given options.
///
/// This is the main entry point for compilation when you have options ready.
/// Unlike run_from_args, it doesn't mess with environment variables or CLI parsing.
///
/// # Arguments
/// * `options` - The compilation options
///
/// # Returns
/// Path to the output file on success, or None if just object emission
pub fn compile_with_options(options: crate::CompileOptions) -> Result<Option<String>> {
    // Set environment variables from options for compatibility with existing code
    // TODO: In future, pass these through the call stack instead of env vars
    unsafe {
        std::env::set_var("OATS_SRC_FILE", &options.src_file);

        if let Some(ref out_dir) = options.out_dir {
            std::env::set_var("OATS_OUT_DIR", out_dir);
        }

        if let Some(ref out_name) = options.out_name {
            std::env::set_var("OATS_OUT_NAME", out_name);
        }

        if options.emit_object_only {
            std::env::set_var("OATS_EMIT_OBJECT_ONLY", "1");
        }

        if let Some(ref opt_level) = options.opt_level {
            std::env::set_var("OATS_OPT_LEVEL", opt_level);
        }

        if let Some(ref lto) = options.lto {
            std::env::set_var("OATS_LTO", lto);
        }

        if let Some(ref target_triple) = options.target_triple {
            std::env::set_var("OATS_TARGET_TRIPLE", target_triple);
        }

        if let Some(ref target_cpu) = options.target_cpu {
            std::env::set_var("OATS_TARGET_CPU", target_cpu);
        }

        if let Some(ref target_features) = options.target_features {
            std::env::set_var("OATS_TARGET_FEATURES", target_features);
        }

        // Serialize extern_oats for the compilation pipeline
        if !options.extern_oats.is_empty() {
            let extern_oats_json = serde_json::to_string(&options.extern_oats)?;
            std::env::set_var("OATS_EXTERN_OATS", extern_oats_json);
        }
    }

    // Delegate to existing compilation logic
    let args = vec!["oatsc".to_string(), options.src_file.clone()];
    run_from_args(&args)
}

/// Executes the complete AOT compilation pipeline from source to executable.
///
/// This is the main function that handles everything: parsing, type checking, codegen, linking.
/// It takes command-line args or uses env vars for config.
///
/// Basically, it goes through:
/// - Figuring out the source file
/// - Parsing the code
/// - Generating LLVM IR
/// - Compiling to object files
/// - Linking everything together
///
/// Returns Ok(()) on success, or an error if something goes wrong.
pub fn run_from_args(args: &[String]) -> Result<Option<String>> {
    // Resolve source file location from arguments or environment
    let src_path = if args.len() > 1 {
        args[1].clone()
    } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
        p
    } else {
        anyhow::bail!(
            "No source file provided. Pass path as first arg or set OATS_SRC_FILE env var."
        );
    };

    let source = std::fs::read_to_string(&src_path)?;

    // OPTIMIZATION: Arrow functions declared as `let foo = (...) => ...` are now
    // processed directly during codegen without intermediate source transformation.
    // This eliminates redundant parsing passes and improves compilation performance.

    // Parse source into AST representation (single pass, no transformations)
    let (parsed_mod_opt, parse_diags) = parser::parse_oats_module(&source, Some(&src_path))?;

    // Emit any parsing diagnostics
    for diag in &parse_diags {
        diagnostics::emit_diagnostic(diag, Some(&source));
    }

    // Unpack the parsed module or bail if parsing failed
    let parsed_mod = match parsed_mod_opt {
        Some(pm) => pm,
        None => {
            anyhow::bail!("Parsing failed: module could not be parsed");
        }
    };

    // Use original parsed AST for all subsequent processing phases.
    // Arrow function extraction happens during codegen traversal.
    let parsed = &parsed_mod.parsed;

    // Read extern_oats from environment if provided
    // TODO: Add validation for extern_oats entries to ensure they point to valid files
    let extern_oats: std::collections::HashMap<String, String> =
        if let Ok(extern_oats_json) = std::env::var("OATS_EXTERN_OATS") {
            serde_json::from_str(&extern_oats_json)
                .map_err(|e| anyhow::anyhow!("Failed to parse OATS_EXTERN_OATS JSON: {}", e))?
        } else {
            std::collections::HashMap::new()
        };

    /// Extracts top-level arrow function declarations from variable statements.
    ///
    /// This looks for arrow functions assigned to const/let vars at the top level,
    /// since they need special treatment in codegen. It separates exported vs non-exported ones.
    ///
    /// Returns a list of (name, arrow_expr, is_exported) tuples.
    fn extract_arrow_functions(
        parsed: &oats_ast::Module,
    ) -> Vec<(String, oats_ast::ArrowExpr, bool)> {
        use oats_ast::*;
        let mut arrows = Vec::new();

        for stmt in &parsed.body {
            match stmt {
                // Process non-exported variable declarations: const/let foo = () => {}
                Stmt::VarDecl(vdecl) => {
                    if vdecl.decls.len() == 1 && !matches!(vdecl.kind, VarDeclKind::Var) {
                        let decl = &vdecl.decls[0];
                        if let Pat::Ident(binding_ident) = &decl.name
                            && let Some(init_expr) = &decl.init
                            && let Expr::Arrow(arrow) = init_expr
                        {
                            let name = binding_ident.sym.clone();
                            arrows.push((name, arrow.clone(), false));
                        }
                    }
                }
                // Note: oats_ast doesn't have separate export declarations in the AST
                // Exports are handled differently - for now we only handle non-exported
                _ => {}
            }
        }
        arrows
    }

    let arrow_functions = extract_arrow_functions(parsed);

    // SECURITY: Perform early validation to reject `var` declarations which are
    // not supported by the Oats type system. This provides clear error messaging
    // rather than allowing confusing runtime behavior in later compilation phases.

    /// Recursively scans statement AST nodes for prohibited `var` declarations.
    ///
    /// This function implements a conservative policy against `var` declarations,
    /// which have function-scoped semantics that conflict with Oats' lexical
    /// scoping model. The function recursively traverses nested statement
    /// structures to ensure comprehensive detection.
    ///
    /// # Arguments
    /// * `stmt` - Statement AST node to scan for `var` usage
    ///
    /// # Returns
    /// `true` if any `var` declarations are found, `false` otherwise
    fn stmt_contains_var(stmt: &oats_ast::Stmt) -> bool {
        use oats_ast::*;
        match stmt {
            // Distinguish `var` (function-scoped) from `let`/`const` (block-scoped).
            // Only true `var` declarations are rejected; `let` and `const` use the
            // same AST node type but have different `kind` discriminators.
            Stmt::VarDecl(vdecl) => {
                matches!(vdecl.kind, VarDeclKind::Var)
            }
            Stmt::Block(block) => {
                for s in &block.stmts {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
                false
            }
            Stmt::If(ifstmt) => {
                if stmt_contains_var(&ifstmt.cons) {
                    return true;
                }
                if let Some(alt) = &ifstmt.alt
                    && stmt_contains_var(alt)
                {
                    return true;
                }
                false
            }
            Stmt::For(forstmt) => {
                if let Some(ForInit::VarDecl(vd)) = &forstmt.init {
                    if matches!(vd.kind, VarDeclKind::Var) {
                        return true;
                    }
                }
                if stmt_contains_var(&forstmt.body) {
                    return true;
                }
                false
            }
            Stmt::While(ws) => stmt_contains_var(&ws.body),
            Stmt::DoWhile(dws) => stmt_contains_var(&dws.body),
            Stmt::Switch(swt) => {
                for case in &swt.cases {
                    for s in &case.cons {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                false
            }
            Stmt::Try(tr) => {
                // tr.block is a BlockStmt
                for s in &tr.block.stmts {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
                if let Some(handler) = &tr.handler {
                    for s in &handler.body.stmts {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                if let Some(finalizer) = &tr.finalizer {
                    for s in &finalizer.stmts {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                false
            }
            Stmt::FnDecl(fn_decl) => {
                if let Some(body) = &fn_decl.body {
                    for s in &body.stmts {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                false
            }
            _ => false,
        }
    }

    // Helper: infer a simple OatsType from an expression (literals and simple arrays)
    // NOTE: This function is now consolidated in crate::types::infer_type_from_expr

    // Walk top-level items and examine function bodies / declarations.
    // Note: var checking is already done in parser.rs, but we keep this for safety
    for stmt in &parsed.body {
        use oats_ast::*;
        if stmt_contains_var(stmt) {
            return diagnostics::report_error_and_bail(
                Some(&src_path),
                Some(&source),
                "`var` declarations are not supported. Use `let` or `const` instead.",
                Some(
                    "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`.",
                ),
            );
        }
        // If it's a function decl, also inspect its body for var
        if let Stmt::FnDecl(fdecl) = stmt
            && let Some(body) = &fdecl.body
        {
            for s in &body.stmts {
                if stmt_contains_var(s) {
                    return diagnostics::report_error_and_bail(
                        Some(&src_path),
                        Some(&source),
                        "`var` declarations are not supported. Use `let` or `const` instead.",
                        Some(
                            "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`.",
                        ),
                    );
                }
            }
        }
    }

    // Module-level body is parsed; do not print debug information here.

    // Require the user script to export a `main` function as the program entrypoint
    let mut func_decl_opt: Option<oats_ast::Function> = None;
    for stmt in &parsed.body {
        use oats_ast::*;
        if let Stmt::FnDecl(fn_decl) = stmt {
            let name = fn_decl.ident.sym.clone();
            if name == "main" {
                func_decl_opt = Some(Function {
                    params: fn_decl.params.clone(),
                    body: fn_decl.body.clone(),
                    return_type: fn_decl.return_type.clone(),
                    span: fn_decl.span.clone(),
                });
                break;
            }
        }
    }

    let func_decl: Option<oats_ast::Function> = if let Some(f) = func_decl_opt {
        Some(f)
    } else {
        // Check if we're in library mode (emitting object only)
        let emit_object_only = std::env::var("OATS_EMIT_OBJECT_ONLY").is_ok();
        if emit_object_only {
            // For library modules, we still need to compile but skip main function requirement
            // We'll continue with compilation but won't require a main function
            // The actual return will happen after object file emission
            None
        } else {
            return diagnostics::report_error_and_bail(
                Some(&src_path),
                Some(&source),
                "No exported `main` function found in script. Please export `function main(...)`.",
                Some("Scripts must export a `main` function to serve as the program entrypoint."),
            );
        }
    };

    let mut symbols = SymbolTable::new();
    let func_sig = if let Some(ref func) = func_decl {
        let (sig_opt, type_diags) = check_function_strictness(func, &mut symbols)?;
        for diag in &type_diags {
            diagnostics::emit_diagnostic(diag, Some(&source));
        }
        sig_opt
    } else {
        None
    };

    let context = Context::create();
    let module = context.create_module("oats_aot");
    // Set the module target triple to the host default so clang doesn't warn
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);

    // Declare external functions for imported symbols
    for symbols_str in extern_oats.values() {
        let symbols: Vec<&str> = symbols_str.split(',').map(|s| s.trim()).collect();
        for symbol in symbols {
            // Declare external function with default signature (no args, returns f64)
            // TODO: Use actual function signatures from metadata
            let fn_type = context.f64_type().fn_type(&[], false);
            module.add_function(symbol, fn_type, None);
        }
    }

    // Declare std functions
    // console.log functions
    let void_t = context.void_type();
    let i8ptr_t = context.ptr_type(inkwell::AddressSpace::default());
    module.add_function(
        "oats_std_console_log",
        void_t.fn_type(&[i8ptr_t.into()], false),
        None,
    );

    let builder = context.create_builder();
    let codegen = CodeGen {
        context: &context,
        module,
        builder,
        next_str_id: std::cell::Cell::new(0),
        string_literals: std::cell::RefCell::new(std::collections::HashMap::new()),
        f64_t: context.f64_type(),
        f32_t: context.f32_type(),
        i64_t: context.i64_type(),
        i32_t: context.i32_type(),
        i16_t: context.i16_type(),
        i8_t: context.i8_type(),
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
        class_fields: RefCell::new(HashMap::new()),
        fn_param_types: RefCell::new(HashMap::new()),
        loop_context_stack: RefCell::new(Vec::new()),
        current_class_parent: RefCell::new(None),
        closure_local_rettype: RefCell::new(HashMap::new()),
        last_expr_origin_local: RefCell::new(None),
        async_await_live_sets: RefCell::new(None),
        async_local_name_to_slot: RefCell::new(None),
        async_resume_blocks: RefCell::new(None),
        async_cont_blocks: RefCell::new(None),
        async_poll_function: RefCell::new(None),
        async_await_counter: Cell::new(0),
        async_param_count: Cell::new(0),
        async_local_slot_count: Cell::new(0),
        async_poll_locals: RefCell::new(None),
        source: &parsed_mod.source,
        const_items: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_globals: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_interns: std::cell::RefCell::new(std::collections::HashMap::new()),
        current_escape_info: RefCell::new(None),
        nested_generic_fns: RefCell::new(HashMap::new()),
        monomorphized_map: RefCell::new(HashMap::new()),
        mut_var_decls: parsed_mod.mut_var_decls.clone(),
        current_function_return_type: RefCell::new(None),
        last_expr_is_boxed_union: Cell::new(false),
        global_function_signatures: RefCell::new(HashMap::new()),
        symbol_table: RefCell::new(symbols),
        external_std_fns: RefCell::new(HashMap::new()),
        current_label: RefCell::new(None),
        rta_results: None,
        uses_async: Cell::new(false),
    };

    // Note: class field metadata is computed per-class when emitting
    // constructors below. We avoid a global pre-scan of the module so
    // member-field inference is based on declared/annotated types only.

    // Emit IR for class methods/constructors. Emit for both exported and
    // non-exported class declarations so constructors are available for
    // `new` expressions regardless of export status.
    for stmt in &parsed.body {
        use oats_ast::*;
        // Handle class declarations: `class Foo {}`
        if let Stmt::ClassDecl(c) = stmt {
            let class_name = c.ident.sym.clone();
            // If this class extends a parent, record the parent name so constructors
            // and `super(...)` lowering can find the parent's initializer.
            let parent_name_opt = if let Some(sc) = &c.super_class {
                if let Expr::Ident(id) = sc {
                    Some(id.sym.clone())
                } else {
                    None
                }
            } else {
                None
            };
            *codegen.current_class_parent.borrow_mut() = parent_name_opt.clone();

            // Emit members for this class
            for member in &c.body {
                match member {
                    ClassMember::Method(m) => {
                        // method name
                        let mname = m.ident.sym.clone();
                        // Convert MethodDecl to Function for type checking
                        let method_func = Function {
                            params: m.params.clone(),
                            body: m.body.clone(),
                            return_type: m.return_type.clone(),
                            span: m.span.clone(),
                        };
                        // Try to type-check the method function
                        let mut method_symbols = SymbolTable::new();
                        let (sig_opt, type_diags) =
                            check_function_strictness(&method_func, &mut method_symbols)?;

                        // Emit type checking diagnostics
                        for diag in &type_diags {
                            diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
                        }

                        if let Some(sig) = sig_opt {
                            // Prepend `this` as the first param (nominal struct pointer)
                            let mut params = Vec::new();
                            params.push(crate::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &m.function, &params, &ret, Some("this"))
                                .map_err(|d| {
                                    diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                                    anyhow::anyhow!(d.message)
                                })?;
                        } else {
                            // If strict check failed (e.g., missing return annotation), try to emit with Void return
                            let mut method_symbols = SymbolTable::new();
                            let (sig2_opt, type_diags2) =
                                check_function_strictness(&m.function, &mut method_symbols)?;

                            for diag in &type_diags2 {
                                diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
                            }

                            if let Some(sig2) = sig2_opt {
                                let mut params = Vec::new();
                                params.push(crate::types::OatsType::NominalStruct(
                                    class_name.clone(),
                                ));
                                params.extend(sig2.params.into_iter());
                                let fname = format!("{}_{}", class_name, mname);
                                codegen
                                    .gen_function_ir(
                                        &fname,
                                        &m.function,
                                        &params,
                                        &crate::types::OatsType::Void,
                                        Some("this"),
                                    )
                                    .map_err(|d| {
                                        crate::diagnostics::emit_diagnostic(
                                            &d,
                                            Some(source.as_str()),
                                        );
                                        anyhow::anyhow!(d.message)
                                    })?;
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Compute fields for this class from explicit props, constructor
                        // param properties, and `this.x = ...` assignments inside the ctor.
                        let mut fields: Vec<(String, crate::types::OatsType)> = Vec::new();
                        use deno_ast::swc::ast::{
                            ClassMember, Expr, MemberProp, ParamOrTsParamProp, Stmt,
                            TsParamPropParam,
                        };
                        // explicit class properties
                        for m in &c.class.body {
                            if let ClassMember::ClassProp(prop) = m
                                && let deno_ast::swc::ast::PropName::Ident(id) = &prop.key
                            {
                                let fname = id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    // If the class property has an Oats type annotation, map it
                                    // to an OatsType; otherwise default to Number.
                                    let ftype = if let Some(type_ann) = &prop.type_ann {
                                        if let Some(mt) =
                                            crate::types::map_ts_type(&type_ann.type_ann)
                                        {
                                            mt
                                        } else {
                                            crate::types::OatsType::Number
                                        }
                                    } else {
                                        crate::types::OatsType::Number
                                    };
                                    fields.push((fname, ftype));
                                }
                            }
                        }
                        // constructor param properties
                        for p in &ctor.params {
                            if let ParamOrTsParamProp::TsParamProp(ts_param) = p
                                && let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                            {
                                let fname = binding_ident.id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ty = crate::types::infer_type(
                                        binding_ident.type_ann.as_ref().map(|ann| &*ann.type_ann),
                                        None,
                                    );
                                    fields.push((fname, ty));
                                }
                            }
                        }
                        // scan ctor body for `this.x = ...` assignments
                        if let Some(body) = &ctor.body {
                            for stmt in &body.stmts {
                                if let Stmt::Expr(expr_stmt) = stmt
                                    && let Expr::Assign(assign) = &*expr_stmt.expr
                                    && let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                        &assign.left
                                    && let deno_ast::swc::ast::SimpleAssignTarget::Member(mem) =
                                        simple_target
                                    && matches!(&*mem.obj, Expr::This(_))
                                    && let MemberProp::Ident(ident) = &mem.prop
                                {
                                    let name = ident.sym.to_string();
                                    // Try to infer type from RHS expression. If the RHS is a
                                    // constructor parameter identifier, prefer its declared
                                    // type annotation when available.
                                    let mut inferred =
                                        crate::types::infer_type(None, Some(&assign.right));
                                    // If RHS is an identifier, try to look up a matching
                                    // constructor parameter and use its annotation.
                                    if let crate::types::OatsType::Number = inferred
                                        && let Expr::Ident(rhs_ident) = &*assign.right
                                    {
                                        for p in &ctor.params {
                                            use deno_ast::swc::ast::{
                                                ParamOrTsParamProp, TsParamPropParam,
                                            };
                                            match p {
                                                ParamOrTsParamProp::Param(param) => {
                                                    if let deno_ast::swc::ast::Pat::Ident(
                                                        bind_ident,
                                                    ) = &param.pat
                                                        && bind_ident.id.sym == rhs_ident.sym
                                                        && let Some(type_ann) = &bind_ident.type_ann
                                                        && let Some(mt) = crate::types::map_ts_type(
                                                            &type_ann.type_ann,
                                                        )
                                                    {
                                                        inferred = mt;
                                                        break;
                                                    }
                                                }
                                                ParamOrTsParamProp::TsParamProp(ts_param) => {
                                                    if let TsParamPropParam::Ident(binding_ident) =
                                                        &ts_param.param
                                                        && binding_ident.id.sym == rhs_ident.sym
                                                        && let Some(type_ann) =
                                                            &binding_ident.type_ann
                                                        && let Some(mt) = crate::types::map_ts_type(
                                                            &type_ann.type_ann,
                                                        )
                                                    {
                                                        inferred = mt;
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if fields.iter().all(|(n, _)| n != &name) {
                                        fields.push((name, inferred));
                                    }
                                }
                            }
                        }
                        // Register computed fields so lowering can reference them
                        codegen
                            .class_fields
                            .borrow_mut()
                            .insert(class_name.clone(), fields.clone());
                        if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields, None)
                        {
                            diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
                            return Err(anyhow::anyhow!(d.message));
                        }
                    }
                    _ => {}
                }
            }
            // Done emitting this class; clear current parent
            codegen.current_class_parent.borrow_mut().take();
        }

        // Also handle non-exported top-level class declarations: `class Foo {}`
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(deno_ast::swc::ast::Decl::Class(c)) = stmt
        {
            let class_name = c.ident.sym.to_string();
            let parent_name_opt = if let Some(sc) = &c.class.super_class {
                if let deno_ast::swc::ast::Expr::Ident(id) = &**sc {
                    Some(id.sym.to_string())
                } else {
                    None
                }
            } else {
                None
            };
            *codegen.current_class_parent.borrow_mut() = parent_name_opt.clone();
            // Emit members for this class
            use deno_ast::swc::ast::ClassMember;
            for member in &c.class.body {
                match member {
                    ClassMember::Method(m) => {
                        // method name
                        let mname = match &m.key {
                            deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                            deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                            _ => continue,
                        };
                        // Try to type-check the method function
                        let mut method_symbols = SymbolTable::new();
                        let (sig_opt, type_diags) =
                            check_function_strictness(&m.function, &mut method_symbols)?;

                        // Emit type checking diagnostics
                        for diag in &type_diags {
                            diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
                        }

                        if let Some(sig) = sig_opt {
                            // Prepend `this` as the first param (nominal struct pointer)
                            let mut params = Vec::new();
                            params.push(crate::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &m.function, &params, &ret, Some("this"))
                                .map_err(|d| {
                                    crate::diagnostics::emit_diagnostic(
                                        &d,
                                        Some(&parsed_mod.source),
                                    );
                                    anyhow::anyhow!(d.message)
                                })?;
                        } else {
                            // If strict check failed (e.g., missing return annotation), try to emit with Void return
                            let mut method_symbols = SymbolTable::new();
                            let (sig2_opt, type_diags2) =
                                check_function_strictness(&m.function, &mut method_symbols)?;

                            for diag in &type_diags2 {
                                diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
                            }

                            if let Some(sig2) = sig2_opt {
                                let mut params = Vec::new();
                                params.push(crate::types::OatsType::NominalStruct(
                                    class_name.clone(),
                                ));
                                params.extend(sig2.params.into_iter());
                                let fname = format!("{}_{}", class_name, mname);
                                codegen
                                    .gen_function_ir(
                                        &fname,
                                        &m.function,
                                        &params,
                                        &crate::types::OatsType::Void,
                                        Some("this"),
                                    )
                                    .map_err(|d| {
                                        crate::diagnostics::emit_diagnostic(
                                            &d,
                                            Some(source.as_str()),
                                        );
                                        anyhow::anyhow!(d.message)
                                    })?;
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Compute fields for non-exported class similarly to exported case
                        let mut fields: Vec<(String, crate::types::OatsType)> = Vec::new();
                        use deno_ast::swc::ast::{
                            ClassMember, Expr, MemberProp, ParamOrTsParamProp, Stmt,
                            TsParamPropParam,
                        };
                        for m in &c.class.body {
                            if let ClassMember::ClassProp(prop) = m
                                && let deno_ast::swc::ast::PropName::Ident(id) = &prop.key
                            {
                                let fname = id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ftype = if let Some(type_ann) = &prop.type_ann {
                                        if let Some(mt) =
                                            crate::types::map_ts_type(&type_ann.type_ann)
                                        {
                                            mt
                                        } else {
                                            crate::types::OatsType::Number
                                        }
                                    } else {
                                        crate::types::OatsType::Number
                                    };
                                    fields.push((fname, ftype));
                                }
                            }
                        }
                        for p in &ctor.params {
                            if let ParamOrTsParamProp::TsParamProp(ts_param) = p
                                && let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                            {
                                let fname = binding_ident.id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ty = crate::types::infer_type(
                                        binding_ident.type_ann.as_ref().map(|ann| &*ann.type_ann),
                                        None,
                                    );
                                    fields.push((fname, ty));
                                }
                            }
                        }
                        // scan ctor body for `this.x = ...` assignments
                        if let Some(body) = &ctor.body {
                            for stmt in &body.stmts {
                                if let Stmt::Expr(expr_stmt) = stmt
                                    && let Expr::Assign(assign) = &*expr_stmt.expr
                                    && let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                        &assign.left
                                    && let deno_ast::swc::ast::SimpleAssignTarget::Member(mem) =
                                        simple_target
                                    && matches!(&*mem.obj, Expr::This(_))
                                    && let MemberProp::Ident(ident) = &mem.prop
                                {
                                    let name = ident.sym.to_string();
                                    let inferred =
                                        crate::types::infer_type(None, Some(&assign.right));
                                    if fields.iter().all(|(n, _)| n != &name) {
                                        fields.push((name, inferred));
                                    }
                                }
                            }
                        }
                        codegen
                            .class_fields
                            .borrow_mut()
                            .insert(class_name.clone(), fields.clone());
                        if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields, None)
                        {
                            diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
                            return Err(anyhow::anyhow!(d.message));
                        }
                    }
                    _ => {}
                }
            }
            // Clear parent after emitting this class
            codegen.current_class_parent.borrow_mut().take();
        }
    }

    // Emit top-level helper functions (non-exported) found in the module so
    // calls to them can be lowered. Skip exported `main` which we handle
    // separately.
    // Before emitting functions, evaluate and emit top-level `const`
    // declarations. This pass collects all top-level consts, builds a
    // dependency graph between them, topologically sorts the graph, and
    // then evaluates each const in order using the codegen const evaluator.
    // Emitted heap-style consts (strings/arrays/objects) are added as LLVM
    // globals via `CodeGen::emit_const_global` and cached in
    // `codegen.const_globals` so expression lowering can reuse them.
    {
        use deno_ast::swc::ast;
        use std::collections::{HashMap, HashSet, VecDeque};

        // Collect top-level const declarations: name -> (init_expr, span_start)
        // We store the initializer as a boxed Expr to match AST ownership.
        let mut top_level_consts: Vec<(String, Box<ast::Expr>, usize)> = Vec::new();
        for item in parsed.program_ref().body() {
            if let deno_ast::ModuleItemRef::Stmt(stmt) = item
                && let ast::Stmt::Decl(ast::Decl::Var(vd)) = stmt
                && matches!(vd.kind, ast::VarDeclKind::Const)
            {
                for decl in &vd.decls {
                    if let ast::Pat::Ident(binding) = &decl.name {
                        if let Some(init) = &decl.init {
                            let name = binding.id.sym.to_string();
                            let span_start = vd.span.lo.0 as usize;
                            top_level_consts.push((name, init.clone(), span_start));
                        } else {
                            return diagnostics::report_error_and_bail(
                                Some(&src_path),
                                Some(&source),
                                "top-level `const` must have an initializer",
                                Some("Rust-like `const` requires compile-time initializer."),
                            );
                        }
                    }
                }
            }
        }

        if !top_level_consts.is_empty() {
            // Build a name -> index map
            let mut name_to_idx: HashMap<String, usize> = HashMap::new();
            for (i, (n, _, _)) in top_level_consts.iter().enumerate() {
                name_to_idx.insert(n.clone(), i);
            }

            // Helper: collect identifier names referenced by an expression
            fn collect_idents(e: &ast::Expr, out: &mut HashSet<String>) {
                use deno_ast::swc::ast::*;
                match e {
                    Expr::Ident(id) => {
                        out.insert(id.sym.to_string());
                    }
                    Expr::Array(arr) => {
                        for el in arr.elems.iter().flatten() {
                            collect_idents(&el.expr, out);
                        }
                    }
                    Expr::Object(obj) => {
                        for prop in &obj.props {
                            if let PropOrSpread::Prop(pb) = prop
                                && let Prop::KeyValue(kv) = &**pb
                            {
                                collect_idents(&kv.value, out);
                            }
                        }
                    }
                    Expr::Unary(u) => collect_idents(&u.arg, out),
                    Expr::Bin(b) => {
                        collect_idents(&b.left, out);
                        collect_idents(&b.right, out);
                    }
                    Expr::Call(c) => {
                        if let Callee::Expr(ec) = &c.callee {
                            collect_idents(ec, out);
                        }
                        for a in &c.args {
                            collect_idents(&a.expr, out);
                        }
                    }
                    Expr::Member(m) => {
                        collect_idents(&m.obj, out);
                        if let MemberProp::Computed(cmp) = &m.prop {
                            collect_idents(&cmp.expr, out);
                        }
                    }
                    Expr::New(n) => {
                        collect_idents(&n.callee, out);
                        if let Some(args) = &n.args {
                            for a in args {
                                collect_idents(&a.expr, out);
                            }
                        }
                    }
                    Expr::Paren(p) => collect_idents(&p.expr, out),
                    Expr::Cond(c) => {
                        collect_idents(&c.test, out);
                        collect_idents(&c.cons, out);
                        collect_idents(&c.alt, out);
                    }
                    Expr::Tpl(_) | Expr::Lit(_) => {}
                    _ => {}
                }
            }

            // Build adjacency and indegree maps only for dependencies that are
            // other top-level const names (ignore other idents).
            let mut adj: Vec<Vec<usize>> = vec![Vec::new(); top_level_consts.len()];
            let mut indeg: Vec<usize> = vec![0; top_level_consts.len()];

            for (i, (_n, init, _)) in top_level_consts.iter().enumerate() {
                let mut ids = HashSet::new();
                collect_idents(init, &mut ids);
                for id in ids {
                    if let Some(&j) = name_to_idx.get(&id) {
                        // edge j -> i (j must be evaluated before i)
                        adj[j].push(i);
                        indeg[i] += 1;
                    }
                }
            }

            // Kahn's algorithm
            let mut q: VecDeque<usize> = VecDeque::new();
            for (i, &d) in indeg.iter().enumerate() {
                if d == 0 {
                    q.push_back(i);
                }
            }
            let mut order: Vec<usize> = Vec::new();
            while let Some(u) = q.pop_front() {
                order.push(u);
                for &v in &adj[u] {
                    indeg[v] -= 1;
                    if indeg[v] == 0 {
                        q.push_back(v);
                    }
                }
            }
            if order.len() != top_level_consts.len() {
                // Cycle detected
                return diagnostics::report_error_and_bail(
                    Some(&src_path),
                    Some(&source),
                    "cyclic dependency among top-level const declarations",
                    Some(
                        "Top-level `const` declarations form a cycle; Rust-style `const` requires acyclic compile-time dependencies.",
                    ),
                );
            }

            // Evaluate consts in topo order and emit globals where needed
            for idx in order {
                let (name, init_expr, span_start) = &top_level_consts[idx];
                // Evaluate with the current const_items map
                match crate::codegen::const_eval::eval_const_expr(
                    init_expr,
                    *span_start,
                    &codegen.const_items.borrow(),
                ) {
                    Ok(cv) => {
                        // Insert into const_items
                        codegen
                            .const_items
                            .borrow_mut()
                            .insert(name.clone(), cv.clone());
                        // If heap-shaped (string/array/objects) emit a const global
                        match cv {
                            crate::codegen::const_eval::ConstValue::Str(_)
                            | crate::codegen::const_eval::ConstValue::Array(_)
                            | crate::codegen::const_eval::ConstValue::Object(_) => {
                                // Emit a global named `const.<name>` and cache pointer under the bare name
                                let gname = format!("const.{}", name);
                                match codegen.emit_const_global(&gname, &cv) {
                                    Ok(ptr) => {
                                        codegen
                                            .const_globals
                                            .borrow_mut()
                                            .insert(name.clone(), ptr);
                                    }
                                    Err(d) => {
                                        diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                                        return Err(anyhow::anyhow!(d.message));
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    Err(d) => {
                        diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                        return Err(anyhow::anyhow!(d.message));
                    }
                }
            }
        }
    }

    for item in parsed.program_ref().body() {
        use deno_ast::swc::ast;
        // non-exported function declarations: `function foo() {}`
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item
            && let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt
        {
            let fname = fdecl.ident.sym.to_string();
            let inner_func = (*fdecl.function).clone();
            let mut inner_symbols = SymbolTable::new();
            let (sig_opt, type_diags) = check_function_strictness(&inner_func, &mut inner_symbols)?;

            for diag in &type_diags {
                diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
            }

            if let Some(fsig) = sig_opt {
                // skip exported `main` (we handle exported main separately)
                if fname != "main" {
                    codegen
                        .gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None)
                        .map_err(|d| {
                            crate::diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                            anyhow::anyhow!("{}", d.message)
                        })?;
                }
            }
        }

        // exported declarations: `export function foo() {}` — emit these too
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let ast::Decl::Fn(fdecl) = &decl.decl
        {
            let fname = fdecl.ident.sym.to_string();
            let inner_func = (*fdecl.function).clone();
            let mut inner_symbols = SymbolTable::new();
            let (sig_opt, type_diags) = check_function_strictness(&inner_func, &mut inner_symbols)?;

            for diag in &type_diags {
                diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
            }

            if let Some(fsig) = sig_opt
                && fname != "main"
            {
                codegen
                    .gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None)
                    .map_err(|d| {
                        crate::diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                        anyhow::anyhow!("{}", d.message)
                    })?;
            }
        }
    }

    // Emit arrow functions declared as top-level const/let bindings
    // Example: const foo = (x: number): number => x + 1;
    for (fname, arrow, is_exported) in &arrow_functions {
        // Skip exported main - it will be handled separately
        if fname == "main" && *is_exported {
            continue;
        }
        // Skip non-exported main as well
        if fname == "main" {
            continue;
        }

        // Convert arrow function to regular function format that gen_function_ir expects
        // Arrow functions need special handling because their structure differs slightly
        use oats_ast::*;

        // Convert arrow params (Vec<Pat>) to function params (Vec<Param>)
        // For now, we need to extract type annotations from the patterns
        // This is a simplified version - in a full implementation, we'd need to
        // handle type annotations properly from the arrow function parameters
        let func_params: Vec<Param> = arrow
            .params
            .iter()
            .map(|pat| {
                // Extract type from pattern if available
                // For now, create a param with no type annotation
                Param {
                    pat: pat.clone(),
                    ty: None, // TODO: Extract type annotation from arrow param
                    span: arrow.span.clone(),
                }
            })
            .collect();

        // Convert arrow body to function body (BlockStmt)
        let func_body = match &arrow.body {
            ArrowBody::Block(block) => Some(block.clone()),
            ArrowBody::Expr(expr) => {
                // Wrap expression in a return statement
                Some(BlockStmt {
                    span: arrow.span.clone(),
                    stmts: vec![Stmt::Return(ReturnStmt {
                        span: arrow.span.clone(),
                        arg: Some(expr.as_ref().clone()),
                    })],
                })
            }
        };

        let func = Function {
            params: func_params,
            body: func_body,
            return_type: arrow.return_type.clone(),
            span: arrow.span.clone(),
        };

        let mut inner_symbols = SymbolTable::new();
        let (sig_opt, type_diags) = check_function_strictness(&func, &mut inner_symbols)?;

        for diag in &type_diags {
            diagnostics::emit_diagnostic(diag, Some(&parsed_mod.source));
        }

        if let Some(fsig) = sig_opt {
            codegen
                .gen_function_ir(fname, &func, &fsig.params, &fsig.ret, None)
                .map_err(|d| {
                    crate::diagnostics::emit_diagnostic(&d, Some(&parsed_mod.source));
                    anyhow::anyhow!("{}", d.message)
                })?;
        }
    }

    // Emit the user's exported `main` under an internal symbol name to avoid
    // conflicting with the C runtime entrypoint. The script must export
    // `main`, but we generate `oats_main` as the emitted symbol the host
    // runtime will call.
    if let Some(ref func) = func_decl
        && let Some(ref sig) = func_sig
    {
        codegen
            .gen_function_ir("oats_main", func, &sig.params, &sig.ret, None)
            .map_err(|d| {
                crate::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                anyhow::anyhow!("{}", d.message)
            })?;
    }

    // Try to emit a host `main` into the module so no external shim is
    // required. Recompute IR after emission.
    let emitted_host_main = if let Some(ref sig) = func_sig {
        codegen.emit_host_main(&sig.params, &sig.ret)
    } else {
        false
    };

    // Note: LLVM optimizations (inlining, loop opts) are applied via clang -O3 during compilation
    let ir = codegen.module.print_to_string().to_string();

    // determine output directory (optional)
    let out_dir = std::env::var("OATS_OUT_DIR").unwrap_or_else(|_| ".".to_string());
    let src_filename = std::path::Path::new(&src_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("out");
    let out_ll = format!("{}/{}.ll", out_dir, src_filename);
    // Allow overriding output name with OATS_OUT_NAME
    let out_exe = if let Ok(name) = std::env::var("OATS_OUT_NAME") {
        format!("{}/{}", out_dir, name)
    } else {
        format!("{}/{}", out_dir, src_filename)
    };
    let out_obj = format!("{}/{}.o", out_dir, src_filename);

    // Ensure output directory exists so File::create doesn't fail with ENOENT.
    if let Err(e) = std::fs::create_dir_all(&out_dir) {
        anyhow::bail!("failed to create output directory {}: {}", out_dir, e);
    }

    let mut f = File::create(&out_ll)?;
    f.write_all(ir.as_bytes())?;
    f.sync_all()?;

    // Compile IR to object file using LLVM's in-process TargetMachine. This
    // avoids shelling out to clang for the IR -> object step and is more
    // robust when used programmatically.

    // Initialize native target and get the default target triple for LLVM
    Target::initialize_native(&InitializationConfig::default()).map_err(|e| anyhow::anyhow!(e))?;
    let triple = TargetMachine::get_default_triple();

    // Read overrides (optional) for CPU/features
    let env_cpu = std::env::var("OATS_TARGET_CPU").ok();
    let env_features = std::env::var("OATS_TARGET_FEATURES").ok();
    let cpu_candidates = if let Some(c) = env_cpu.clone() {
        vec![c, "".to_string()]
    } else {
        // Prefer a generic CPU (empty) before trying 'native' which can be
        // misinterpreted for some LLVM targets (causing subtarget errors).
        vec!["".to_string(), "native".to_string()]
    };
    // Determine optimization level. Priority:
    // 1. OATS_OPT_LEVEL env var (explicit)
    // 2. OATS_BUILD_PROFILE=release -> Aggressive
    // 3. Default -> None (fastest)
    let build_profile = std::env::var("OATS_BUILD_PROFILE").unwrap_or_default();
    let opt_level = if let Ok(opt_str) = std::env::var("OATS_OPT_LEVEL") {
        match opt_str.as_str() {
            "none" | "0" => OptimizationLevel::None,
            "1" | "default" => OptimizationLevel::Default,
            "2" | "3" | "aggressive" => OptimizationLevel::Aggressive,
            _ => OptimizationLevel::Default,
        }
    } else if build_profile == "release" {
        OptimizationLevel::Aggressive
    } else {
        OptimizationLevel::None
    };
    let features = env_features.unwrap_or_default();

    // Get the target from the triple
    let target = Target::from_triple(&triple).map_err(|e| anyhow::anyhow!("{}", e))?;

    // Create TargetMachine, trying CPU candidates in order
    let mut tm = None;
    for cpu in &cpu_candidates {
        if let Some(machine) = target.create_target_machine(
            &triple,
            cpu,
            &features,
            opt_level,
            RelocMode::Default,
            CodeModel::Default,
        ) {
            tm = Some(machine);
            break;
        }
    }
    let tm =
        tm.ok_or_else(|| anyhow::anyhow!("Failed to create TargetMachine with any CPU candidate"))?;

    // Emit object file directly from the in-memory module.
    let out_obj_path = std::path::Path::new(&out_obj);
    tm.write_to_file(&codegen.module, FileType::Object, out_obj_path)
        .map_err(|e| {
            anyhow::anyhow!(
                "TargetMachine failed to emit object file {}: {:?}",
                out_obj,
                e
            )
        })?;

    // If requested, only emit the object and skip the final host linking step.
    if std::env::var("OATS_EMIT_OBJECT_ONLY").is_ok() {
        eprintln!(
            "OATS_EMIT_OBJECT_ONLY set; emitted {} and skipping link",
            out_obj
        );
        println!("{}", out_obj);
        return Ok(Some(out_obj));
    }

    // Build the runtime library
    let rust_lib = {
        let mut cargo_cmd = Command::new("cargo");
        cargo_cmd
            .arg("build")
            .arg("--release")
            .arg("--package")
            .arg("runtime");
        match cargo_cmd.status() {
            Ok(status) => {
                if !status.success() {
                    anyhow::bail!("cargo failed to build runtime");
                }
            }
            Err(e) => {
                if e.kind() == std::io::ErrorKind::NotFound {
                    anyhow::bail!("cargo not found in PATH");
                } else {
                    return Err(e.into());
                }
            }
        }
        "target/release/libruntime.a".to_string()
    };

    // Locate or produce rt_main object. Prefer an existing top-level `rt_main.o` so
    // the repo can ship a prebuilt small host object. Otherwise try to compile
    // `runtime/rt_main/src/main.rs` if it exists.
    let rt_main_obj = if emitted_host_main {
        // host main emitted into the module; no external rt_main.o requi
        String::new()
    } else if Path::new("rt_main.o").exists() {
        // Use the repo-provided object file
        String::from("rt_main.o")
    } else if Path::new("crates/runtime/rt_main/src/main.rs").exists() {
        let rt_main_obj = format!("{}/rt_main.o", out_dir);
        // Compile rt_main with rustc; check for missing rustc binary explicitly
        let mut rustc_cmd = Command::new("rustc");
        rustc_cmd
            .arg("--crate-type")
            .arg("bin")
            .arg("--emit=obj")
            .arg("crates/runtime/rt_main/src/main.rs")
            .arg("-O")
            .arg("-o")
            .arg(&rt_main_obj);
        match rustc_cmd.status() {
            Ok(status) => {
                if !status.success() {
                    anyhow::bail!("rustc failed to compile rt_main to object");
                }
            }
            Err(e) => {
                if e.kind() == std::io::ErrorKind::NotFound {
                    anyhow::bail!(
                        "`rustc` not found in PATH; please install Rust toolchain or ensure `rustc` is available"
                    );
                } else {
                    return Err(e.into());
                }
            }
        }
        rt_main_obj
    } else {
        anyhow::bail!(
            "No rt_main.o found and no runtime/rt_main/src/main.rs available; please provide a runtime main (rt_main.o) or add a runtime/rt_main/src/main.rs"
        );
    };

    // Link final binary. Prefer explicit linker via OATS_LINKER, otherwise
    // use clang (with -fuse-ld=lld when available) or fall back to ld.lld/lld.
    let oats_linker = std::env::var("OATS_LINKER").ok();

    // helper to test whether a program is runnable
    fn is_prog_available(name: &str) -> bool {
        use std::process::Stdio;
        // Respect TOASTY_VERBOSE to allow printing --version output
        let verbose = std::env::var("TOASTY_VERBOSE").is_ok();
        let status = if verbose {
            Command::new(name).arg("--version").status()
        } else {
            Command::new(name)
                .arg("--version")
                .stdout(Stdio::null())
                .stderr(Stdio::null())
                .status()
        };
        match status {
            Ok(s) => s.success(),
            Err(_) => false,
        }
    }

    // Detect available tools
    let clang_candidates = ["clang", "clang-18", "clang-17"];
    let mut found_clang: Option<String> = None;
    for &c in &clang_candidates {
        if is_prog_available(c) {
            found_clang = Some(c.to_string());
            break;
        }
    }
    // detect lld (prefer ld.lld then lld)
    let lld_candidate = if is_prog_available("ld.lld") || is_prog_available("lld") {
        Some("lld".to_string())
    } else {
        None
    };

    let mut linked_ok = false;
    let mut link_run_err: Option<std::io::Error> = None;

    if let Some(linker) = oats_linker {
        // Try the user-specified linker exactly once
        let mut cmd = Command::new(&linker);
        if !rt_main_obj.is_empty() {
            cmd.arg(&rt_main_obj);
        }
        cmd.arg(&out_obj).arg(&rust_lib).arg("-o").arg(&out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{} failed to link final binary", linker);
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else if let Some(clang_bin) = found_clang.clone() {
        // Use clang; if lld is present, prefer clang + -fuse-ld=lld so we keep clang's driver behavior
        let mut cmd = Command::new(&clang_bin);
        // Host-side optimization flags / LTO
        let lto_mode = std::env::var("OATS_LTO").unwrap_or_else(|_| {
            if build_profile == "release" {
                "auto".to_string()
            } else {
                "none".to_string()
            }
        });
        // opt-level env may also instruct host flags
        if let Ok(opt_str) = std::env::var("OATS_OPT_LEVEL") {
            if opt_str == "3" || opt_str == "aggressive" {
                cmd.arg("-O3");
            }
        } else if build_profile == "release" {
            cmd.arg("-O3");
        }
        // LTO handling
        match lto_mode.as_str() {
            "none" => {}
            "thin" => {
                cmd.arg("-flto=thin");
            }
            "fat" | "full" => {
                cmd.arg("-flto");
            }
            "auto" => {
                if build_profile == "release" {
                    cmd.arg("-flto");
                }
            }
            _ => {}
        }
        if let Some(ref lld) = lld_candidate {
            // use clang driver with lld
            cmd.arg(format!("-fuse-ld={}", lld));
        }
        if !rt_main_obj.is_empty() {
            cmd.arg(&rt_main_obj);
        }
        cmd.arg(&out_obj).arg(&rust_lib).arg("-o").arg(&out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{} failed to link final binary", clang_bin);
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else if let Some(lld_bin) = lld_candidate.clone() {
        // Last resort: call lld directly (may not support LTO flags)
        let mut cmd = Command::new(&lld_bin);
        if !rt_main_obj.is_empty() {
            cmd.arg(&rt_main_obj);
        }
        cmd.arg(&out_obj).arg(&rust_lib).arg("-o").arg(&out_exe);
        match cmd.status() {
            Ok(status) => {
                if status.success() {
                    linked_ok = true;
                } else {
                    anyhow::bail!("{} failed to link final binary", lld_bin);
                }
            }
            Err(e) => link_run_err = Some(e),
        }
    } else {
        anyhow::bail!(
            "No suitable linker found: please install clang or lld, or set OATS_LINKER to a linker path"
        );
    }

    if !linked_ok {
        if let Some(e) = link_run_err {
            if e.kind() == std::io::ErrorKind::NotFound {
                anyhow::bail!(
                    "linker not found in PATH; install clang or lld, or set OATS_LINKER to a path"
                );
            } else {
                return Err(e.into());
            }
        } else {
            anyhow::bail!("failed to link final binary");
        }
    }

    println!("{}", out_exe);
    Ok(Some(out_exe))
}
