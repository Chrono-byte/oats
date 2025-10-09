use anyhow::Result;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use oats::codegen::CodeGen;
use oats::diagnostics;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

fn main() -> Result<()> {
    // Read source from first CLI arg or from OATS_SRC_FILE env var.
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

    let source = std::fs::read_to_string(&src_path)?;

    let parsed_mod = parser::parse_oats_module(&source, Some(&src_path))?;
    let parsed = parsed_mod.parsed;

    // Scan AST and reject any use of `var` declarations. We purposely do
    // this early so users get a clear error rather than surprising
    // codegen/runtime behavior later.
    fn stmt_contains_var(stmt: &deno_ast::swc::ast::Stmt) -> bool {
        use deno_ast::swc::ast;
        match stmt {
            // Only consider true `var` (function-scoped) declarations as
            // rejected. `let` and `const` are represented by the same
            // `Decl::Var` AST node but have a different `kind`.
            ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                matches!(vdecl.kind, ast::VarDeclKind::Var)
            }
            ast::Stmt::Block(block) => {
                for s in &block.stmts {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
                false
            }
            ast::Stmt::If(ifstmt) => {
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
            ast::Stmt::For(forstmt) => {
                if stmt_contains_var(&forstmt.body) {
                    return true;
                }
                false
            }
            ast::Stmt::While(ws) => stmt_contains_var(&ws.body),
            ast::Stmt::DoWhile(dws) => stmt_contains_var(&dws.body),
            ast::Stmt::Switch(swt) => {
                for case in &swt.cases {
                    for s in &case.cons {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                false
            }
            ast::Stmt::Try(tr) => {
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
            _ => false,
        }
    }

    // Helper: infer a simple OatsType from an expression (literals and simple arrays)
    // NOTE: This function is now consolidated in oats::types::infer_type_from_expr

    // Walk top-level items and examine function bodies / declarations.
    for item in parsed.program_ref().body() {
        use deno_ast::swc::ast;
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
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
            if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt
                && let Some(body) = &fdecl.function.body
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
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
        {
            if let deno_ast::swc::ast::Decl::Var(vdecl) = &decl.decl
                && matches!(vdecl.kind, ast::VarDeclKind::Var)
            {
                return diagnostics::report_error_and_bail(
                    Some(&src_path),
                    Some(&source),
                    "`var` declarations are not supported. Use `let` or `const` instead.",
                    Some(
                        "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const",
                    ),
                );
            }
            if let ast::Decl::Fn(fdecl) = &decl.decl
                && let Some(body) = &fdecl.function.body
            {
                for s in &body.stmts {
                    if stmt_contains_var(s) {
                        return diagnostics::report_error_and_bail(
                            Some(&src_path),
                            Some(&source),
                            "`var` declarations are not supported. Use `let` or `const` instead.",
                            Some(
                                "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const.",
                            ),
                        );
                    }
                }
            }
        }
    }

    // Module-level body is parsed; do not print debug information here.

    // Require the user script to export a `main` function as the program entrypoint
    let mut func_decl_opt: Option<deno_ast::swc::ast::Function> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
        {
            let name = f.ident.sym.to_string();
            if name == "main" {
                func_decl_opt = Some((*f.function).clone());
                break;
            }
        }
    }

    let func_decl = func_decl_opt.ok_or_else(|| {
        anyhow::anyhow!(
            "No exported `main` function found in script. Please export `function main(...)`."
        )
    })?;

    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("oats_aot");
    // Set the module target triple to the host default so clang doesn't warn
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
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
        source: &parsed_mod.source,
    };

    // Note: class field metadata is computed per-class when emitting
    // constructors below. We avoid a global pre-scan of the module so
    // member-field inference is based on declared/annotated types only.

    // Emit IR for class methods/constructors. Emit for both exported and
    // non-exported class declarations so constructors are available for
    // `new` expressions regardless of export status.
    for item_ref in parsed.program_ref().body() {
        // Handle exported class declarations: `export class Foo {}`
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
        {
            let class_name = c.ident.sym.to_string();
            // Emit members for this class
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
                        // Try to type-check the method function
                        let mut method_symbols = SymbolTable::new();
                        if let Ok(sig) = check_function_strictness(&m.function, &mut method_symbols)
                        {
                            // Prepend `this` as the first param (nominal struct pointer)
                            let mut params = Vec::new();
                            params.push(oats::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &m.function, &params, &ret, Some("this"))
                                .map_err(|d| {
                                    oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                                    anyhow::anyhow!(d.message)
                                })?;
                        } else {
                            // If strict check failed (e.g., missing return annotation), try to emit with Void return
                            let mut method_symbols = SymbolTable::new();
                            if let Ok(sig2) =
                                check_function_strictness(&m.function, &mut method_symbols)
                            {
                                let mut params = Vec::new();
                                params
                                    .push(oats::types::OatsType::NominalStruct(class_name.clone()));
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
                        let mut fields: Vec<(String, oats::types::OatsType)> = Vec::new();
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
                                    // If the class property has a TypeScript type annotation, map it
                                    // to an OatsType; otherwise default to Number.
                                    let ftype = if let Some(type_ann) = &prop.type_ann {
                                        if let Some(mt) =
                                            oats::types::map_ts_type(&type_ann.type_ann)
                                        {
                                            mt
                                        } else {
                                            oats::types::OatsType::Number
                                        }
                                    } else {
                                        oats::types::OatsType::Number
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
                                    let ty = oats::types::infer_type(
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
                                        oats::types::infer_type(None, Some(&assign.right));
                                    // If RHS is an identifier, try to look up a matching
                                    // constructor parameter and use its annotation.
                                    if let oats::types::OatsType::Number = inferred
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
                                                        && let Some(mt) = oats::types::map_ts_type(
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
                                                        && let Some(mt) = oats::types::map_ts_type(
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
                        if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields) {
                            diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
                            return Err(anyhow::anyhow!(d.message));
                        }
                    }
                    _ => {}
                }
            }
        }

        // Also handle non-exported top-level class declarations: `class Foo {}`
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(deno_ast::swc::ast::Decl::Class(c)) = stmt
        {
            let class_name = c.ident.sym.to_string();
            for member in &c.class.body {
                use deno_ast::swc::ast::ClassMember;
                match member {
                    ClassMember::Method(m) => {
                        let mname = match &m.key {
                            deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                            deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                            _ => continue,
                        };
                        let mut method_symbols = SymbolTable::new();
                        if let Ok(sig) = check_function_strictness(&m.function, &mut method_symbols)
                        {
                            let mut params = Vec::new();
                            params.push(oats::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &m.function, &params, &ret, Some("this"))
                                .map_err(|d| {
                                    oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                                    anyhow::anyhow!(d.message)
                                })?;
                        } else {
                            let mut method_symbols = SymbolTable::new();
                            if let Ok(sig2) =
                                check_function_strictness(&m.function, &mut method_symbols)
                            {
                                let mut params = Vec::new();
                                params
                                    .push(oats::types::OatsType::NominalStruct(class_name.clone()));
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
                                            Some(source.as_str()),
                                        );
                                        anyhow::anyhow!(d.message)
                                    })?;
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Compute fields for non-exported class similarly to exported case
                        let mut fields: Vec<(String, oats::types::OatsType)> = Vec::new();
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
                                            oats::types::map_ts_type(&type_ann.type_ann)
                                        {
                                            mt
                                        } else {
                                            oats::types::OatsType::Number
                                        }
                                    } else {
                                        oats::types::OatsType::Number
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
                                    let ty = oats::types::infer_type(
                                        binding_ident.type_ann.as_ref().map(|ann| &*ann.type_ann),
                                        None,
                                    );
                                    fields.push((fname, ty));
                                }
                            }
                        }
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
                                        oats::types::infer_type(None, Some(&assign.right));
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
                        if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields) {
                            diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
                            return Err(anyhow::anyhow!(d.message));
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    // Emit top-level helper functions (non-exported) found in the module so
    // calls to them can be lowered. Skip exported `main` which we handle
    // separately.
    for item in parsed.program_ref().body() {
        use deno_ast::swc::ast;
        // non-exported function declarations: `function foo() {}`
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item
            && let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt
        {
            let fname = fdecl.ident.sym.to_string();
            let inner_func = (*fdecl.function).clone();
            let mut inner_symbols = SymbolTable::new();
            let fsig = check_function_strictness(&inner_func, &mut inner_symbols)?;
            // skip exported `main` (we handle exported main separately later)
            if fname != "main" {
                codegen
                    .gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None)
                    .map_err(|d| {
                        oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                        anyhow::anyhow!("{}", d.message)
                    })?;
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
            let fsig = check_function_strictness(&inner_func, &mut inner_symbols)?;
            if fname != "main" {
                codegen
                    .gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None)
                    .map_err(|d| {
                        oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                        anyhow::anyhow!("{}", d.message)
                    })?;
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
            oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
            anyhow::anyhow!("{}", d.message)
        })?;

    // Try to emit a host `main` into the module so no external shim is
    // required. Recompute IR after emission.
    let emitted_host_main = codegen.emit_host_main(&func_sig.params, &func_sig.ret);

    let ir = codegen.module.print_to_string().to_string();

    // determine output directory (optional)
    let out_dir = std::env::var("OATS_OUT_DIR").unwrap_or_else(|_| ".".to_string());

    // Create output filename based on input filename
    let src_filename = std::path::Path::new(&src_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("out");
    let out_ll = format!("{}/{}.ll", out_dir, src_filename);
    let out_exe = format!("{}/{}", out_dir, src_filename);
    let out_obj = format!("{}/{}.o", out_dir, src_filename);
    // Ensure output directory exists so File::create doesn't fail with ENOENT.
    if let Err(e) = std::fs::create_dir_all(&out_dir) {
        anyhow::bail!("failed to create output directory {}: {}", out_dir, e);
    }

    let mut f = File::create(&out_ll)?;
    f.write_all(ir.as_bytes())?;
    f.sync_all()?;

    // Build Rust runtime staticlib
    // Build the runtime crate from the workspace
    let status = Command::new("cargo")
        .arg("build")
        .arg("-p")
        .arg("runtime")
        .arg("--release")
        .status()?;
    if !status.success() {
        anyhow::bail!("building rust runtime failed");
    }

    // locate the produced staticlib
    // Locate the produced staticlib. Cargo may put workspace artifacts under
    // the workspace `target/` directory instead of `crates/runtime/target/`.
    let candidates = [
        "crates/runtime/target/release/libruntime.a",
        "target/release/libruntime.a",
        "crates/runtime/target/debug/libruntime.a",
        "target/debug/libruntime.a",
    ];
    let rust_lib = candidates
        .into_iter()
        .find(|p| Path::new(p).exists())
        .map(|s| s.to_string())
        .ok_or_else(|| {
            anyhow::anyhow!("runtime staticlib not found; please build the runtime crate")
        })?;

    // Compile IR to object file using clang
    let status = Command::new("clang")
        .arg("-O2")
        .arg("-c")
        .arg(&out_ll)
        .arg("-o")
        .arg(&out_obj)
        .status()?;
    if !status.success() {
        anyhow::bail!("clang failed to compile IR to object");
    }

    // Locate or produce rt_main object. Prefer an existing top-level `rt_main.o` so
    // the repo can ship a prebuilt small host object. Otherwise try to compile
    // `runtime/rt_main/src/main.rs` if it exists.
    let rt_main_obj = if emitted_host_main {
        // host main emitted into the module; no external rt_main.o required
        String::new()
    } else if Path::new("rt_main.o").exists() {
        // Use the repo-provided object file
        String::from("rt_main.o")
    } else if Path::new("crates/runtime/rt_main/src/main.rs").exists() {
        let rt_main_obj = format!("{}/rt_main.o", out_dir);
        let status = Command::new("rustc")
            .arg("--crate-type")
            .arg("bin")
            .arg("--emit=obj")
            .arg("crates/runtime/rt_main/src/main.rs")
            .arg("-O")
            .arg("-o")
            .arg(&rt_main_obj)
            .status()?;
        if !status.success() {
            anyhow::bail!("rustc failed to compile rt_main to object");
        }
        rt_main_obj
    } else {
        anyhow::bail!(
            "No rt_main.o found and no runtime/rt_main/src/main.rs available; please provide a runtime main (rt_main.o) or add a runtime/rt_main/src/main.rs"
        );
    };
    // Link final binary with clang. If we emitted the host `main` in the
    // module then `rt_main_obj` will be empty and we skip adding it to the
    // link line.
    let mut link_cmd = Command::new("clang");
    link_cmd.arg("-O2");
    if !rt_main_obj.is_empty() {
        link_cmd.arg(&rt_main_obj);
    }
    link_cmd.arg(&out_obj).arg(rust_lib).arg("-o").arg(&out_exe);
    let status = link_cmd.status()?;
    if !status.success() {
        anyhow::bail!("clang failed to link final binary");
    }

    Ok(())
}
