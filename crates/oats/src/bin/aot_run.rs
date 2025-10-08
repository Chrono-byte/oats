use anyhow::Result;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use oats::codegen::CodeGen;
use oats::diagnostics;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use inkwell::types::BasicType;

fn map_ts_type_to_oats(ty: &deno_ast::swc::ast::TsType) -> Option<oats::types::OatsType> {
    use deno_ast::swc::ast;
    match ty {
        ast::TsType::TsKeywordType(keyword) => match keyword.kind {
            ast::TsKeywordTypeKind::TsNumberKeyword => Some(oats::types::OatsType::Number),
            ast::TsKeywordTypeKind::TsVoidKeyword => Some(oats::types::OatsType::Void),
            ast::TsKeywordTypeKind::TsBooleanKeyword => Some(oats::types::OatsType::Boolean),
            ast::TsKeywordTypeKind::TsStringKeyword => Some(oats::types::OatsType::String),
            _ => None,
        },
        ast::TsType::TsTypeRef(type_ref) => type_ref
            .type_name
            .as_ident()
            .map(|type_name| oats::types::OatsType::NominalStruct(type_name.sym.to_string())),
        ast::TsType::TsArrayType(arr) => map_ts_type_to_oats(&arr.elem_type)
            .map(|elem| oats::types::OatsType::Array(Box::new(elem))),
        _ => None,
    }
}

fn oats_type_to_basic_type<'ctx>(
    codegen: &CodeGen<'ctx>,
    ty: &oats::types::OatsType,
) -> inkwell::types::BasicTypeEnum<'ctx> {
    match ty {
        oats::types::OatsType::Number => codegen.f64_t.as_basic_type_enum(),
        oats::types::OatsType::Boolean => codegen.bool_t.as_basic_type_enum(),
        oats::types::OatsType::String
        | oats::types::OatsType::NominalStruct(_)
        | oats::types::OatsType::Array(_) => codegen.i8ptr_t.as_basic_type_enum(),
        oats::types::OatsType::Void => codegen
            .context
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
    }
}

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
    fn infer_from_expr(e: &deno_ast::swc::ast::Expr) -> Option<oats::types::OatsType> {
        use deno_ast::swc::ast;
        use deno_ast::swc::ast::Expr;
        match e {
            Expr::Lit(lit) => match lit {
                ast::Lit::Num(_) => Some(oats::types::OatsType::Number),
                ast::Lit::Str(_) => Some(oats::types::OatsType::String),
                ast::Lit::Bool(_) => Some(oats::types::OatsType::Boolean),
                _ => None,
            },
            Expr::Array(arr) => {
                if let Some(Some(first)) = arr.elems.first() {
                    infer_from_expr(&first.expr)
                        .map(|et| oats::types::OatsType::Array(Box::new(et)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

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
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        source: &parsed_mod.source,
    };

    // Populate class_fields for exported classes by examining ClassProp
    // declarations and constructor assignment ASTs (this.x = ...).
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
        {
            let class_name = c.ident.sym.to_string();
            let mut fields: Vec<(String, oats::types::OatsType)> = Vec::new();
            use deno_ast::swc::ast::{ClassMember, Expr, MemberProp, Stmt};
            // Collect explicit property declarations
            for member in &c.class.body {
                if let ClassMember::ClassProp(prop) = member
                    && let deno_ast::swc::ast::PropName::Ident(id) = &prop.key
                {
                    let fname = id.sym.to_string();
                    if !fields.iter().any(|(n, _)| n == &fname) {
                        fields.push((fname, oats::types::OatsType::Number));
                    }
                }
            }
            // Record constructor parameter properties (e.g., `public x: number`)
            for member in &c.class.body {
                if let ClassMember::Constructor(cons) = member {
                    for param in &cons.params {
                        use deno_ast::swc::ast::{ParamOrTsParamProp, TsParamPropParam};
                        if let ParamOrTsParamProp::TsParamProp(ts_param) = param {
                            if let TsParamPropParam::Ident(binding_ident) = &ts_param.param {
                                let fname = binding_ident.id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ty = binding_ident
                                        .type_ann
                                        .as_ref()
                                        .and_then(|ann| map_ts_type_to_oats(&ann.type_ann))
                                        .unwrap_or(oats::types::OatsType::Number);
                                    fields.push((fname, ty));
                                }
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
                                let inferred = infer_from_expr(&assign.right)
                                    .unwrap_or(oats::types::OatsType::Number);
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

    // Generate IR for class methods/constructors
    for item_ref in parsed.program_ref().body() {
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
                            codegen.gen_function_ir(
                                &fname,
                                &m.function,
                                &params,
                                &ret,
                                Some("this"),
                            );
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
                                codegen.gen_function_ir(
                                    &fname,
                                    &m.function,
                                    &params,
                                    &oats::types::OatsType::Void,
                                    Some("this"),
                                );
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Generate constructor function that allocates object and executes body
                        let fname = format!("{}_ctor", class_name);

                        use deno_ast::swc::ast::{ParamOrTsParamProp, Pat, TsParamPropParam};
                        let mut param_infos: Vec<(String, oats::types::OatsType, bool)> =
                            Vec::new();
                        for param in &ctor.params {
                            match param {
                                ParamOrTsParamProp::Param(p) => {
                                    if let Pat::Ident(ident) = &p.pat {
                                        let ty = ident
                                            .type_ann
                                            .as_ref()
                                            .and_then(|ann| map_ts_type_to_oats(&ann.type_ann))
                                            .unwrap_or(oats::types::OatsType::Number);
                                        param_infos.push((ident.id.sym.to_string(), ty, false));
                                    }
                                }
                                ParamOrTsParamProp::TsParamProp(ts_param) => {
                                    if let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                                    {
                                        let ty = binding_ident
                                            .type_ann
                                            .as_ref()
                                            .and_then(|ann| map_ts_type_to_oats(&ann.type_ann))
                                            .unwrap_or(oats::types::OatsType::Number);
                                        param_infos.push((
                                            binding_ident.id.sym.to_string(),
                                            ty,
                                            true,
                                        ));
                                    }
                                }
                            }
                        }

                        let param_types: Vec<_> =
                            param_infos.iter().map(|(_, ty, _)| ty.clone()).collect();
                        let param_llvm_types: Vec<_> = param_types
                            .iter()
                            .map(|ty| oats_type_to_basic_type(&codegen, ty))
                            .collect();
                        let llvm_param_metadata: Vec<_> =
                            param_llvm_types.iter().map(|ty| (*ty).into()).collect();

                        let ctor_fn_type = codegen.i8ptr_t.fn_type(&llvm_param_metadata, false);
                        let func = codegen.module.add_function(&fname, ctor_fn_type, None);
                        let entry = codegen.context.append_basic_block(func, "entry");
                        codegen.builder.position_at_end(entry);

                        let mut fields_for_class = {
                            let borrowed = codegen.class_fields.borrow();
                            borrowed.get(&class_name).cloned().unwrap_or_default()
                        };
                        if fields_for_class.is_empty() {
                            fields_for_class = param_infos
                                .iter()
                                .filter(|(_, _, is_prop)| *is_prop)
                                .map(|(name, ty, _)| (name.clone(), ty.clone()))
                                .collect();
                            if !fields_for_class.is_empty() {
                                codegen
                                    .class_fields
                                    .borrow_mut()
                                    .insert(class_name.clone(), fields_for_class.clone());
                            }
                        }
                        let header_bytes = std::mem::size_of::<u64>() as u64;
                        let field_bytes =
                            (fields_for_class.len() as u64) * (std::mem::size_of::<usize>() as u64);
                        let obj_size = header_bytes + field_bytes;
                        let size_const = codegen
                            .i64_t
                            .const_int(obj_size.max(header_bytes) as u64, false);

                        if codegen.module.get_function("malloc").is_none() {
                            let malloc_ty = codegen.i8ptr_t.fn_type(&[codegen.i64_t.into()], false);
                            let _ = codegen.module.add_function("malloc", malloc_ty, None);
                        }
                        let obj_ptr = if let Some(malloc_fn) = codegen.module.get_function("malloc")
                        {
                            let call_malloc = codegen
                                .builder
                                .build_call(malloc_fn, &[size_const.into()], "call_malloc")
                                .expect("build_call failed");
                            if let inkwell::Either::Left(bv) = call_malloc.try_as_basic_value() {
                                bv.into_pointer_value()
                            } else {
                                // fallback to null pointer if malloc didn't produce a value
                                codegen
                                    .context
                                    .ptr_type(inkwell::AddressSpace::default())
                                    .const_null()
                            }
                        } else {
                            // If malloc isn't declared (shouldn't happen), return a null pointer
                            codegen
                                .context
                                .ptr_type(inkwell::AddressSpace::default())
                                .const_null()
                        };

                        let header_val = codegen.i64_t.const_int(1u64, false);
                        let _ = codegen.builder.build_store(obj_ptr, header_val);

                        let mut param_map = std::collections::HashMap::new();
                        for (idx, (name, _, _)) in param_infos.iter().enumerate() {
                            param_map.insert(name.clone(), idx as u32);
                        }

                        let mut locals_stack = vec![std::collections::HashMap::new()];

                        use inkwell::types::BasicType;
                        let this_alloca = codegen
                            .builder
                            .build_alloca(codegen.i8ptr_t, "this")
                            .expect("alloca this");
                        let _ = codegen.builder.build_store(this_alloca, obj_ptr);
                        if let Some(last) = locals_stack.last_mut() {
                            last.insert(
                                "this".to_string(),
                                (
                                    this_alloca,
                                    codegen.i8ptr_t.as_basic_type_enum(),
                                    true,
                                    false,
                                ),
                            );
                        }

                        let mut param_allocas: std::collections::HashMap<
                            String,
                            (inkwell::values::PointerValue, inkwell::types::BasicTypeEnum),
                        > = std::collections::HashMap::new();
                        for (idx, (name, _ty, _is_prop)) in param_infos.iter().enumerate() {
                            let llvm_ty = param_llvm_types[idx];
                            let param_alloca = match codegen.builder.build_alloca(llvm_ty, name) {
                                Ok(a) => a,
                                Err(_) => continue,
                            };
                            let param_val = match func.get_nth_param(idx as u32) {
                                Some(p) => p,
                                None => continue,
                            };
                            let _ = codegen.builder.build_store(param_alloca, param_val);
                            if llvm_ty == codegen.i8ptr_t.as_basic_type_enum() {
                                let rc_inc = codegen.get_rc_inc();
                                let _ = codegen
                                    .builder
                                    .build_call(rc_inc, &[param_val.into()], "rc_inc_param")
                                    .expect("rc_inc param");
                            }
                            if let Some(last) = locals_stack.last_mut() {
                                last.insert(name.clone(), (param_alloca, llvm_ty, true, false));
                            }
                            param_allocas.insert(name.clone(), (param_alloca, llvm_ty));
                        }

                        let opaque_ptr_ty =
                            codegen.context.ptr_type(inkwell::AddressSpace::default());
                        let hdr_size_const = codegen.i64_t.const_int(header_bytes, false);
                        let ptr_size_const = codegen
                            .i64_t
                            .const_int(std::mem::size_of::<usize>() as u64, false);

                        for (name, _ty, is_prop) in &param_infos {
                            if !*is_prop {
                                continue;
                            }
                            let Some((field_idx, (_fname, field_ty))) = fields_for_class
                                .iter()
                                .enumerate()
                                .find(|(_, (n, _))| n == name)
                            else {
                                continue;
                            };
                            let Some((alloca, alloc_ty)) = param_allocas.get(name) else {
                                continue;
                            };
                            let loaded = codegen
                                .builder
                                .build_load(*alloc_ty, *alloca, &format!("{}_init", name))
                                .expect("load param value");
                            let idx_const = codegen.i64_t.const_int(field_idx as u64, false);
                            let mul = codegen
                                .builder
                                .build_int_mul(idx_const, ptr_size_const, "field_off_mul")
                                .expect("field mul");
                            let offset = codegen
                                .builder
                                .build_int_add(hdr_size_const, mul, "field_off")
                                .expect("field add");
                            let offset_i32 = codegen
                                .builder
                                .build_int_cast(offset, codegen.context.i32_type(), "field_off_i32")
                                .expect("field cast");
                            let gep_res = unsafe {
                                codegen.builder.build_gep(
                                    codegen.context.i8_type(),
                                    obj_ptr,
                                    &[offset_i32],
                                    "field_ptr",
                                )
                            };
                            let Ok(field_ptr) = gep_res else { continue };

                            match field_ty {
                                oats::types::OatsType::Number => {
                                    let slot_ptr = codegen
                                        .builder
                                        .build_pointer_cast(
                                            field_ptr,
                                            opaque_ptr_ty,
                                            "field_f64_ptr",
                                        )
                                        .expect("cast field ptr");
                                    let _ = codegen.builder.build_store(slot_ptr, loaded);
                                }
                                oats::types::OatsType::Boolean => {
                                    let slot_ptr = codegen
                                        .builder
                                        .build_pointer_cast(
                                            field_ptr,
                                            opaque_ptr_ty,
                                            "field_bool_ptr",
                                        )
                                        .expect("cast bool ptr");
                                    let _ = codegen.builder.build_store(slot_ptr, loaded);
                                }
                                oats::types::OatsType::String
                                | oats::types::OatsType::NominalStruct(_)
                                | oats::types::OatsType::Array(_) => {
                                    let slot_ptr = codegen
                                        .builder
                                        .build_pointer_cast(
                                            field_ptr,
                                            opaque_ptr_ty,
                                            "field_ptr_slot",
                                        )
                                        .expect("cast ptr slot");
                                    let _ = codegen.builder.build_store(slot_ptr, loaded);
                                    if let inkwell::values::BasicValueEnum::PointerValue(pv) =
                                        loaded
                                    {
                                        let rc_inc = codegen.get_rc_inc();
                                        let _ = codegen
                                            .builder
                                            .build_call(rc_inc, &[pv.into()], "rc_inc_field_init")
                                            .expect("rc_inc field");
                                    }
                                }
                                _ => {
                                    let slot_ptr = codegen
                                        .builder
                                        .build_pointer_cast(
                                            field_ptr,
                                            opaque_ptr_ty,
                                            "field_generic_ptr",
                                        )
                                        .expect("cast generic ptr");
                                    let _ = codegen.builder.build_store(slot_ptr, loaded);
                                }
                            }
                        }

                        if let Some(body) = &ctor.body {
                            for stmt in &body.stmts {
                                if let deno_ast::swc::ast::Stmt::Expr(expr_stmt) = stmt {
                                    let _ = codegen.lower_expr(
                                        &expr_stmt.expr,
                                        func,
                                        &param_map,
                                        &mut locals_stack,
                                    );
                                }
                            }
                        }

                        if let Some(last) = locals_stack.last_mut() {
                            last.remove("this");
                        }
                        codegen.emit_rc_dec_for_locals(&locals_stack);

                        let obj_bv: inkwell::values::BasicValueEnum = obj_ptr.into();
                        let _ = codegen.builder.build_return(Some(&obj_bv));
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
                codegen.gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None);
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
                codegen.gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None);
            }
        }
    }

    // Emit the user's exported `main` under an internal symbol name to avoid
    // conflicting with the C runtime entrypoint. The script must export
    // `main`, but we generate `oats_main` as the emitted symbol the host
    // runtime will call.
    codegen.gen_function_ir(
        "oats_main",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
        None,
    );

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

    // run produced program and forward its exit code. This keeps program
    // output visible while making the runner behave like a thin wrapper.
    let run = Command::new(&out_exe).status()?;
    if let Some(code) = run.code() {
        if code != 0 {
            // Exit with the same code as the produced program so callers
            // can observe the program's result without the runner converting
            // it into an error.
            std::process::exit(code);
        }
    } else {
        // If there is no exit code (terminated by signal), return an error.
        anyhow::bail!("running out failed (terminated by signal)");
    }

    Ok(())
}
