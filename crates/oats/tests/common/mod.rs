use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[cfg(test)]
#[allow(dead_code)]
pub fn gen_ir_for_source(src: &str) -> Result<String> {
    // Silence diagnostics printed to stderr during test runs to keep test
    // output focused. The suppress guard restores the previous state when
    // dropped at the end of this function.
    let _diag_guard = oats::diagnostics::suppress();

    let parsed_mod = parser::parse_oats_module(src, None)?;
    let parsed = &parsed_mod.parsed;

    // find exported main
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

    let func_decl =
        func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported `main` found in example"))?;

    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("test_module");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();
    let codegen = CodeGen {
        context: &context,
        module,
        builder,
        next_str_id: Cell::new(0),
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
        closure_local_rettype: std::cell::RefCell::new(std::collections::HashMap::new()),
        last_expr_origin_local: std::cell::RefCell::new(None),
        source: &parsed_mod.source,
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
    };

    // gen_function_ir returns Diagnostic on failures; convert to anyhow for `?` compatibility
    // Emit class methods/constructors for exported classes so tests can inspect
    // generated IR (mirror behavior in crates/oats/src/main.rs).
    let parsed_mod_ref = parser::parse_oats_module(src, None)?;
    let parsed_full = &parsed_mod_ref.parsed;

    for item_ref in parsed_full.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
        {
            let class_name = c.ident.sym.to_string();
            // collect fields as in main.rs
            let mut fields: Vec<(String, oats::types::OatsType)> = Vec::new();
            use deno_ast::swc::ast::{ClassMember, Expr, MemberProp, Stmt};
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
                                    .unwrap_or(oats::types::OatsType::Number);
                                fields.push((fname, ty));
                            }
                        }
                    }
                }
            }

            for member in &c.class.body {
                if let ClassMember::Constructor(cons) = member
                    && let Some(body) = &cons.body
                {
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
                                let inferred = oats::types::infer_type(None, Some(&assign.right));
                                if fields.iter().all(|(n, _)| n != &name) {
                                    fields.push((name, inferred));
                                }
                            }
                    }
                }
            }

            if !fields.is_empty() {
                codegen
                    .class_fields
                    .borrow_mut()
                    .insert(class_name.clone(), fields.clone());
            }

            // emit methods & constructors
            for member in &c.class.body {
                use deno_ast::swc::ast::ClassMember;
                match member {
                    ClassMember::Method(m) => {
                        let mname = match &m.key {
                            deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                            deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                            _ => continue,
                        };
                        if let Ok(sig) = oats::types::check_function_strictness(
                            &m.function,
                            &mut oats::types::SymbolTable::new(),
                        ) {
                            let mut params = Vec::new();
                            params.push(oats::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &m.function, &params, &ret, Some("this"))
                                .map_err(|d| anyhow::anyhow!(d.message))?;
                        } else {
                            // fallback: emit with void return
                            if let Ok(sig2) = oats::types::check_function_strictness(
                                &m.function,
                                &mut oats::types::SymbolTable::new(),
                            ) {
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
                                    .map_err(|d| anyhow::anyhow!(d.message))?;
                            }
                        }
                    }
                    ClassMember::Constructor(cons) => {
                        // Use empty fields list if none were recorded to avoid borrow-panics
                        let fields_vec = codegen
                            .class_fields
                            .borrow()
                            .get(&class_name)
                            .cloned()
                            .unwrap_or_else(Vec::new);
                        codegen
                            .gen_constructor_ir(&class_name, cons, &fields_vec)
                            .map_err(|d| anyhow::anyhow!(d.message))?;
                    }
                    _ => {}
                }
            }
        }
    }

    // Now emit oats_main
    codegen
        .gen_function_ir(
            "oats_main",
            &func_decl,
            &func_sig.params,
            &func_sig.ret,
            None,
        )
        .map_err(|d| anyhow::anyhow!(d.message))?;

    Ok(codegen.module.print_to_string().to_string())
}
