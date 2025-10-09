use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

fn gen_ir_for_source(src: &str) -> Result<String> {
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

    let func_decl = func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported `main` found"))?;
    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("loops_and_fields_test");
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
        source: &parsed_mod.source,
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
    };

    // Populate class_fields for exported classes by examining ClassProp
    // declarations and constructor assignment ASTs, mirroring main.rs logic.
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
            // Scan constructor ASTs for `this.<ident> = <expr>` assignments
            if fields.is_empty() {
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
                                let inferred = oats::types::OatsType::Number;
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

    codegen
        .gen_function_ir(
            "oats_main",
            &func_decl,
            &func_sig.params,
            &func_sig.ret,
            None,
        )
        .expect("codegen should succeed");

    Ok(codegen.module.print_to_string().to_string())
}

#[test]
fn labeled_break_continue_lowering_generates_ir() -> Result<()> {
    let src = r#"
export function main(): number {
  let s = 0;
  outer: for (let i = 0; i < 3; i = i + 1) {
    for (let j = 0; j < 3; j = j + 1) {
      if (i == 1) { break outer; }
      if (j == 1) { continue; }
      s = s + 1;
    }
  }
  return s;
}
"#;
    let ir = gen_ir_for_source(src)?;
    // Ensure IR was generated and contains a function definition and a return
    assert!(
        ir.contains("define") && ir.contains("ret"),
        "unexpected IR: {}",
        ir
    );
    Ok(())
}

#[test]
fn dot_member_param_access_generates_field_load() -> Result<()> {
    let src = r#"
export class Foo { x: number }
export function main(p: Foo): number { return p.x; }
"#;
    let ir = gen_ir_for_source(src)?;
    // Field access lowering uses a named load "field_load"
    assert!(
        ir.contains("field_f64_load") || ir.contains("field_load"),
        "expected field load in IR: {}",
        ir
    );
    Ok(())
}
