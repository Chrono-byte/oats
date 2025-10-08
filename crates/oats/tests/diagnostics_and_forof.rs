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
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl {
                    let name = f.ident.sym.to_string();
                    if name == "main" {
                        func_decl_opt = Some((*f.function).clone());
                        break;
                    }
                }
            }
        }
    }

    let func_decl =
        func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported `main` found in example"))?;
    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("forof_test");
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
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        source: &parsed_mod.source,
    };

    codegen.gen_function_ir(
        "oats_main",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
        None,
    );

    Ok(codegen.module.print_to_string().to_string())
}

#[test]
fn parser_reports_missing_semicolon_with_hint() -> Result<()> {
    let src = "export function main(): number { let x = 1\n return x; }"; // missing semicolon after let
    let res = parser::parse_oats_module(src, None);
    assert!(
        res.is_err(),
        "expected parse to fail due to missing semicolon"
    );
    let err = match res {
        Err(e) => format!("{}", e),
        Ok(_) => panic!("expected error"),
    };
    assert!(err.contains("missing semicolon"));
    assert!(
        err.contains("add a trailing") || err.contains("hint:"),
        "err was: {}",
        err
    );
    Ok(())
}

#[test]
fn for_of_lowering_uses_array_helpers() -> Result<()> {
    let src = r#"
export function main(): number {
  let s = 0;
  for (let v of [1,2,3]) {
    s = s + v;
  }
  return s;
}
"#;
    let ir = gen_ir_for_source(src)?;
    assert!(
        ir.contains("array_get_f64") || ir.contains("array_get_ptr") || ir.contains("array_alloc"),
        "expected array helpers in IR: {}",
        ir
    );
    Ok(())
}

#[test]
fn for_of_pointer_array_uses_ptr_helpers() -> Result<()> {
    let src = r#"
export function main(): number {
  for (let s of ["a","b"]) {
    println(s);
  }
  return 0;
}
"#;
    let ir = gen_ir_for_source(src)?;
    // pointer-based for-of should rely on array_get_ptr/array_alloc/array_set_ptr
    assert!(
        ir.contains("array_get_ptr") || ir.contains("array_alloc") || ir.contains("array_set_ptr"),
        "expected pointer-array helpers in IR: {}",
        ir
    );
    Ok(())
}
