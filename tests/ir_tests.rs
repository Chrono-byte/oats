use anyhow::Result;

use oats::parser;
use oats::types::{check_function_strictness, SymbolTable};
use oats::codegen::CodeGen;
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[test]
fn gen_add_function_ir_contains_fadd() -> Result<()> {
    let source = r#"export function main(a: number, b: number): number { return a + b; }"#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    // extract exported function and name
    let mut func_decl_opt: Option<(String, deno_ast::swc::ast::Function)> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl {
                    let name = f.ident.sym.to_string();
                    func_decl_opt = Some((name, (*f.function).clone()));
                    break;
                }
            }
        }
    }

    let (func_name, func_decl) = func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported function found"))?;

    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("oats_test");
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
        &func_name,
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
        None,
    );

    let ir = codegen.module.print_to_string().to_string();

    let expected_sig = format!("define double @{}(double", func_name);
    assert!(ir.contains(&expected_sig), "unexpected function signature: {}", ir);
    assert!(ir.contains("fadd double"), "expected fadd in IR: {}", ir);

    Ok(())
}
