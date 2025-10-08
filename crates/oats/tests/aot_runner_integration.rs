use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[test]
fn aot_run_generates_oats_main_symbol() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/add.oats")?;

    let parsed_mod = parser::parse_oats_module(&src, None)?;
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

    // LLVM setup (mirror aot_run)
    let context = Context::create();
    let module = context.create_module("aot_integration_test");
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
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        source: &parsed_mod.source,
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
    };

    codegen.gen_function_ir(
        "oats_main",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
        None,
    );

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("oats_main"),
        "expected generated IR to contain `oats_main`: {}",
        ir
    );

    Ok(())
}
