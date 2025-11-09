use anyhow::Result;

use oatsc::codegen::CodeGen;
use oatsc::parser;
use oatsc::types::{SymbolTable, check_function_strictness};

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[test]
fn gen_add_function_ir_contains_fadd() -> Result<()> {
    let source = r#"export function main(a: number, b: number): number { return a + b; }"#;

    let (parsed_mod_opt, _) = parser::parse_oats_module(source, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;

    // extract exported function and name
    use oats_ast::*;
    let mut func_decl_opt: Option<(String, Function)> = None;
    for stmt in &parsed.body {
        if let Stmt::FnDecl(fn_decl) = stmt {
            let name = fn_decl.ident.sym.clone();
            func_decl_opt = Some((
                name.clone(),
                Function {
                    params: fn_decl.params.clone(),
                    body: fn_decl.body.clone(),
                    return_type: fn_decl.return_type.clone(),
                    span: fn_decl.span.clone(),
                    is_async: fn_decl.is_async,
                    is_generator: fn_decl.is_generator,
                },
            ));
            break;
        }
    }

    let (func_name, func_decl) =
        func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported function found"))?;

    let mut symbols = SymbolTable::new();
    let (func_sig_opt, _) = check_function_strictness(&func_decl, &mut symbols)?;
    let func_sig =
        func_sig_opt.ok_or_else(|| anyhow::anyhow!("Failed to check function strictness"))?;

    let context = Context::create();
    let module = context.create_module("oats_test");
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
        const_items: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_globals: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_interns: std::cell::RefCell::new(std::collections::HashMap::new()),
        current_escape_info: std::cell::RefCell::new(None),
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
        current_label: std::cell::RefCell::new(None),
        current_class_parent: std::cell::RefCell::new(None),
        closure_local_rettype: std::cell::RefCell::new(std::collections::HashMap::new()),
        last_expr_origin_local: std::cell::RefCell::new(None),
        async_await_counter: std::cell::Cell::new(0),
        async_await_live_sets: std::cell::RefCell::new(None),
        async_cont_blocks: std::cell::RefCell::new(None),
        async_local_name_to_slot: std::cell::RefCell::new(None),
        async_param_count: std::cell::Cell::new(0),
        async_local_slot_count: std::cell::Cell::new(0),
        async_poll_function: std::cell::RefCell::new(None),
        async_resume_blocks: std::cell::RefCell::new(None),
        async_poll_locals: std::cell::RefCell::new(None),
        source: &parsed_mod.source,
        mut_var_decls: parsed_mod.mut_var_decls.clone(),
        current_function_return_type: std::cell::RefCell::new(None),
        last_expr_is_boxed_union: std::cell::Cell::new(false),
        global_function_signatures: std::cell::RefCell::new(std::collections::HashMap::new()),
        symbol_table: std::cell::RefCell::new(symbols),
        external_std_fns: std::cell::RefCell::new(std::collections::HashMap::new()),
        nested_generic_fns: std::cell::RefCell::new(std::collections::HashMap::new()),
        monomorphized_map: std::cell::RefCell::new(std::collections::HashMap::new()),
        rta_results: None,
        uses_async: std::cell::Cell::new(false),
    };

    codegen
        .gen_function_ir(
            &func_name,
            &func_decl,
            &func_sig.params,
            &func_sig.ret,
            None,
        )
        .map_err(|d| anyhow::anyhow!(d.message))?;

    let ir = codegen.module.print_to_string().to_string();

    let expected_sig = format!("define double @{}(double", func_name);
    assert!(
        ir.contains(&expected_sig),
        "unexpected function signature: {}",
        ir
    );
    assert!(ir.contains("fadd double"), "expected fadd in IR: {}", ir);

    Ok(())
}
