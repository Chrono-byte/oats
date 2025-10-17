use anyhow::Result;
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use oatsc::codegen::CodeGen;
use oatsc::parser;
use oatsc::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;

#[test]
fn test_typeof_uses_discriminant_for_unions() -> Result<()> {
    let source = r#"
export function main(x: number | string): string {
    // unary typeof
    let a = typeof x;
    // binary typeof comparison
    if (typeof x === "number") {
        return "num";
    }
    return "other";
}
"#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    // Extract exported function
    let mut func_decl_opt: Option<(String, deno_ast::swc::ast::Function)> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
        {
            let name = f.ident.sym.to_string();
            func_decl_opt = Some((name, (*f.function).clone()));
            break;
        }
    }

    let (func_name, func_decl) =
        func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported function found"))?;

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
        closure_local_rettype: std::cell::RefCell::new(std::collections::HashMap::new()),
        last_expr_origin_local: std::cell::RefCell::new(None),
    current_class_parent: std::cell::RefCell::new(None),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
    source: &parsed_mod.source,
    mut_var_decls: parsed_mod.mut_var_decls.clone(),
        current_function_return_type: std::cell::RefCell::new(None),
        last_expr_is_boxed_union: std::cell::Cell::new(false),
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
        nested_generic_fns: std::cell::RefCell::new(std::collections::HashMap::new()),
        monomorphized_map: std::cell::RefCell::new(std::collections::HashMap::new()),
        rta_results: None,
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

    // Expect the discriminant helper to be referenced in the IR
    assert!(
        ir.contains("union_get_discriminant") || ir.contains("union_get_disc"),
        "IR should call the union discriminant helper"
    );

    // Expect interned string literal globals for "number" and "string"
    assert!(
        ir.contains("strlit"),
        "IR should contain string literal globals (interned strings)"
    );

    Ok(())
}
