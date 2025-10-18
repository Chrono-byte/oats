// Test arrow function return type inference
use anyhow::Result;

use oatsc::codegen::CodeGen;
use oatsc::parser;
use oatsc::types::OatsType;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

/// Helper to create a minimal CodeGen instance for testing
fn create_test_codegen<'a>(context: &'a Context, source: &'a str) -> CodeGen<'a> {
    let module = context.create_module("test");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();

    CodeGen {
        context,
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
        fn_union_get_discriminant: std::cell::RefCell::new(None),
        fn_rc_weak_inc: std::cell::RefCell::new(None),
        fn_rc_weak_dec: std::cell::RefCell::new(None),
        fn_rc_weak_upgrade: std::cell::RefCell::new(None),
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
        current_label: std::cell::RefCell::new(None),
        current_class_parent: std::cell::RefCell::new(None),
        closure_local_rettype: std::cell::RefCell::new(std::collections::HashMap::new()),
        last_expr_origin_local: std::cell::RefCell::new(None),
        async_await_live_sets: std::cell::RefCell::new(None),
        async_local_name_to_slot: std::cell::RefCell::new(None),
        async_resume_blocks: std::cell::RefCell::new(None),
        async_cont_blocks: std::cell::RefCell::new(None),
        async_poll_function: std::cell::RefCell::new(None),
        async_await_counter: std::cell::Cell::new(0),
        async_param_count: std::cell::Cell::new(0),
        async_local_slot_count: std::cell::Cell::new(0),
        async_poll_locals: std::cell::RefCell::new(None),
        source,
        mut_var_decls: std::collections::HashSet::new(),
        current_function_return_type: std::cell::RefCell::new(None),
        last_expr_is_boxed_union: std::cell::Cell::new(false),
        global_function_signatures: std::cell::RefCell::new(std::collections::HashMap::new()),
        symbol_table: std::cell::RefCell::new(oatsc::types::SymbolTable::new()),
        const_items: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_globals: std::cell::RefCell::new(std::collections::HashMap::new()),
        const_interns: std::cell::RefCell::new(std::collections::HashMap::new()),
        current_escape_info: std::cell::RefCell::new(None),
        nested_generic_fns: std::cell::RefCell::new(std::collections::HashMap::new()),
        monomorphized_map: std::cell::RefCell::new(std::collections::HashMap::new()),
        rta_results: None,
        uses_async: std::cell::Cell::new(false),
    }
}

#[test]
fn infer_number_return_from_block() -> Result<()> {
    let source = r#"
        let add = (x: number) => {
            return x * 2;
        };
    "#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    // Find the arrow function
    let mut arrow_body = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Var(var_decl) = decl
        {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init
                    && let deno_ast::swc::ast::Expr::Arrow(arrow) = &**init
                {
                    arrow_body = Some(&arrow.body);
                    break;
                }
            }
        }
    }

    let body = arrow_body.ok_or_else(|| anyhow::anyhow!("No arrow function found"))?;

    let context = Context::create();
    let codegen = create_test_codegen(&context, source);

    let inferred_type = codegen
        .infer_return_type_from_arrow_body(body)
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;
    assert_eq!(inferred_type, OatsType::Number);

    Ok(())
}

#[test]
fn infer_string_return_from_block() -> Result<()> {
    let source = r#"
        let getMessage = (x: number) => {
            return "hello";
        };
    "#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    let mut arrow_body = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Var(var_decl) = decl
        {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init
                    && let deno_ast::swc::ast::Expr::Arrow(arrow) = &**init
                {
                    arrow_body = Some(&arrow.body);
                    break;
                }
            }
        }
    }

    let body = arrow_body.ok_or_else(|| anyhow::anyhow!("No arrow function found"))?;

    let context = Context::create();
    let codegen = create_test_codegen(&context, source);

    let inferred_type = codegen
        .infer_return_type_from_arrow_body(body)
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;
    assert_eq!(inferred_type, OatsType::String);

    Ok(())
}

#[test]
fn infer_void_return_from_block_no_return() -> Result<()> {
    let source = r#"
        let doSomething = (x: number) => {
            print_f64(x);
        };
    "#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    let mut arrow_body = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Var(var_decl) = decl
        {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init
                    && let deno_ast::swc::ast::Expr::Arrow(arrow) = &**init
                {
                    arrow_body = Some(&arrow.body);
                    break;
                }
            }
        }
    }

    let body = arrow_body.ok_or_else(|| anyhow::anyhow!("No arrow function found"))?;

    let context = Context::create();
    let codegen = create_test_codegen(&context, source);

    let inferred_type = codegen
        .infer_return_type_from_arrow_body(body)
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;
    assert_eq!(inferred_type, OatsType::Void);

    Ok(())
}

#[test]
fn infer_union_return_from_block_multiple_types() -> Result<()> {
    let source = r#"
        let getMessage = (x: number) => {
            if (x > 0) {
                return "positive";
            }
            return x;
        };
    "#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    let mut arrow_body = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Var(var_decl) = decl
        {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init
                    && let deno_ast::swc::ast::Expr::Arrow(arrow) = &**init
                {
                    arrow_body = Some(&arrow.body);
                    break;
                }
            }
        }
    }

    let body = arrow_body.ok_or_else(|| anyhow::anyhow!("No arrow function found"))?;

    let context = Context::create();
    let codegen = create_test_codegen(&context, source);

    let inferred_type = codegen
        .infer_return_type_from_arrow_body(body)
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;

    // Should infer a union of String and Number
    match inferred_type {
        OatsType::Union(types) => {
            assert_eq!(types.len(), 2);
            assert!(types.contains(&OatsType::String));
            assert!(types.contains(&OatsType::Number));
        }
        _ => panic!("Expected Union type, got {:?}", inferred_type),
    }

    Ok(())
}

#[test]
fn infer_number_return_from_nested_if() -> Result<()> {
    let source = r#"
        let compute = (x: number) => {
            if (x > 0) {
                return x;
            }
            return 0;
        };
    "#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    let mut arrow_body = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Var(var_decl) = decl
        {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init
                    && let deno_ast::swc::ast::Expr::Arrow(arrow) = &**init
                {
                    arrow_body = Some(&arrow.body);
                    break;
                }
            }
        }
    }

    let body = arrow_body.ok_or_else(|| anyhow::anyhow!("No arrow function found"))?;

    let context = Context::create();
    let codegen = create_test_codegen(&context, source);

    let inferred_type = codegen
        .infer_return_type_from_arrow_body(body)
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;
    // Both returns are numbers, so should infer Number
    assert_eq!(inferred_type, OatsType::Number);

    Ok(())
}

#[test]
fn infer_expr_body_return() -> Result<()> {
    let source = r#"
        let double = (x: number) => x * 2;
    "#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    let mut arrow_body = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(decl) = stmt
            && let deno_ast::swc::ast::Decl::Var(var_decl) = decl
        {
            for decl in &var_decl.decls {
                if let Some(init) = &decl.init
                    && let deno_ast::swc::ast::Expr::Arrow(arrow) = &**init
                {
                    arrow_body = Some(&arrow.body);
                    break;
                }
            }
        }
    }

    let body = arrow_body.ok_or_else(|| anyhow::anyhow!("No arrow function found"))?;

    let context = Context::create();
    let codegen = create_test_codegen(&context, source);

    let inferred_type = codegen
        .infer_return_type_from_arrow_body(body)
        .map_err(|e| anyhow::anyhow!("{:?}", e))?;
    // Expression body should infer Number
    assert_eq!(inferred_type, OatsType::Number);

    Ok(())
}
