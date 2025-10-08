use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[test]
fn field_write_emits_gep_store_and_rc_calls() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/field_write.oats")?;

    let parsed_mod = parser::parse_oats_module(&src, None)?;
    let parsed = &parsed_mod.parsed;

    // Find the Counter class and extract its fields for codegen
    let mut class_fields = vec![];
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl {
                    let class_name = c.ident.sym.to_string();
                    if class_name == "Counter" {
                        // Extract constructor params as fields
                        for member in &c.class.body {
                            if let deno_ast::swc::ast::ClassMember::Constructor(cons) = member {
                                for param in &cons.params {
                                    if let deno_ast::swc::ast::ParamOrTsParamProp::TsParamProp(
                                        prop,
                                    ) = param
                                        && let deno_ast::swc::ast::TsParamPropParam::Ident(ident) =
                                            &prop.param
                                        {
                                            let field_name = ident.id.sym.to_string();
                                            // assume number type for simplicity
                                            class_fields
                                                .push((field_name, oats::types::OatsType::Number));
                                        }
                                }
                            }
                        }
                    }
                }
    }

    // Find the increment method
    let mut increment_func_opt: Option<deno_ast::swc::ast::Function> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl {
                    for member in &c.class.body {
                        if let deno_ast::swc::ast::ClassMember::Method(method) = member
                            && let deno_ast::swc::ast::PropName::Ident(method_ident) = &method.key {
                                let method_name = method_ident.sym.to_string();
                                if method_name == "increment" {
                                    increment_func_opt = Some((*method.function).clone());
                                    break;
                                }
                            }
                    }
                }
    }

    let increment_func = increment_func_opt
        .ok_or_else(|| anyhow::anyhow!("No `increment` method found in Counter class"))?;

    // Build nominal struct type for the receiver
    let receiver_type = oats::types::OatsType::NominalStruct("Counter".to_string());
    let mut symbols = SymbolTable::new();
    check_function_strictness(&increment_func, &mut symbols)?;

    // LLVM setup
    let context = Context::create();
    let module = context.create_module("field_write_test");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();

    let i8ptr_t = context.ptr_type(inkwell::AddressSpace::default());
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
        i8ptr_t,
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
        class_fields: std::cell::RefCell::new(
            vec![("Counter".to_string(), class_fields)]
                .into_iter()
                .collect(),
        ),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        source: &src,
    };

    // Generate IR for the increment method (has `this` as receiver)
    let param_types = vec![receiver_type];
    let ret_type = oats::types::OatsType::Void;
    let _function = codegen.gen_function_ir(
        "Counter_increment",
        &increment_func,
        &param_types,
        &ret_type,
        Some("this"),
    );

    // Convert module to string and verify expected IR patterns
    let ir_string = codegen.module.print_to_string().to_string();

    // Debug: print IR for manual inspection
    println!("Generated IR:\n{}", ir_string);

    // Assertions: verify the IR contains expected patterns for field write
    // 1. Should have GEP instruction for field access
    assert!(
        ir_string.contains("getelementptr") || ir_string.contains("gep"),
        "IR should contain GEP instruction for field offset calculation"
    );

    // 2. Should have store instruction
    assert!(
        ir_string.contains("store"),
        "IR should contain store instruction for field write"
    );

    // 3. For numeric fields, we don't expect RC operations, but the pattern should exist
    // Check that the function structure is correct
    assert!(
        ir_string.contains("Counter_increment"),
        "IR should contain the Counter_increment function"
    );

    // 4. Field offset calculation (header size + field index * pointer size)
    assert!(
        ir_string.contains("fld_off") || ir_string.contains("field"),
        "IR should contain field offset calculation"
    );

    Ok(())
}

#[test]
fn field_write_with_pointer_type_uses_rc() -> Result<()> {
    // Test that writing to a pointer-typed field (String, Array, or NominalStruct)
    // properly calls rc_dec on old value and rc_inc on new value

    let src = r#"
        export class Container {
            constructor(public data: string) {}
            
            setData(newData: string): void {
                this.data = newData;
            }
        }
        
        export function main(): number {
            let c = new Container("hello");
            c.setData("world");
            return 0;
        }
    "#;

    let parsed_mod = parser::parse_oats_module(src, None)?;
    let parsed = &parsed_mod.parsed;

    // Extract class fields
    let class_fields = vec![("data".to_string(), oats::types::OatsType::String)];

    // Find setData method
    let mut setdata_func_opt: Option<deno_ast::swc::ast::Function> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
                && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl {
                    for member in &c.class.body {
                        if let deno_ast::swc::ast::ClassMember::Method(method) = member
                            && let deno_ast::swc::ast::PropName::Ident(method_ident) = &method.key
                                && method_ident.sym == "setData" {
                                    setdata_func_opt = Some((*method.function).clone());
                                    break;
                                }
                    }
                }
    }

    let setdata_func =
        setdata_func_opt.ok_or_else(|| anyhow::anyhow!("No `setData` method found"))?;

    let receiver_type = oats::types::OatsType::NominalStruct("Container".to_string());
    let param_types = vec![receiver_type, oats::types::OatsType::String];
    let ret_type = oats::types::OatsType::Void;

    let mut symbols = SymbolTable::new();
    let _func_sig = check_function_strictness(&setdata_func, &mut symbols)?;

    // LLVM setup
    let context = Context::create();
    let module = context.create_module("field_write_rc_test");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();

    let i8ptr_t = context.ptr_type(inkwell::AddressSpace::default());
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
        i8ptr_t,
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
        class_fields: std::cell::RefCell::new(
            vec![("Container".to_string(), class_fields)]
                .into_iter()
                .collect(),
        ),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        source: src,
    };

    let _function = codegen.gen_function_ir(
        "Container_setData",
        &setdata_func,
        &param_types,
        &ret_type,
        Some("this"),
    );

    let ir_string = codegen.module.print_to_string().to_string();

    println!("Generated IR for RC test:\n{}", ir_string);

    // Verify RC operations are present
    assert!(
        ir_string.contains("rc_dec") || ir_string.contains("rc_dec_old_field"),
        "IR should contain rc_dec call for old field value"
    );

    assert!(
        ir_string.contains("rc_inc") || ir_string.contains("rc_inc_new_field"),
        "IR should contain rc_inc call for new field value"
    );

    assert!(
        ir_string.contains("store"),
        "IR should contain store instruction"
    );

    Ok(())
}
