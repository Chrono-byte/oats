use anyhow::Result;
use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{OatsType, SymbolTable, check_function_strictness};
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[test]
fn constructor_with_params_allocates_and_initializes() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/class_constructor.oats")?;

    let parsed_mod = parser::parse_oats_module(&src, None)?;
    let parsed = &parsed_mod.parsed;

    // LLVM setup
    let context = Context::create();
    let module = context.create_module("test_constructor");
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

    // Collect class fields
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
        {
            let class_name = c.ident.sym.to_string();
            let mut fields: Vec<(String, OatsType)> = Vec::new();

            // Collect fields from constructor params
            for member in &c.class.body {
                use deno_ast::swc::ast::ClassMember;
                if let ClassMember::Constructor(cons) = member {
                    for param in &cons.params {
                        use deno_ast::swc::ast::{ParamOrTsParamProp, TsParamPropParam};
                        if let ParamOrTsParamProp::TsParamProp(ts_param) = param
                            && let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                        {
                            let fname = binding_ident.id.sym.to_string();
                            let ty = binding_ident
                                .type_ann
                                .as_ref()
                                .and_then(|ann| oats::types::map_ts_type(&ann.type_ann))
                                .unwrap_or(OatsType::Number);
                            fields.push((fname, ty));
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

            // Emit constructor
            for member in &c.class.body {
                use deno_ast::swc::ast::ClassMember;
                if let ClassMember::Constructor(ctor) = member {
                    codegen.gen_constructor_ir(&class_name, ctor, &fields);
                }
            }

            // Emit methods
            for member in &c.class.body {
                use deno_ast::swc::ast::ClassMember;
                if let ClassMember::Method(m) = member {
                    let mname = match &m.key {
                        deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                        deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                        _ => continue,
                    };
                    let mut symbols = SymbolTable::new();
                    if let Ok(sig) = check_function_strictness(&m.function, &mut symbols) {
                        let mut params = Vec::new();
                        params.push(OatsType::NominalStruct(class_name.clone()));
                        params.extend(sig.params.into_iter());
                        let fname = format!("{}_{}", class_name, mname);
                        codegen.gen_function_ir(
                            &fname,
                            &m.function,
                            &params,
                            &sig.ret,
                            Some("this"),
                        ).expect("codegen should succeed");
                    }
                }
            }
        }
    }

    // Emit main function
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
        {
            let name = f.ident.sym.to_string();
            if name == "main" {
                let mut symbols = SymbolTable::new();
                let sig = check_function_strictness(&f.function, &mut symbols)?;
                codegen.gen_function_ir("oats_main", &f.function, &sig.params, &sig.ret, None).expect("codegen should succeed");
            }
        }
    }

    let ir = codegen.module.print_to_string().to_string();

    // Verify constructor was generated with correct signature
    assert!(
        ir.contains("define ptr @Counter_ctor(double"),
        "Constructor should accept number parameter"
    );
    assert!(
        ir.contains("call ptr @malloc"),
        "Constructor should allocate memory"
    );

    // Verify methods were generated
    assert!(
        ir.contains("define double @Counter_increment(ptr"),
        "increment method should be generated"
    );
    assert!(
        ir.contains("define double @Counter_getValue(ptr"),
        "getValue method should be generated"
    );

    // Verify main calls constructor
    assert!(
        ir.contains("call ptr @Counter_ctor(double"),
        "main should call constructor with parameter"
    );

    Ok(())
}
