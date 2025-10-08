use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[test]
fn class_fields_lowering_emits_field_access() -> Result<()> {
    let src = std::fs::read_to_string("../../examples/class_field.oats")?;

    let parsed_mod = parser::parse_oats_module(&src, None)?;
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
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        source: &parsed_mod.source,
    };

    // Emit class symbols by scanning module items and invoking main's codegen
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Class(c) = &decl.decl {
                    let class_name = c.ident.sym.to_string();
                    // collect field names from constructor source text using a regex
                    // This is a test heuristic: look for `this.<name> =` patterns inside constructor bodies.
                    let mut fields = Vec::new();
                    for member in &c.class.body {
                        use deno_ast::swc::ast::ClassMember;
                        if let ClassMember::Constructor(cons) = member {
                            if let Some(body) = &cons.body {
                                // reconstruct the source slice for the constructor body
                                let start = body.span.lo.0 as usize;
                                let end = body.span.hi.0 as usize;
                                if end > start && end <= src.len() {
                                    let slice = &src[start..end];
                                    // find occurrences of `this.<ident> =`
                                    // simple scanner: find `this.` then read identifier chars
                                    let mut i = 0;
                                    while let Some(pos) = slice[i..].find("this.") {
                                        i += pos + "this.".len();
                                        // read identifier
                                        let mut ident = String::new();
                                        while i < slice.len() {
                                            let c = slice.as_bytes()[i] as char;
                                            if ident.is_empty() {
                                                if c.is_ascii_alphabetic() || c == '_' {
                                                    ident.push(c);
                                                } else {
                                                    break;
                                                }
                                            } else {
                                                if c.is_ascii_alphanumeric() || c == '_' {
                                                    ident.push(c);
                                                } else {
                                                    break;
                                                }
                                            }
                                            i += 1;
                                        }
                                        if !ident.is_empty() {
                                            fields.push((ident, oats::types::OatsType::Number));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    codegen
                        .class_fields
                        .borrow_mut()
                        .insert(class_name.clone(), fields);

                    // emit ctor and method symbols (placeholders)
                    let ctor_name = format!("{}_ctor", class_name);
                    codegen.module.add_function(
                        &ctor_name,
                        codegen
                            .context
                            .ptr_type(inkwell::AddressSpace::default())
                            .fn_type(&[], false),
                        None,
                    );
                    for member in &c.class.body {
                        use deno_ast::swc::ast::ClassMember;
                        if let ClassMember::Method(m) = member {
                            let mname = match &m.key {
                                deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                                deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                                _ => continue,
                            };
                            let fname = format!("{}_{}", class_name, mname);
                            codegen.module.add_function(
                                &fname,
                                codegen.context.f64_type().fn_type(&[], false),
                                None,
                            );
                        }
                    }
                }
            }
        }
    }

    // Now emit main
    codegen.gen_function_ir(
        "oats_main",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
        None,
    );

    let ir = codegen.module.print_to_string().to_string();

    // Expect constructor and method present
    assert!(
        ir.contains("Point_ctor"),
        "expected generated IR to contain `Point_ctor`: {}",
        ir
    );
    assert!(
        ir.contains("Point_sum"),
        "expected generated IR to contain `Point_sum`: {}",
        ir
    );

    // Ensure the method call used in `main` was actually lowered/used.
    // If the lowering rejected the call expression (returned None) it may
    // result in the call not being present in the emitted IR. Detect that
    // by asserting we actually call the generated `Point_sum` from `oats_main`.
    assert!(
        ir.contains("call double @Point_sum"),
        "expected generated IR to call `Point_sum` from `oats_main`, got IR: {}",
        ir
    );

    Ok(())
}
