use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};
use std::cell::Cell;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

fn gen_ir_for_source(src_path: &str) -> Result<String> {
    let src = std::fs::read_to_string(src_path)?;
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

    let context = Context::create();
    let module = context.create_module("arrays_and_loops_test");
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
        mut_decls: &parsed_mod.mut_decls,
        source: &parsed_mod.preprocessed,
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
fn numeric_array_loop_sum_uses_array_get_f64() -> Result<()> {
    // Example source that reads a numeric array element by index
    let src =
        "export function main(): number { let a = [1,2,3,4]; let x = a[2]; println(x); return 0; }";
    let tmp_path =
        std::env::temp_dir().join(format!("oats_test_numeric_{}.oats", std::process::id()));
    std::fs::write(&tmp_path, src)?;
    let ir = gen_ir_for_source(tmp_path.to_str().unwrap())?;
    let _ = std::fs::remove_file(&tmp_path);
    assert!(
        ir.contains("array_get_f64"),
        "expected array_get_f64 call in IR: {}",
        ir
    );
    Ok(())
}

#[test]
fn pointer_array_index_and_set_uses_ptr_helpers() -> Result<()> {
    // Example source that creates a pointer array (string literals) and reads an element
    let src = "export function main(): number { let a = [\"x\", \"y\"]; let p = a[1]; return 0; }";
    let tmp_path = std::env::temp_dir().join(format!("oats_test_ptr_{}.oats", std::process::id()));
    std::fs::write(&tmp_path, src)?;
    let ir = gen_ir_for_source(tmp_path.to_str().unwrap())?;
    let _ = std::fs::remove_file(&tmp_path);
    // The lowering should at least declare or call array_alloc/array_set_ptr/array_get_ptr
    assert!(
        ir.contains("array_alloc") || ir.contains("array_set_ptr") || ir.contains("array_get_ptr"),
        "expected pointer-array helpers in IR: {}",
        ir
    );
    Ok(())
}
