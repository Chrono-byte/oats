use anyhow::Result;

use oats::parser;
use oats::types::{check_function_strictness, SymbolTable};
use oats::codegen::CodeGen;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

#[test]
fn gen_add_function_ir_contains_fadd() -> Result<()> {
    let source = r#"export function add_oats(a: number, b: number): number { return a + b; }"#;

    let parsed = parser::parse_oats_module(source)?;

    // extract exported function
    let mut func_decl_opt: Option<deno_ast::swc::ast::Function> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl {
                    func_decl_opt = Some((*f.function).clone());
                    break;
                }
            }
        }
    }

    let func_decl = func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported function found"))?;

    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("oats_test");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();
    let codegen = CodeGen { context: &context, module, builder };

    codegen.gen_function_ir(
        "add_oats",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
    );

    let ir = codegen.module.print_to_string().to_string();

    assert!(ir.contains("define double @add_oats(double"), "unexpected function signature: {}", ir);
    assert!(ir.contains("fadd double"), "expected fadd in IR: {}", ir);

    Ok(())
}
