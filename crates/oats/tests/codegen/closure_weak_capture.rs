use anyhow::Result;
use inkwell::context::Context;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};
use super::common::create_codegen;

#[test]
fn test_closure_weak_capture_codegen() -> Result<()> {
    let source = r#"
export function main(): number {
    let obj: any = {} as any;
    let w: Weak<any> = obj.downgrade();
    let f = () => w;
    return 0;
}
"#;

    let parsed_mod = parser::parse_oats_module(source, None)?;
    let parsed = &parsed_mod.parsed;

    let mut func_decl_opt: Option<(String, deno_ast::swc::ast::Function)> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
        {
            func_decl_opt = Some((f.ident.sym.to_string(), (*f.function).clone()));
            break;
        }
    }

    let (_, func_decl) =
        func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported function found"))?;
    let mut symbols = SymbolTable::new();
    check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let codegen = create_codegen(&context, "closure_weak_capture_test", symbols, source);

    let ir = codegen.module.print_to_string().to_string();

    assert!(
        ir.contains("rc_weak_inc") || ir.contains("rc_weak_upgrade") || ir.contains("rc_weak_dec"),
        "IR should reference rc_weak helper functions"
    );

    Ok(())
}
