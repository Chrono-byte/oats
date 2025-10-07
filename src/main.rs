use anyhow::Result;

use oats::parser;
use oats::types::{check_function_strictness, SymbolTable};
use oats::codegen::CodeGen;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

fn main() -> Result<()> {
    // Read source from first CLI arg or OATS_SRC_FILE env var
    let args: Vec<String> = std::env::args().collect();
    let src_path = if args.len() > 1 {
        args[1].clone()
    } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
        p
    } else {
        anyhow::bail!("No source file provided. Pass path as first arg or set OATS_SRC_FILE env var.");
    };

    let source = std::fs::read_to_string(&src_path)?;

    // Parse
    let parsed = parser::parse_oats_module(&source)?;

    // Find first exported function declaration using the ParsedSource API
    let mut func_decl_opt: Option<deno_ast::swc::ast::Function> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl {
                    // f.function is a Box<Function>, so clone and deref
                    func_decl_opt = Some((*f.function).clone());
                    break;
                }
            }
        }
    }

    let func_decl = func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported function found"))?;

    // Type check
    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    // LLVM setup
    let context = Context::create();
    let module = context.create_module("oats");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();
    let codegen = CodeGen { context: &context, module, builder };

    // Generate IR
    codegen.gen_function_ir(
        "add_oats",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
    );

    // Print IR
    println!("{}", codegen.module.print_to_string().to_string());

    Ok(())
}
