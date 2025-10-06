use std::process::Command;
use std::fs::File;
use std::io::Write;
use anyhow::Result;

use oats::parser;
use oats::types::{check_function_strictness, SymbolTable};
use oats::codegen::CodeGen;

use inkwell::context::Context;

fn main() -> Result<()> {
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
    let module = context.create_module("oats_aot");
    let builder = context.create_builder();
    let codegen = CodeGen { context: &context, module, builder };

    codegen.gen_function_ir(
        "add_oats",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
    );

    let ir = codegen.module.print_to_string().to_string();

    // determine output directory (optional)
    let out_dir = std::env::var("OATS_OUT_DIR").unwrap_or_else(|_| ".".to_string());
    let out_ll = format!("{}/out.ll", out_dir);
    let mut f = File::create(&out_ll)?;
    f.write_all(ir.as_bytes())?;
    f.sync_all()?;

    // Build Rust runtime staticlib
    let status = Command::new("cargo")
        .arg("build")
        .arg("--manifest-path")
        .arg("runtime/rust_rt/Cargo.toml")
        .arg("--release")
        .status()?;
    if !status.success() {
        anyhow::bail!("building rust runtime failed");
    }

    // locate the produced staticlib
    let rust_lib = "runtime/rust_rt/target/release/librust_rt.a";

    // Compile IR to object file using clang
    let out_obj = format!("{}/out.o", out_dir);
    let status = Command::new("clang")
        .arg("-O2")
        .arg("-c")
        .arg(&out_ll)
        .arg("-o")
        .arg(&out_obj)
        .status()?;
    if !status.success() {
        anyhow::bail!("clang failed to compile IR to object");
    }

    // Compile rt_main to an object file with rustc
    let rt_main_src = "runtime/rt_main/src/main.rs";
    let rt_main_obj = format!("{}/rt_main.o", out_dir);
    let status = Command::new("rustc")
        .arg("--crate-type")
        .arg("bin")
        .arg("--emit=obj")
        .arg(rt_main_src)
        .arg("-O")
        .arg("-o")
        .arg(&rt_main_obj)
        .status()?;
    if !status.success() {
        anyhow::bail!("rustc failed to compile rt_main to object");
    }

    // Link final binary with clang: rt_main.o + out.o + rust runtime staticlib
    let out_bin = format!("{}/out", out_dir);
    let status = Command::new("clang")
        .arg("-O2")
        .arg(&rt_main_obj)
        .arg(&out_obj)
        .arg(rust_lib)
        .arg("-o")
        .arg(&out_bin)
        .status()?;
    if !status.success() {
        anyhow::bail!("clang failed to link final binary");
    }

    // run produced program
    let run = Command::new(out_bin).status()?;
    if !run.success() {
        anyhow::bail!("running out failed");
    }

    Ok(())
}
