use std::process::Command;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use anyhow::Result;

use oats::parser;
use oats::types::{check_function_strictness, SymbolTable};
use oats::codegen::CodeGen;

use inkwell::context::Context;
use inkwell::targets::TargetMachine;

fn main() -> Result<()> {
    // Read source from first CLI arg or from OATS_SRC_FILE env var.
    let args: Vec<String> = std::env::args().collect();
    let src_path = if args.len() > 1 {
        args[1].clone()
    } else if let Ok(p) = std::env::var("OATS_SRC_FILE") {
        p
    } else {
        anyhow::bail!("No source file provided. Pass path as first arg or set OATS_SRC_FILE env var.");
    };

    let source = std::fs::read_to_string(&src_path)?;

    let parsed = parser::parse_oats_module(&source)?;

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
    // Set the module target triple to the host default so clang doesn't warn
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
        .arg("runtime/Cargo.toml")
        .arg("--release")
        .status()?;
    if !status.success() {
        anyhow::bail!("building rust runtime failed");
    }

    // locate the produced staticlib
    let rust_lib = "runtime/target/release/libruntime.a";

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

    // Locate or produce rt_main object. Prefer an existing top-level `rt_main.o` so
    // the repo can ship a prebuilt small host object. Otherwise try to compile
    // `runtime/rt_main/src/main.rs` if it exists.
    let rt_main_obj = if Path::new("rt_main.o").exists() {
        // Use the repo-provided object file
        String::from("rt_main.o")
    } else if Path::new("runtime/rt_main/src/main.rs").exists() {
        let rt_main_obj = format!("{}/rt_main.o", out_dir);
        let status = Command::new("rustc")
            .arg("--crate-type")
            .arg("bin")
            .arg("--emit=obj")
            .arg("runtime/rt_main/src/main.rs")
            .arg("-O")
            .arg("-o")
            .arg(&rt_main_obj)
            .status()?;
        if !status.success() {
            anyhow::bail!("rustc failed to compile rt_main to object");
        }
        rt_main_obj
    } else {
        // As a fallback, generate a tiny C main that calls the exported function
        // (assumes the function has signature double add_oats(double,double)).
        let rt_main_c = format!("{}/rt_main.c", out_dir);
        let rt_main_obj = format!("{}/rt_main.o", out_dir);
        let mut cf = File::create(&rt_main_c)?;
        cf.write_all(b"#include <stdio.h>\n\nextern double add_oats(double,double);\n\nint main(){ double r = add_oats(1.5, 2.25); printf(\"result: %f\\n\", r); return 0; }\n")?;
        cf.sync_all()?;
        let status = Command::new("clang")
            .arg("-O2")
            .arg("-c")
            .arg(&rt_main_c)
            .arg("-o")
            .arg(&rt_main_obj)
            .status()?;
        if !status.success() {
            anyhow::bail!("clang failed to compile generated rt_main.c");
        }
        rt_main_obj
    };

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
