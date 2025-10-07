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

    // Module-level body is parsed; do not print debug information here.

    // Require the user script to export a `main` function as the program entrypoint
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

    let func_decl = func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported `main` function found in script. Please export `function main(...)`."))?;

    let mut symbols = SymbolTable::new();
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("oats_aot");
    // Set the module target triple to the host default so clang doesn't warn
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let builder = context.create_builder();
    let codegen = CodeGen { context: &context, module, builder };

    // Emit top-level helper functions (non-exported) found in the module so
    // calls to them can be lowered. Skip exported `main` which we handle
    // separately.
    for item in parsed.program_ref().body() {
        use deno_ast::swc::ast;
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
            if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt {
                let fname = fdecl.ident.sym.to_string();
                let inner_func = (*fdecl.function).clone();
                let mut inner_symbols = SymbolTable::new();
                let fsig = check_function_strictness(&inner_func, &mut inner_symbols)?;
                codegen.gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret);
            }
        }
    }

    // Emit the user's exported `main` under an internal symbol name to avoid
    // conflicting with the C runtime entrypoint. The script must export
    // `main`, but we generate `oats_main` as the emitted symbol the host
    // runtime will call.
    codegen.gen_function_ir(
        "oats_main",
        &func_decl,
        &func_sig.params,
        &func_sig.ret,
    );

    // Try to emit a host `main` into the module so no external shim is
    // required. Recompute IR after emission.
    let emitted_host_main = codegen.emit_host_main(&func_sig.params, &func_sig.ret);

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
    let rt_main_obj = if emitted_host_main {
        // host main emitted into the module; no external rt_main.o required
        String::new()
    } else if Path::new("rt_main.o").exists() {
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
        anyhow::bail!("No rt_main.o found and no runtime/rt_main/src/main.rs available; please provide a runtime main (rt_main.o) or add a runtime/rt_main/src/main.rs");
    };
    // Link final binary with clang. If we emitted the host `main` in the
    // module then `rt_main_obj` will be empty and we skip adding it to the
    // link line.
    let out_bin = format!("{}/out", out_dir);
    let mut link_cmd = Command::new("clang");
    link_cmd.arg("-O2");
    if !rt_main_obj.is_empty() {
        link_cmd.arg(&rt_main_obj);
    }
    link_cmd.arg(&out_obj).arg(rust_lib).arg("-o").arg(&out_bin);
    let status = link_cmd.status()?;
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
