use std::process::Command;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use anyhow::Result;

use oats::parser;
use oats::types::{check_function_strictness, SymbolTable};
use oats::codegen::CodeGen;
use oats::diagnostics;

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

    let parsed = parser::parse_oats_module(&source, Some(&src_path))?;

    // Scan AST and reject any use of `var` declarations. We purposely do
    // this early so users get a clear error rather than surprising
    // codegen/runtime behavior later.
    fn stmt_contains_var(stmt: &deno_ast::swc::ast::Stmt) -> bool {
        use deno_ast::swc::ast;
        match stmt {
            // Only consider true `var` (function-scoped) declarations as
            // rejected. `let` and `const` are represented by the same
            // `Decl::Var` AST node but have a different `kind`.
            ast::Stmt::Decl(ast::Decl::Var(vdecl)) => {
                matches!(vdecl.kind, ast::VarDeclKind::Var)
            }
            ast::Stmt::Block(block) => {
                for s in &block.stmts {
                    if stmt_contains_var(s) { return true; }
                }
                false
            }
            ast::Stmt::If(ifstmt) => {
                if stmt_contains_var(&*ifstmt.cons) { return true; }
                if let Some(alt) = &ifstmt.alt { if stmt_contains_var(&*alt) { return true; } }
                false
            }
            ast::Stmt::For(forstmt) => {
                if stmt_contains_var(&*forstmt.body) { return true; }
                false
            }
            ast::Stmt::While(ws) => stmt_contains_var(&*ws.body),
            ast::Stmt::DoWhile(dws) => stmt_contains_var(&*dws.body),
            ast::Stmt::Switch(swt) => {
                for case in &swt.cases {
                    for s in &case.cons { if stmt_contains_var(s) { return true; } }
                }
                false
            }
            ast::Stmt::Try(tr) => {
                // tr.block is a BlockStmt
                for s in &tr.block.stmts { if stmt_contains_var(s) { return true; } }
                if let Some(handler) = &tr.handler {
                    for s in &handler.body.stmts { if stmt_contains_var(s) { return true; } }
                }
                if let Some(finalizer) = &tr.finalizer {
                    for s in &finalizer.stmts { if stmt_contains_var(s) { return true; } }
                }
                false
            }
            _ => false,
        }
    }

    // Walk top-level items and examine function bodies / declarations.
    for item in parsed.program_ref().body() {
        use deno_ast::swc::ast;
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
            if stmt_contains_var(stmt) {
                return diagnostics::report_error_and_bail(
                    Some(&src_path),
                    Some(&source),
                    "`var` declarations are not supported. Use `let` or `const` instead.",
                    Some("`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`."),
                );
            }
            // If it's a function decl, also inspect its body for var
            if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt {
                if let Some(body) = &fdecl.function.body {
                    for s in &body.stmts {
                        if stmt_contains_var(s) {
                            return diagnostics::report_error_and_bail(
                                Some(&src_path),
                                Some(&source),
                                "`var` declarations are not supported. Use `let` or `const` instead.",
                                Some("`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`."),
                            );
                        }
                    }
                }
            }
        }
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let ast::Decl::Var(vdecl) = &decl.decl {
                    if matches!(vdecl.kind, ast::VarDeclKind::Var) {
                        return diagnostics::report_error_and_bail(
                            Some(&src_path),
                            Some(&source),
                            "`var` declarations are not supported. Use `let` or `const` instead.",
                            Some("`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const"),
                        );
                    }
                }
                if let ast::Decl::Fn(fdecl) = &decl.decl {
                    if let Some(body) = &fdecl.function.body {
                        for s in &body.stmts {
                            if stmt_contains_var(s) {
                                return diagnostics::report_error_and_bail(
                                    Some(&src_path),
                                    Some(&source),
                                    "`var` declarations are not supported. Use `let` or `const` instead.",
                                    Some("`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`."),
                                );
                            }
                        }
                    }
                }
            }
        }
    }

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
    let codegen = CodeGen {
        context: &context,
        module,
        builder,
        next_str_id: std::cell::Cell::new(0),
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
    };

    // Emit top-level helper functions (non-exported) found in the module so
    // calls to them can be lowered. Skip exported `main` which we handle
    // separately.
    for item in parsed.program_ref().body() {
        use deno_ast::swc::ast;
        // non-exported function declarations: `function foo() {}`
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
            if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt {
                let fname = fdecl.ident.sym.to_string();
                let inner_func = (*fdecl.function).clone();
                let mut inner_symbols = SymbolTable::new();
                let fsig = check_function_strictness(&inner_func, &mut inner_symbols)?;
                // skip exported `main` (we handle exported main separately later)
                if fname != "main" {
                    codegen.gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret);
                }
            }
        }

        // exported declarations: `export function foo() {}` — emit these too
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let ast::Decl::Fn(fdecl) = &decl.decl {
                    let fname = fdecl.ident.sym.to_string();
                    let inner_func = (*fdecl.function).clone();
                    let mut inner_symbols = SymbolTable::new();
                    let fsig = check_function_strictness(&inner_func, &mut inner_symbols)?;
                    if fname != "main" {
                        codegen.gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret);
                    }
                }
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
    // Build the runtime crate from the workspace
    let status = Command::new("cargo")
        .arg("build")
        .arg("-p")
        .arg("runtime")
        .arg("--release")
        .status()?;
    if !status.success() {
        anyhow::bail!("building rust runtime failed");
    }

    // locate the produced staticlib
    // Locate the produced staticlib. Cargo may put workspace artifacts under
    // the workspace `target/` directory instead of `crates/runtime/target/`.
    let candidates = [
        "crates/runtime/target/release/libruntime.a",
        "target/release/libruntime.a",
        "crates/runtime/target/debug/libruntime.a",
        "target/debug/libruntime.a",
    ];
    let rust_lib = candidates
        .into_iter()
        .find(|p| Path::new(p).exists())
        .map(|s| s.to_string())
        .ok_or_else(|| anyhow::anyhow!("runtime staticlib not found; please build the runtime crate"))?;

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
    } else if Path::new("crates/runtime/rt_main/src/main.rs").exists() {
        let rt_main_obj = format!("{}/rt_main.o", out_dir);
        let status = Command::new("rustc")
            .arg("--crate-type")
            .arg("bin")
            .arg("--emit=obj")
            .arg("crates/runtime/rt_main/src/main.rs")
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

    // run produced program and forward its exit code. This keeps program
    // output visible while making the runner behave like a thin wrapper.
    let run = Command::new(out_bin).status()?;
    if let Some(code) = run.code() {
        if code != 0 {
            // Exit with the same code as the produced program so callers
            // can observe the program's result without the runner converting
            // it into an error.
            std::process::exit(code);
        }
    } else if !run.success() {
        // If there is no exit code (terminated by signal), return an error.
        anyhow::bail!("running out failed (terminated by signal)");
    }

    Ok(())
}
