use anyhow::Result;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use oats::codegen::CodeGen;
use oats::diagnostics;
use oats::parser;
use oats::types::{SymbolTable, check_function_strictness};

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
        anyhow::bail!(
            "No source file provided. Pass path as first arg or set OATS_SRC_FILE env var."
        );
    };

    let source = std::fs::read_to_string(&src_path)?;

    let parsed_mod = parser::parse_oats_module(&source, Some(&src_path))?;
    let parsed = parsed_mod.parsed;

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
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
                false
            }
            ast::Stmt::If(ifstmt) => {
                if stmt_contains_var(&ifstmt.cons) {
                    return true;
                }
                if let Some(alt) = &ifstmt.alt
                    && stmt_contains_var(alt)
                {
                    return true;
                }
                false
            }
            ast::Stmt::For(forstmt) => {
                if stmt_contains_var(&forstmt.body) {
                    return true;
                }
                false
            }
            ast::Stmt::While(ws) => stmt_contains_var(&ws.body),
            ast::Stmt::DoWhile(dws) => stmt_contains_var(&dws.body),
            ast::Stmt::Switch(swt) => {
                for case in &swt.cases {
                    for s in &case.cons {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                false
            }
            ast::Stmt::Try(tr) => {
                // tr.block is a BlockStmt
                for s in &tr.block.stmts {
                    if stmt_contains_var(s) {
                        return true;
                    }
                }
                if let Some(handler) = &tr.handler {
                    for s in &handler.body.stmts {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                if let Some(finalizer) = &tr.finalizer {
                    for s in &finalizer.stmts {
                        if stmt_contains_var(s) {
                            return true;
                        }
                    }
                }
                false
            }
            _ => false,
        }
    }

    // Helper: infer a simple OatsType from an expression (literals and simple arrays)
    fn infer_from_expr(e: &deno_ast::swc::ast::Expr) -> Option<oats::types::OatsType> {
        use deno_ast::swc::ast;
        use deno_ast::swc::ast::Expr;
        match e {
            Expr::Lit(lit) => match lit {
                ast::Lit::Num(_) => Some(oats::types::OatsType::Number),
                ast::Lit::Str(_) => Some(oats::types::OatsType::String),
                ast::Lit::Bool(_) => Some(oats::types::OatsType::Boolean),
                _ => None,
            },
            Expr::Array(arr) => {
                if let Some(Some(first)) = arr.elems.first() {
                    infer_from_expr(&first.expr).map(|et| oats::types::OatsType::Array(Box::new(et)))
                } else {
                    None
                }
            }
            _ => None,
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
                    Some(
                        "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`.",
                    ),
                );
            }
            // If it's a function decl, also inspect its body for var
            if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt
                && let Some(body) = &fdecl.function.body
            {
                for s in &body.stmts {
                    if stmt_contains_var(s) {
                        return diagnostics::report_error_and_bail(
                            Some(&src_path),
                            Some(&source),
                            "`var` declarations are not supported. Use `let` or `const` instead.",
                            Some(
                                "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const`.",
                            ),
                        );
                    }
                }
            }
        }
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
        {
            if let deno_ast::swc::ast::Decl::Var(vdecl) = &decl.decl
                && matches!(vdecl.kind, ast::VarDeclKind::Var)
            {
                return diagnostics::report_error_and_bail(
                    Some(&src_path),
                    Some(&source),
                    "`var` declarations are not supported. Use `let` or `const` instead.",
                    Some(
                        "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const",
                    ),
                );
            }
            if let ast::Decl::Fn(fdecl) = &decl.decl
                && let Some(body) = &fdecl.function.body
            {
                for s in &body.stmts {
                    if stmt_contains_var(s) {
                        return diagnostics::report_error_and_bail(
                            Some(&src_path),
                            Some(&source),
                            "`var` declarations are not supported. Use `let` or `const` instead.",
                            Some(
                                "`var` has function-scoped semantics which we intentionally disallow; prefer `let` or `const.",
                            ),
                        );
                    }
                }
            }
        }
    }

    // Module-level body is parsed; do not print debug information here.

    // Require the user script to export a `main` function as the program entrypoint
    let mut func_decl_opt: Option<deno_ast::swc::ast::Function> = None;
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl
        {
            let name = f.ident.sym.to_string();
            if name == "main" {
                func_decl_opt = Some((*f.function).clone());
                break;
            }
        }
    }

    let func_decl = func_decl_opt.ok_or_else(|| {
        anyhow::anyhow!(
            "No exported `main` function found in script. Please export `function main(...)`."
        )
    })?;

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
        fn_array_alloc: std::cell::RefCell::new(None),
        fn_rc_inc: std::cell::RefCell::new(None),
        fn_rc_dec: std::cell::RefCell::new(None),
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        mut_decls: &parsed_mod.mut_decls,
        source: &parsed_mod.preprocessed,
    };

    // Populate class_fields for exported classes by examining ClassProp
    // declarations and constructor assignment ASTs (this.x = ...).
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
        {
            let class_name = c.ident.sym.to_string();
            let mut fields: Vec<(String, oats::types::OatsType)> = Vec::new();
            use deno_ast::swc::ast::{ClassMember, Expr, MemberProp, Stmt};
            // Collect explicit property declarations
            for member in &c.class.body {
                if let ClassMember::ClassProp(prop) = member
                    && let deno_ast::swc::ast::PropName::Ident(id) = &prop.key
                {
                    let fname = id.sym.to_string();
                    if !fields.iter().any(|(n, _)| n == &fname) {
                        fields.push((fname, oats::types::OatsType::Number));
                    }
                }
            }
            // Scan constructor ASTs for `this.<ident> = <expr>` assignments
            if fields.is_empty() {
                for member in &c.class.body {
                    if let ClassMember::Constructor(cons) = member
                        && let Some(body) = &cons.body
                    {
                        for stmt in &body.stmts {
                            if let Stmt::Expr(expr_stmt) = stmt
                                && let Expr::Assign(assign) = &*expr_stmt.expr
                                && let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                    &assign.left
                            {
                                // Match a simple member assignment like `this.x = ...`
                                if let deno_ast::swc::ast::SimpleAssignTarget::Member(mem) =
                                    simple_target
                                    && matches!(&*mem.obj, Expr::This(_))
                                    && let MemberProp::Ident(ident) = &mem.prop
                                {
                                    let name = ident.sym.to_string();
                                    let inferred =
                                        infer_from_expr(&assign.right).unwrap_or(oats::types::OatsType::Number);
                                    if fields.iter().all(|(n, _)| n != &name) {
                                        fields.push((name, inferred));
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if !fields.is_empty() {
                codegen.class_fields.borrow_mut().insert(class_name, fields);
            }
        }
    }

    // Generate IR for class methods/constructors
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
        {
            let class_name = c.ident.sym.to_string();
            // `ClassDecl` contains an inner `class: Class` field; iterate
            // over `c.class.body` to access members.
            for member in &c.class.body {
                use deno_ast::swc::ast::ClassMember;
                match member {
                    ClassMember::Method(m) => {
                        // method name
                        let mname = match &m.key {
                            deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                            deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                            _ => continue,
                        };
                        // Try to type-check the method function
                        let mut method_symbols = SymbolTable::new();
                        if let Ok(sig) = check_function_strictness(&m.function, &mut method_symbols) {
                            // Prepend `this` as the first param (nominal struct pointer)
                            let mut params = Vec::new();
                            params.push(oats::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen.gen_function_ir(
                                &fname,
                                &m.function,
                                &params,
                                &ret,
                                Some("this"),
                            );
                        } else {
                            // If strict check failed (e.g., missing return annotation), try to emit with Void return
                            let mut method_symbols = SymbolTable::new();
                            if let Ok(sig2) = check_function_strictness(&m.function, &mut method_symbols) {
                                let mut params = Vec::new();
                                params
                                    .push(oats::types::OatsType::NominalStruct(class_name.clone()));
                                params.extend(sig2.params.into_iter());
                                let fname = format!("{}_{}", class_name, mname);
                                codegen.gen_function_ir(
                                    &fname,
                                    &m.function,
                                    &params,
                                    &oats::types::OatsType::Void,
                                    Some("this"),
                                );
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Generate constructor function that allocates object and executes body
                        let fname = format!("{}_ctor", class_name);

                        // For now, hardcode parameter types for Point constructor
                        // TODO: Extract parameter types from ctor AST
                        let param_types = vec![oats::types::OatsType::Number, oats::types::OatsType::Number];

                        // Create function type: (param_types...) -> i8*
                        let param_llvm_types: Vec<inkwell::types::BasicTypeEnum> = param_types
                            .iter()
                            .map(|t| match t {
                                oats::types::OatsType::Number => codegen.f64_t.into(),
                                _ => codegen.f64_t.into(), // default to f64
                            })
                            .collect();
                        let i8ptr = codegen.context.ptr_type(inkwell::AddressSpace::default());
                        let ctor_fn_type = i8ptr.fn_type(
                            &param_llvm_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                            false,
                        );
                        let func = codegen.module.add_function(&fname, ctor_fn_type, None);
                        let entry = codegen.context.append_basic_block(func, "entry");
                        codegen.builder.position_at_end(entry);

                        // Allocate object memory: header + fields
                        let num_fields = 2; // TODO: get from class_fields
                        let obj_size = 8 + (num_fields * 8); // header + fields * sizeof(void*)
                        let size_const = codegen.i64_t.const_int(obj_size as u64, false);

                        // Get malloc function
                        if codegen.module.get_function("malloc").is_none() {
                            let malloc_ty = i8ptr.fn_type(&[codegen.i64_t.into()], false);
                            let _ = codegen.module.add_function("malloc", malloc_ty, None);
                        }
                        let malloc_fn = codegen.module.get_function("malloc").unwrap();

                        let call_malloc = codegen.builder.build_call(
                            malloc_fn,
                            &[size_const.into()],
                            "call_malloc",
                        ).expect("build_call failed");
                        let obj_ptr = call_malloc.try_as_basic_value().left().unwrap().into_pointer_value();

                        // Store header (refcount = 1)
                        let header_val = codegen.i64_t.const_int(1u64, false);
                        let _ = codegen.builder.build_store(obj_ptr, header_val);

                        // Set up constructor execution environment
                        let mut param_map = std::collections::HashMap::new();
                        for (i, _) in param_types.iter().enumerate() {
                            param_map.insert(format!("param_{}", i), i as u32);
                        }

                        // Create locals stack for constructor body execution
                        let mut locals_stack = vec![std::collections::HashMap::new()];

                        // Add 'this' as a local pointing to the allocated object
                        let this_alloca = codegen.builder.build_alloca(i8ptr, "this").unwrap();
                        let _ = codegen.builder.build_store(this_alloca, obj_ptr);
                        use inkwell::types::BasicType;
                        locals_stack.last_mut().unwrap().insert(
                            "this".to_string(),
                            (this_alloca, i8ptr.as_basic_type_enum(), true, false),
                        );

                        // Add constructor parameters as locals
                        for (i, param_type) in param_types.iter().enumerate() {
                            let param_val = func.get_nth_param(i as u32).unwrap();
                            let llvm_type = match param_type {
                                oats::types::OatsType::Number => codegen.f64_t,
                                _ => codegen.f64_t,
                            };
                            let param_alloca = codegen.builder.build_alloca(llvm_type, &format!("param_{}", i)).unwrap();
                            let _ = codegen.builder.build_store(param_alloca, param_val);
                            locals_stack.last_mut().unwrap().insert(
                                format!("param_{}", i),
                                (param_alloca, llvm_type.as_basic_type_enum(), true, false),
                            );
                        }

                        // Execute constructor body if it exists
                        if let Some(body) = &ctor.body {
                            for stmt in &body.stmts {
                                use deno_ast::swc::ast::Stmt;
                                match stmt {
                                    Stmt::Expr(expr_stmt) => {
                                        // This will trigger member-write lowering for `this.x = x` etc.
                                        let _ = codegen.lower_expr(
                                            &expr_stmt.expr,
                                            func,
                                            &param_map,
                                            &mut locals_stack,
                                        );
                                    }
                                    _ => {} // Skip other statement types for now
                                }
                            }
                        }

                        // Clean up locals
                        codegen.emit_rc_dec_for_locals(&locals_stack);

                        // Return the object pointer
                        let obj_bv: inkwell::values::BasicValueEnum = obj_ptr.into();
                        let _ = codegen.builder.build_return(Some(&obj_bv));
                    }
                    _ => {}
                }
            }
        }
    }

    // Emit top-level helper functions (non-exported) found in the module so
    // calls to them can be lowered. Skip exported `main` which we handle
    // separately.
    for item in parsed.program_ref().body() {
        use deno_ast::swc::ast;
        // non-exported function declarations: `function foo() {}`
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item
            && let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt
        {
            let fname = fdecl.ident.sym.to_string();
            let inner_func = (*fdecl.function).clone();
            let mut inner_symbols = SymbolTable::new();
            let fsig = check_function_strictness(&inner_func, &mut inner_symbols)?;
            // skip exported `main` (we handle exported main separately later)
            if fname != "main" {
                codegen.gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None);
            }
        }

        // exported declarations: `export function foo() {}` — emit these too
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let ast::Decl::Fn(fdecl) = &decl.decl
        {
            let fname = fdecl.ident.sym.to_string();
            let inner_func = (*fdecl.function).clone();
            let mut inner_symbols = SymbolTable::new();
            let fsig = check_function_strictness(&inner_func, &mut inner_symbols)?;
            if fname != "main" {
                codegen.gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None);
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
        None,
    );

    // Try to emit a host `main` into the module so no external shim is
    // required. Recompute IR after emission.
    let emitted_host_main = codegen.emit_host_main(&func_sig.params, &func_sig.ret);

    let ir = codegen.module.print_to_string().to_string();

    // determine output directory (optional)
    let out_dir = std::env::var("OATS_OUT_DIR").unwrap_or_else(|_| ".".to_string());
    
    // Create output filename based on input filename
    let src_filename = std::path::Path::new(&src_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("out");
    let out_ll = format!("{}/{}.ll", out_dir, src_filename);
    let out_exe = format!("{}/{}", out_dir, src_filename);
    let out_obj = format!("{}/{}.o", out_dir, src_filename);
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
        .ok_or_else(|| {
            anyhow::anyhow!("runtime staticlib not found; please build the runtime crate")
        })?;

    // Compile IR to object file using clang
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
        anyhow::bail!(
            "No rt_main.o found and no runtime/rt_main/src/main.rs available; please provide a runtime main (rt_main.o) or add a runtime/rt_main/src/main.rs"
        );
    };
    // Link final binary with clang. If we emitted the host `main` in the
    // module then `rt_main_obj` will be empty and we skip adding it to the
    // link line.
    let mut link_cmd = Command::new("clang");
    link_cmd.arg("-O2");
    if !rt_main_obj.is_empty() {
        link_cmd.arg(&rt_main_obj);
    }
    link_cmd.arg(&out_obj).arg(rust_lib).arg("-o").arg(&out_exe);
    let status = link_cmd.status()?;
    if !status.success() {
        anyhow::bail!("clang failed to link final binary");
    }

    // run produced program and forward its exit code. This keeps program
    // output visible while making the runner behave like a thin wrapper.
    let run = Command::new(&out_exe).status()?;
    if let Some(code) = run.code() {
        if code != 0 {
            // Exit with the same code as the produced program so callers
            // can observe the program's result without the runner converting
            // it into an error.
            std::process::exit(code);
        }
    } else {
        // If there is no exit code (terminated by signal), return an error.
        anyhow::bail!("running out failed (terminated by signal)");
    }

    Ok(())
}
