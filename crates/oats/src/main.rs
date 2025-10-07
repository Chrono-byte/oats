use anyhow::Result;

use oats::codegen::CodeGen;
use oats::parser;
use oats::types::{OatsType, SymbolTable, check_function_strictness};

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
        anyhow::bail!(
            "No source file provided. Pass path as first arg or set OATS_SRC_FILE env var."
        );
    };

    let source = std::fs::read_to_string(&src_path)?;

    // Parse (supports preprocessor for `let mut` annotations)
    let parsed_mod = parser::parse_oats_module(&source, None)?;
    let parsed = &parsed_mod.parsed;

    // Require the user script to export a `main` function as the program entrypoint
    // Also pre-populate the symbol table with imported names and exported class
    // declarations so nominal type references can be resolved during typecheck.
    let mut func_decl_opt: Option<deno_ast::swc::ast::Function> = None;
    let mut pre_symbols = SymbolTable::new();
    for item_ref in parsed.program_ref().body() {
        match item_ref {
            deno_ast::ModuleItemRef::ModuleDecl(module_decl) => {
                match module_decl {
                    deno_ast::swc::ast::ModuleDecl::Import(import_decl) => {
                        // register each imported local name as a nominal type placeholder
                        for spec in &import_decl.specifiers {
                            match spec {
                                deno_ast::swc::ast::ImportSpecifier::Named(named) => {
                                    let local = named.local.sym.to_string();
                                    pre_symbols
                                        .insert(local.clone(), OatsType::NominalStruct(local));
                                }
                                deno_ast::swc::ast::ImportSpecifier::Default(d) => {
                                    let local = d.local.sym.to_string();
                                    pre_symbols
                                        .insert(local.clone(), OatsType::NominalStruct(local));
                                }
                                deno_ast::swc::ast::ImportSpecifier::Namespace(ns) => {
                                    // import * as ns from '...'; treat namespace as nominal too
                                    let local = ns.local.sym.to_string();
                                    pre_symbols
                                        .insert(local.clone(), OatsType::NominalStruct(local));
                                }
                            }
                        }
                    }
                    deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) => {
                        if let deno_ast::swc::ast::Decl::Class(c) = &decl.decl {
                            // export class Foo { ... } -> register Foo as nominal struct
                            let name = c.ident.sym.to_string();
                            pre_symbols.insert(name.clone(), OatsType::NominalStruct(name));
                            // Collect declared fields (property declarations) and store
                            // them into CodeGen later. We can't access CodeGen here yet,
                            // but main will build CodeGen and can populate its
                            // `class_fields` before emitting methods — that happens
                            // after CodeGen construction below.
                        }
                        if let deno_ast::swc::ast::Decl::Fn(f) = &decl.decl {
                            let name = f.ident.sym.to_string();
                            if name == "main" {
                                func_decl_opt = Some((*f.function).clone());
                            }
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    let func_decl = func_decl_opt.ok_or_else(|| {
        anyhow::anyhow!(
            "No exported `main` function found. Please export `function main(...)` in your script."
        )
    })?;

    // Type check: start with pre-collected symbols (imports/classes)
    let mut symbols = pre_symbols;
    let func_sig = check_function_strictness(&func_decl, &mut symbols)?;

    // LLVM setup
    let context = Context::create();
    let module = context.create_module("oats");
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

    // Helper: infer a simple OatsType from an expression (literals and simple arrays)
    fn infer_from_expr(e: &deno_ast::swc::ast::Expr) -> Option<OatsType> {
        use deno_ast::swc::ast;
        use deno_ast::swc::ast::Expr;
        match e {
            Expr::Lit(lit) => match &*lit {
                ast::Lit::Num(_) => Some(OatsType::Number),
                ast::Lit::Str(_) => Some(OatsType::String),
                ast::Lit::Bool(_) => Some(OatsType::Boolean),
                _ => None,
            },
            Expr::Array(arr) => {
                if let Some(Some(first)) = arr.elems.get(0) {
                    infer_from_expr(&first.expr).map(|et| OatsType::Array(Box::new(et)))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // Populate class_fields for exported classes by examining ClassProp
    // declarations and constructor assignment ASTs (this.x = ...).
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Class(c) = &decl.decl {
                    let class_name = c.ident.sym.to_string();
                    let mut fields: Vec<(String, OatsType)> = Vec::new();
                    use deno_ast::swc::ast::{ClassMember, Expr, MemberProp, Stmt};
                    // Collect explicit property declarations
                    for member in &c.class.body {
                        if let ClassMember::ClassProp(prop) = member {
                            if let deno_ast::swc::ast::PropName::Ident(id) = &prop.key {
                                let fname = id.sym.to_string();
                                if !fields.iter().any(|(n, _)| n == &fname) {
                                    fields.push((fname, OatsType::Number));
                                }
                            }
                        }
                    }
                    // Scan constructor ASTs for `this.<ident> = <expr>` assignments
                    if fields.is_empty() {
                        for member in &c.class.body {
                            if let ClassMember::Constructor(cons) = member {
                                if let Some(body) = &cons.body {
                                    for stmt in &body.stmts {
                                        if let Stmt::Expr(expr_stmt) = stmt {
                                            if let Expr::Assign(assign) = &*expr_stmt.expr {
                                                if let deno_ast::swc::ast::AssignTarget::Simple(
                                                    simple_target,
                                                ) = &assign.left
                                                {
                                                    // Match a simple member assignment like `this.x = ...`
                                                    if let deno_ast::swc::ast::SimpleAssignTarget::Member(mem) =
                                                        simple_target
                                                    {
                                                        if matches!(&*mem.obj, Expr::This(_)) {
                                                            if let MemberProp::Ident(ident) = &mem.prop {
                                                                let name = ident.sym.to_string();
                                                                let inferred = infer_from_expr(&*assign.right)
                                                                    .unwrap_or(OatsType::Number);
                                                                if fields.iter().all(|(n, _)| n != &name) {
                                                                    fields.push((name, inferred));
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
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
        }
    }

    // Generate IR
    // Emit class methods/constructors as top-level functions so they can be
    // linked and called. We lower each ClassDecl's members into functions
    // named `<Class>_<method>` and `<Class>_ctor` for constructors.
    for item_ref in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref {
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let deno_ast::swc::ast::Decl::Class(c) = &decl.decl {
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
                                if let Ok(sig) =
                                    check_function_strictness(&m.function, &mut symbols)
                                {
                                    // Prepend `this` as the first param (nominal struct pointer)
                                    let mut params = Vec::new();
                                    params.push(oats::types::OatsType::NominalStruct(
                                        class_name.clone(),
                                    ));
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
                                    if let Ok(sig2) = (|| {
                                        // map params manually; reuse check for params only via a small helper
                                        check_function_strictness(&m.function, &mut symbols)
                                    })() {
                                        let mut params = Vec::new();
                                        params.push(oats::types::OatsType::NominalStruct(
                                            class_name.clone(),
                                        ));
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
                            ClassMember::Constructor(_ctor) => {
                                // Emit a minimal placeholder constructor function
                                // that returns a null i8* (opaque NominalStruct ptr).
                                // This is a temporary placeholder until we implement
                                // full constructor semantics (allocation/field init).
                                let fname = format!("{}_ctor", class_name);
                                // create a function returning i8* with no params
                                // Ensure malloc is declared in the module (declare if missing)
                                if codegen.module.get_function("malloc").is_none() {
                                    let i8ptr =
                                        codegen.context.ptr_type(inkwell::AddressSpace::default());
                                    let i64t = codegen.i64_t;
                                    let malloc_ty = i8ptr.fn_type(&[i64t.into()], false);
                                    let f = codegen.module.add_function("malloc", malloc_ty, None);
                                    let _ = f; // keep for clarity
                                }
                                let i8ptr =
                                    codegen.context.ptr_type(inkwell::AddressSpace::default());
                                let fn_ty = i8ptr.fn_type(&[], false);
                                let f = codegen.module.add_function(&fname, fn_ty, None);
                                // emit a body that allocates a header word and
                                // initializes it to refcount=1 (low 32 bits = 1)
                                let entry = codegen.context.append_basic_block(f, "entry");
                                codegen.builder.position_at_end(entry);
                                // call malloc(size: i64) -> i8*
                                let malloc_fn = codegen
                                    .module
                                    .get_function("malloc")
                                    .expect("malloc should be declared");
                                // allocate 8 bytes for a single u64 header
                                let size_const = codegen
                                    .i64_t
                                    .const_int(std::mem::size_of::<u64>() as u64, false);
                                let call_site = codegen
                                    .builder
                                    .build_call(malloc_fn, &[size_const.into()], "call_malloc")
                                    .expect("build_call failed");
                                let either = call_site.try_as_basic_value();
                                let malloc_ret = if let inkwell::Either::Left(bv) = either {
                                    bv.into_pointer_value()
                                } else {
                                    // If malloc didn't produce a basic value, return null
                                    i8ptr.const_null()
                                };
                                // bitcast returned i8* to i64* and store header=1
                                let header_ptr = codegen
                                    .builder
                                    .build_pointer_cast(
                                        malloc_ret,
                                        codegen.context.ptr_type(inkwell::AddressSpace::default()),
                                        "hdr_ptr",
                                    )
                                    .expect("cast hdr_ptr failed");
                                // header value: low 32 bits = refcount (1), high bits = 0 (type tag)
                                let header_val = codegen.i64_t.const_int(1u64, false);
                                let _ = codegen.builder.build_store(header_ptr, header_val);
                                // return original i8* pointer
                                let _ = codegen.builder.build_return(Some(&malloc_ret));
                                // leave function in module
                            }
                            _ => {}
                        }
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
        None,
    );

    // Print IR
    println!("{}", codegen.module.print_to_string().to_string());

    Ok(())
}
