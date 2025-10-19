use super::common::create_codegen;
use anyhow::Result;
use inkwell::context::Context;
use inkwell::targets::TargetMachine;
use oatsc::parser;
use oatsc::types::{SymbolTable, check_function_strictness};

#[test]
fn const_object_emits_meta_and_globals() -> Result<()> {
    let src = r#"
const cfg = { name: "Alice", nested: { val: 3.14 }, flag: true };

export function main(): number {
    return 0;
}
"#;

    // Parse module and find exported main
    let (parsed_mod_opt, _) = parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;
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
    let func_decl = func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported `main` found"))?;

    let mut symbols = SymbolTable::new();
    let (_func_sig_opt, _) = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("test_module");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let source_str = parsed_mod.source.clone();
    let codegen = create_codegen(&context, "test_module", symbols, &source_str)?;

    // Replicate builder's top-level const pass to emit const globals
    use deno_ast::swc::ast;
    use std::collections::{HashMap, HashSet, VecDeque};

    let mut top_level_consts: Vec<(String, Box<ast::Expr>, usize)> = Vec::new();
    for item in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(ast::Stmt::Decl(ast::Decl::Var(vd))) = item
            && matches!(vd.kind, ast::VarDeclKind::Const)
        {
            for decl in &vd.decls {
                if let ast::Pat::Ident(binding) = &decl.name
                    && let Some(init) = &decl.init
                {
                    let name = binding.id.sym.to_string();
                    let span_start = vd.span.lo.0 as usize;
                    top_level_consts.push((name, init.clone(), span_start));
                }
            }
        }
    }

    if !top_level_consts.is_empty() {
        let mut name_to_idx: HashMap<String, usize> = HashMap::new();
        for (i, (n, _, _)) in top_level_consts.iter().enumerate() {
            name_to_idx.insert(n.clone(), i);
        }

        fn collect_idents(e: &ast::Expr, out: &mut HashSet<String>) {
            use deno_ast::swc::ast::*;
            match e {
                Expr::Ident(id) => {
                    out.insert(id.sym.to_string());
                }
                Expr::Array(arr) => {
                    for el in arr.elems.iter().flatten() {
                        collect_idents(&el.expr, out);
                    }
                }
                Expr::Object(obj) => {
                    for prop in &obj.props {
                        if let PropOrSpread::Prop(pb) = prop
                            && let Prop::KeyValue(kv) = pb.as_ref()
                        {
                            collect_idents(&kv.value, out);
                        }
                    }
                }
                Expr::Unary(u) => collect_idents(&u.arg, out),
                Expr::Bin(b) => {
                    collect_idents(&b.left, out);
                    collect_idents(&b.right, out);
                }
                Expr::Call(c) => {
                    if let Callee::Expr(ec) = &c.callee {
                        collect_idents(ec, out);
                    }
                    for a in &c.args {
                        collect_idents(&a.expr, out);
                    }
                }
                Expr::Member(m) => {
                    collect_idents(&m.obj, out);
                    if let MemberProp::Computed(cmp) = &m.prop {
                        collect_idents(&cmp.expr, out);
                    }
                }
                Expr::New(n) => {
                    collect_idents(&n.callee, out);
                    if let Some(args) = &n.args {
                        for a in args {
                            collect_idents(&a.expr, out);
                        }
                    }
                }
                Expr::Paren(p) => collect_idents(&p.expr, out),
                Expr::Cond(c) => {
                    collect_idents(&c.test, out);
                    collect_idents(&c.cons, out);
                    collect_idents(&c.alt, out);
                }
                Expr::Tpl(_) | Expr::Lit(_) => {}
                _ => {}
            }
        }

        let mut adj: Vec<Vec<usize>> = vec![Vec::new(); top_level_consts.len()];
        let mut indeg: Vec<usize> = vec![0; top_level_consts.len()];

        for (i, (_n, init, _)) in top_level_consts.iter().enumerate() {
            let mut ids = HashSet::new();
            collect_idents(init, &mut ids);
            for id in ids {
                if let Some(&j) = name_to_idx.get(&id) {
                    adj[j].push(i);
                    indeg[i] += 1;
                }
            }
        }

        let mut q: VecDeque<usize> = VecDeque::new();
        for (i, &d) in indeg.iter().enumerate() {
            if d == 0 {
                q.push_back(i);
            }
        }
        let mut order: Vec<usize> = Vec::new();
        while let Some(u) = q.pop_front() {
            order.push(u);
            for &v in &adj[u] {
                indeg[v] -= 1;
                if indeg[v] == 0 {
                    q.push_back(v);
                }
            }
        }

        // Evaluate and emit
        for idx in order {
            let (name, init_expr, span_start) = &top_level_consts[idx];
            // Borrow const_items immutably for evaluation and drop when done to avoid
            // conflicting RefCell borrows when we later mutably insert the result.
            let const_map = codegen.const_items.borrow();
            match oatsc::codegen::const_eval::eval_const_expr(init_expr, *span_start, &const_map) {
                Ok(cv) => {
                    drop(const_map);
                    codegen
                        .const_items
                        .borrow_mut()
                        .insert(name.clone(), cv.clone());
                    match cv {
                        oatsc::codegen::const_eval::ConstValue::Str(_)
                        | oatsc::codegen::const_eval::ConstValue::Array(_)
                        | oatsc::codegen::const_eval::ConstValue::Object(_) => {
                            let gname = format!("const.{}", name);
                            match codegen.emit_const_global(&gname, &cv) {
                                Ok(ptr) => {
                                    codegen.const_globals.borrow_mut().insert(name.clone(), ptr);
                                }
                                Err(d) => {
                                    return Err(anyhow::anyhow!(d.message));
                                }
                            }
                        }
                        _ => {}
                    }
                }
                Err(d) => {
                    return Err(anyhow::anyhow!(d.message));
                }
            }
        }
    }

    // Emit the main function using codegen helpers
    let mut inner_symbols = SymbolTable::new();
    let (fsig_opt, _) = check_function_strictness(&func_decl, &mut inner_symbols)?;
    let fsig = fsig_opt.ok_or_else(|| anyhow::anyhow!("Failed to check function strictness"))?;
    codegen
        .gen_function_ir("oats_main", &func_decl, &fsig.params, &fsig.ret, None)
        .map_err(|d| anyhow::anyhow!(d.message))?;

    let ir = codegen.module.print_to_string().to_string();

    // Expect a global for the const and a metadata global. The emitter may
    // either use the legacy name `@const.<name>` or an interned global name
    // `@const.intern.<hash>`; accept either form.
    let has_legacy = ir.contains("@const.cfg =") && ir.contains("@const.cfg_meta =");
    let has_intern = ir.contains("const.intern.") && ir.contains("_meta");
    assert!(
        has_legacy || has_intern,
        "neither legacy const.cfg nor interned const globals found in IR:\n{}",
        ir
    );

    // meta should contain offsets for pointer fields (24 and 32 bytes) in initializer
    assert!(
        ir.contains("[2 x i32] [i32 24, i32 32]"),
        "meta offsets not present in IR:\n{}",
        ir
    );

    // Also assert the meta0 value encodes META_MAGIC in the high 32 bits.
    let meta_magic: u64 = 0x4F415453u64; // 'OATS'
    let meta0 = ((meta_magic << 32) | 2u64).to_string();
    assert!(
        ir.contains(&format!("i64 {}", meta0)),
        "meta0 value not present in IR:\n{}",
        ir
    );

    Ok(())
}

#[test]
fn const_extended_eval() -> Result<()> {
    let src = r#"
const a = 5;
const b = 10;
const logical_and = a > 0 && b > 5; // true
const logical_or = a < 0 || b > 5;  // true
const conditional = a > b ? "greater" : "less"; // "less"
const math_abs = Math.abs(-3.14); // 3.14
const math_floor = Math.floor(3.9); // 3

export function main(): number {
    return 0;
}
"#;

    // Parse module and find exported main
    let (parsed_mod_opt, _) = parser::parse_oats_module(src, None)?;
    let parsed_mod = parsed_mod_opt.ok_or_else(|| anyhow::anyhow!("Failed to parse source"))?;
    let parsed = &parsed_mod.parsed;
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
    let func_decl = func_decl_opt.ok_or_else(|| anyhow::anyhow!("No exported `main` found"))?;

    let mut symbols = SymbolTable::new();
    let (_func_sig_opt, _) = check_function_strictness(&func_decl, &mut symbols)?;

    let context = Context::create();
    let module = context.create_module("test_module");
    let triple = TargetMachine::get_default_triple();
    module.set_triple(&triple);
    let source_str = parsed_mod.source.clone();
    let codegen = create_codegen(&context, "test_module", symbols, &source_str)?;

    // Replicate the const evaluation process
    use deno_ast::swc::ast;

    let mut top_level_consts: Vec<(String, Box<ast::Expr>, usize)> = Vec::new();
    for item in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(ast::Stmt::Decl(ast::Decl::Var(vd))) = item
            && matches!(vd.kind, ast::VarDeclKind::Const)
        {
            for decl in &vd.decls {
                if let ast::Pat::Ident(binding) = &decl.name
                    && let Some(init) = &decl.init
                {
                    let name = binding.id.sym.to_string();
                    let span_start = vd.span.lo.0 as usize;
                    top_level_consts.push((name, init.clone(), span_start));
                }
            }
        }
    }

    // For simplicity, evaluate in order without dependency analysis
    for (name, init_expr, span_start) in &top_level_consts {
        let const_map = codegen.const_items.borrow();
        match oatsc::codegen::const_eval::eval_const_expr(init_expr, *span_start, &const_map) {
            Ok(cv) => {
                drop(const_map);
                codegen
                    .const_items
                    .borrow_mut()
                    .insert(name.clone(), cv.clone());
                // Check expected values
                match name.as_str() {
                    "logical_and" => assert!(matches!(
                        cv,
                        oatsc::codegen::const_eval::ConstValue::Bool(true)
                    )),
                    "logical_or" => assert!(matches!(
                        cv,
                        oatsc::codegen::const_eval::ConstValue::Bool(true)
                    )),
                    "conditional" => assert!(
                        matches!(cv, oatsc::codegen::const_eval::ConstValue::Str(s) if s == "less")
                    ),
                    "math_abs" => assert!(
                        matches!(cv, oatsc::codegen::const_eval::ConstValue::Number(n) if (n - std::f64::consts::PI).abs() < 0.001)
                    ),
                    "math_floor" => assert!(
                        matches!(cv, oatsc::codegen::const_eval::ConstValue::Number(n) if n == 3.0)
                    ),
                    _ => {}
                }
            }
            Err(d) => {
                return Err(anyhow::anyhow!(
                    "Failed to evaluate {}: {}",
                    name,
                    d.message
                ));
            }
        }
    }

    Ok(())
}
