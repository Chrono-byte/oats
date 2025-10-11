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

    // Note: the older textual `rewrite_top_level_arrow_decls` helper was removed
    // in favor of the AST-based `rewrite_top_level_arrows_ast` implementation
    // defined later in this function. Keeping the AST-based approach ensures
    // more accurate rewrites and avoids duplicated parsing logic.

    // First parse the original source to run the parser-level checks
    let initial_parsed_mod = parser::parse_oats_module(&source, Some(&src_path))?;

    // Rewrite top-level arrow bindings into function declarations
    fn rewrite_top_level_arrows_ast(parsed: &deno_ast::ParsedSource, source: &str) -> String {
        use deno_ast::swc::ast;

        // Collect replacements as (start, end, replacement_text)
        let mut repls: Vec<(usize, usize, String)> = Vec::new();

        for item in parsed.program_ref().body() {
            match item {
                deno_ast::ModuleItemRef::Stmt(stmt) => {
                    if let ast::Stmt::Decl(ast::Decl::Var(vdecl)) = stmt {
                        // Only handle single-declarator top-level const/let bindings
                        if vdecl.decls.len() != 1 {
                            continue;
                        }
                        // only const/let (not var)
                        if matches!(vdecl.kind, ast::VarDeclKind::Var) {
                            continue;
                        }
                        let decl = &vdecl.decls[0];
                        // Only simple identifier patterns
                        if let ast::Pat::Ident(binding_ident) = &decl.name
                            && let Some(init_expr) = &decl.init
                            && let ast::Expr::Arrow(_arrow) = &**init_expr
                        {
                            // Rather than rely on child spans (which may not include
                            // opening paren), scan from the var decl to find '=' then
                            // parse params/ret/=>/body from the source text to preserve
                            // original formatting and types.
                            let decl_lo = vdecl.span.lo.0 as usize;
                            // compute line start
                            let start = source[..decl_lo].rfind('\n').map(|i| i + 1).unwrap_or(0);
                            // find '=' after decl_lo
                            let mut eq = None;
                            let mut p = decl_lo;
                            while p < source.len() {
                                let ch = source.as_bytes()[p];
                                if ch == b'=' {
                                    eq = Some(p);
                                    break;
                                }
                                p += 1;
                            }
                            let eq = match eq {
                                Some(x) => x,
                                None => continue,
                            };
                            // scan forward from eq to find '=>' and parse params/ret/body
                            let mut iidx = eq + 1;
                            while iidx < source.len()
                                && source.as_bytes()[iidx].is_ascii_whitespace()
                            {
                                iidx += 1;
                            }
                            // params
                            let (params_text, after_params_idx) =
                                if iidx < source.len() && source.as_bytes()[iidx] == b'(' {
                                    // collect balanced parens
                                    let mut depth = 0i32;
                                    let mut j = iidx;
                                    while j < source.len() {
                                        let ch = source.as_bytes()[j];
                                        if ch == b'(' {
                                            depth += 1;
                                        } else if ch == b')' {
                                            depth -= 1;
                                            if depth == 0 {
                                                j += 1;
                                                break;
                                            }
                                        }
                                        j += 1;
                                    }
                                    (source[iidx..j].to_string(), j)
                                } else {
                                    // single identifier param
                                    let mut j = iidx;
                                    while j < source.len()
                                        && (source.as_bytes()[j] as char).is_alphanumeric()
                                        || source.as_bytes()[j] == b'_'
                                    {
                                        j += 1;
                                    }
                                    (format!("({})", source[iidx..j].trim()), j)
                                };
                            // skip whitespace and optional return type
                            let mut j = after_params_idx;
                            while j < source.len() && (source.as_bytes()[j] as char).is_whitespace()
                            {
                                j += 1;
                            }
                            let mut ret_text = String::new();
                            if j < source.len() && source.as_bytes()[j] == b':' {
                                let rt_start = j;
                                // scan until =>
                                while j < source.len()
                                    && !(source.as_bytes()[j] == b'='
                                        && j + 1 < source.len()
                                        && source.as_bytes()[j + 1] == b'>')
                                {
                                    j += 1;
                                }
                                ret_text = source[rt_start..j].trim().to_string();
                            }
                            // find =>
                            let mut arrow_pos = None;
                            while j + 1 < source.len() {
                                if source.as_bytes()[j] == b'=' && source.as_bytes()[j + 1] == b'>'
                                {
                                    arrow_pos = Some(j);
                                    break;
                                }
                                j += 1;
                            }
                            let arrow_pos = match arrow_pos {
                                Some(x) => x,
                                None => continue,
                            };
                            let mut body_start = arrow_pos + 2;
                            while body_start < source.len()
                                && (source.as_bytes()[body_start] as char).is_whitespace()
                            {
                                body_start += 1;
                            }
                            // body: block or expression
                            let (body_text, body_end) = if body_start < source.len()
                                && source.as_bytes()[body_start] == b'{'
                            {
                                let mut bd = body_start;
                                let mut depth = 0i32;
                                while bd < source.len() {
                                    let ch = source.as_bytes()[bd];
                                    if ch == b'{' {
                                        depth += 1;
                                    } else if ch == b'}' {
                                        depth -= 1;
                                        if depth == 0 {
                                            bd += 1;
                                            break;
                                        }
                                    }
                                    bd += 1;
                                }
                                (source[body_start..bd].to_string(), bd)
                            } else {
                                let mut bd = body_start;
                                while bd < source.len()
                                    && source.as_bytes()[bd] != b';'
                                    && source.as_bytes()[bd] != b'\n'
                                {
                                    bd += 1;
                                }
                                (
                                    format!("{{ return {}; }}", source[body_start..bd].trim()),
                                    bd,
                                )
                            };

                            let func_decl = if ret_text.is_empty() {
                                format!(
                                    "function {}{} {}\n",
                                    binding_ident.id.sym, params_text, body_text
                                )
                            } else {
                                format!(
                                    "function {}{} {} {}\n",
                                    binding_ident.id.sym, params_text, ret_text, body_text
                                )
                            };

                            repls.push((start, body_end, func_decl));
                        }
                    }
                }
                deno_ast::ModuleItemRef::ModuleDecl(module_decl) => {
                    // handle `export const foo = (...) => ...;`
                    if let ast::ModuleDecl::ExportDecl(decl) = module_decl
                        && let ast::Decl::Var(vdecl) = &decl.decl
                    {
                        if vdecl.decls.len() != 1 {
                            continue;
                        }
                        if matches!(vdecl.kind, ast::VarDeclKind::Var) {
                            continue;
                        }
                        let declarator = &vdecl.decls[0];
                        if let ast::Pat::Ident(binding_ident) = &declarator.name
                            && let Some(init_expr) = &declarator.init
                            && let ast::Expr::Arrow(arrow) = &**init_expr
                        {
                            // For export decl use the inner decl span so we cover
                            // exactly the `export const ...;` range
                            let start = decl.span.lo.0 as usize;
                            let mut end = decl.span.hi.0 as usize;
                            let sbytes = source.as_bytes();
                            while end < sbytes.len() && sbytes[end] != b'\n' {
                                end += 1;
                            }
                            if end < sbytes.len() {
                                end += 1;
                            }

                            // Use arrow span-based extraction similar to the non-export path
                            let alo = arrow.span.lo.0 as usize;
                            let ahi = arrow.span.hi.0 as usize;
                            let arrow_src = if alo < ahi && ahi <= source.len() {
                                &source[alo..ahi]
                            } else {
                                ""
                            };
                            if let Some(apos) = arrow_src.find("=>") {
                                let left = arrow_src[..apos].trim();
                                let right = arrow_src[apos + 2..].trim();
                                let params_text = if let Some(lp) = left.find('(') {
                                    // find matching )
                                    let mut depth = 0i32;
                                    let mut match_idx: Option<usize> = None;
                                    for (i, ch) in left.char_indices().skip(lp) {
                                        if ch == '(' {
                                            depth += 1;
                                        } else if ch == ')' {
                                            depth -= 1;
                                            if depth == 0 {
                                                match_idx = Some(i);
                                                break;
                                            }
                                        }
                                    }
                                    if let Some(rp) = match_idx {
                                        left[lp..=rp].to_string()
                                    } else {
                                        format!("({})", left)
                                    }
                                } else {
                                    let mut end_idx = left.len();
                                    if let Some(colon) = left.find(':') {
                                        end_idx = colon;
                                    }
                                    let id = left[..end_idx].trim();
                                    format!("({})", id)
                                };
                                let mut ret_text = String::new();
                                if let Some(params_pos) = left.find(&params_text) {
                                    let after = left[params_pos + params_text.len()..].trim_start();
                                    if after.starts_with(':') {
                                        let rt = after;
                                        if !rt.is_empty() {
                                            ret_text = format!(" {}", rt);
                                        }
                                    }
                                }
                                let body_text = if right.starts_with('{') {
                                    let mut depth = 0i32;
                                    let mut end_rel: Option<usize> = None;
                                    for (i, ch) in right.char_indices() {
                                        if ch == '{' {
                                            depth += 1;
                                        } else if ch == '}' {
                                            depth -= 1;
                                            if depth == 0 {
                                                end_rel = Some(i);
                                                break;
                                            }
                                        }
                                    }
                                    if let Some(er) = end_rel {
                                        right[..=er].to_string()
                                    } else {
                                        format!("{{ {} }}", right)
                                    }
                                } else {
                                    format!("{{ return {}; }}", right.trim_end_matches(';').trim())
                                };
                                let name = binding_ident.id.sym.to_string();
                                let func_decl = format!(
                                    "export function {}{}{} {}\n",
                                    name, params_text, ret_text, body_text
                                );
                                repls.push((start, end, func_decl));
                            }
                        }
                    }
                }
            }
        }

        if repls.is_empty() {
            return source.to_string();
        }

        // Apply replacements from end->start so indices remain valid
        repls.sort_by_key(|r| r.0);
        let mut out = String::new();
        let mut last = 0usize;
        for (start, end, text) in repls.iter() {
            if *start < last {
                continue;
            }
            out.push_str(&source[last..*start]);
            out.push_str(text);
            last = *end;
        }
        out.push_str(&source[last..]);
        out
    }

    let transformed_source = rewrite_top_level_arrows_ast(&initial_parsed_mod.parsed, &source);

    let parsed_mod = parser::parse_oats_module(&transformed_source, Some(&src_path))?;
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
    // NOTE: This function is now consolidated in oats::types::infer_type_from_expr

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

    let func_decl = if let Some(f) = func_decl_opt {
        f
    } else {
        return diagnostics::report_error_and_bail(
            Some(&src_path),
            Some(&source),
            "No exported `main` function found in script. Please export `function main(...)`.",
            Some("Scripts must export a `main` function to serve as the program entrypoint."),
        );
    };

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
        fn_number_to_string: std::cell::RefCell::new(None),
        fn_union_box_f64: std::cell::RefCell::new(None),
        fn_union_box_ptr: std::cell::RefCell::new(None),
        fn_union_unbox_f64: std::cell::RefCell::new(None),
        fn_union_unbox_ptr: std::cell::RefCell::new(None),
        fn_rc_weak_inc: std::cell::RefCell::new(None),
        fn_rc_weak_dec: std::cell::RefCell::new(None),
        fn_rc_weak_upgrade: std::cell::RefCell::new(None),
        fn_union_get_discriminant: std::cell::RefCell::new(None),
        class_fields: std::cell::RefCell::new(std::collections::HashMap::new()),
    
        fn_param_types: std::cell::RefCell::new(std::collections::HashMap::new()),
        loop_context_stack: std::cell::RefCell::new(Vec::new()),
        current_class_parent: std::cell::RefCell::new(None),
        closure_local_rettype: std::cell::RefCell::new(std::collections::HashMap::new()),
        last_expr_origin_local: std::cell::RefCell::new(None),
        async_await_counter: std::cell::Cell::new(0),
        async_await_live_sets: std::cell::RefCell::new(None),
        async_cont_blocks: std::cell::RefCell::new(None),
        async_local_name_to_slot: std::cell::RefCell::new(None),
        async_param_count: std::cell::Cell::new(0),
    async_poll_function: std::cell::RefCell::new(None),
    async_resume_blocks: std::cell::RefCell::new(None),
    async_poll_locals: std::cell::RefCell::new(None),
        source: &parsed_mod.source,
    };

    // Note: class field metadata is computed per-class when emitting
    // constructors below. We avoid a global pre-scan of the module so
    // member-field inference is based on declared/annotated types only.

    // Emit IR for class methods/constructors. Emit for both exported and
    // non-exported class declarations so constructors are available for
    // `new` expressions regardless of export status.
    for item_ref in parsed.program_ref().body() {
        // Handle exported class declarations: `export class Foo {}`
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item_ref
            && let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl
            && let deno_ast::swc::ast::Decl::Class(c) = &decl.decl
        {
            let class_name = c.ident.sym.to_string();
            // If this class extends a parent, record the parent name so constructors
            // and `super(...)` lowering can find the parent's initializer.
            let parent_name_opt = if let Some(sc) = &c.class.super_class {
                if let deno_ast::swc::ast::Expr::Ident(id) = &**sc {
                    Some(id.sym.to_string())
                } else {
                    None
                }
            } else {
                None
            };
            *codegen.current_class_parent.borrow_mut() = parent_name_opt.clone();

            // Emit members for this class
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
                        if let Ok(sig) = check_function_strictness(&m.function, &mut method_symbols)
                        {
                            // Prepend `this` as the first param (nominal struct pointer)
                            let mut params = Vec::new();
                            params.push(oats::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &m.function, &params, &ret, Some("this"))
                                .map_err(|d| {
                                    oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                                    anyhow::anyhow!(d.message)
                                })?;
                        } else {
                            // If strict check failed (e.g., missing return annotation), try to emit with Void return
                            let mut method_symbols = SymbolTable::new();
                            if let Ok(sig2) =
                                check_function_strictness(&m.function, &mut method_symbols)
                            {
                                let mut params = Vec::new();
                                params
                                    .push(oats::types::OatsType::NominalStruct(class_name.clone()));
                                params.extend(sig2.params.into_iter());
                                let fname = format!("{}_{}", class_name, mname);
                                codegen
                                    .gen_function_ir(
                                        &fname,
                                        &m.function,
                                        &params,
                                        &oats::types::OatsType::Void,
                                        Some("this"),
                                    )
                                    .map_err(|d| {
                                        oats::diagnostics::emit_diagnostic(
                                            &d,
                                            Some(source.as_str()),
                                        );
                                        anyhow::anyhow!(d.message)
                                    })?;
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Compute fields for this class from explicit props, constructor
                        // param properties, and `this.x = ...` assignments inside the ctor.
                        let mut fields: Vec<(String, oats::types::OatsType)> = Vec::new();
                        use deno_ast::swc::ast::{
                            ClassMember, Expr, MemberProp, ParamOrTsParamProp, Stmt,
                            TsParamPropParam,
                        };
                        // explicit class properties
                        for m in &c.class.body {
                            if let ClassMember::ClassProp(prop) = m
                                && let deno_ast::swc::ast::PropName::Ident(id) = &prop.key
                            {
                                let fname = id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    // If the class property has a TypeScript type annotation, map it
                                    // to an OatsType; otherwise default to Number.
                                    let ftype = if let Some(type_ann) = &prop.type_ann {
                                        if let Some(mt) =
                                            oats::types::map_ts_type(&type_ann.type_ann)
                                        {
                                            mt
                                        } else {
                                            oats::types::OatsType::Number
                                        }
                                    } else {
                                        oats::types::OatsType::Number
                                    };
                                    fields.push((fname, ftype));
                                }
                            }
                        }
                        // constructor param properties
                        for p in &ctor.params {
                            if let ParamOrTsParamProp::TsParamProp(ts_param) = p
                                && let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                            {
                                let fname = binding_ident.id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ty = oats::types::infer_type(
                                        binding_ident.type_ann.as_ref().map(|ann| &*ann.type_ann),
                                        None,
                                    );
                                    fields.push((fname, ty));
                                }
                            }
                        }
                        // scan ctor body for `this.x = ...` assignments
                        if let Some(body) = &ctor.body {
                            for stmt in &body.stmts {
                                if let Stmt::Expr(expr_stmt) = stmt
                                    && let Expr::Assign(assign) = &*expr_stmt.expr
                                    && let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                        &assign.left
                                    && let deno_ast::swc::ast::SimpleAssignTarget::Member(mem) =
                                        simple_target
                                    && matches!(&*mem.obj, Expr::This(_))
                                    && let MemberProp::Ident(ident) = &mem.prop
                                {
                                    let name = ident.sym.to_string();
                                    // Try to infer type from RHS expression. If the RHS is a
                                    // constructor parameter identifier, prefer its declared
                                    // type annotation when available.
                                    let mut inferred =
                                        oats::types::infer_type(None, Some(&assign.right));
                                    // If RHS is an identifier, try to look up a matching
                                    // constructor parameter and use its annotation.
                                    if let oats::types::OatsType::Number = inferred
                                        && let Expr::Ident(rhs_ident) = &*assign.right
                                    {
                                        for p in &ctor.params {
                                            use deno_ast::swc::ast::{
                                                ParamOrTsParamProp, TsParamPropParam,
                                            };
                                            match p {
                                                ParamOrTsParamProp::Param(param) => {
                                                    if let deno_ast::swc::ast::Pat::Ident(
                                                        bind_ident,
                                                    ) = &param.pat
                                                        && bind_ident.id.sym == rhs_ident.sym
                                                        && let Some(type_ann) = &bind_ident.type_ann
                                                        && let Some(mt) = oats::types::map_ts_type(
                                                            &type_ann.type_ann,
                                                        )
                                                    {
                                                        inferred = mt;
                                                        break;
                                                    }
                                                }
                                                ParamOrTsParamProp::TsParamProp(ts_param) => {
                                                    if let TsParamPropParam::Ident(binding_ident) =
                                                        &ts_param.param
                                                        && binding_ident.id.sym == rhs_ident.sym
                                                        && let Some(type_ann) =
                                                            &binding_ident.type_ann
                                                        && let Some(mt) = oats::types::map_ts_type(
                                                            &type_ann.type_ann,
                                                        )
                                                    {
                                                        inferred = mt;
                                                        break;
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if fields.iter().all(|(n, _)| n != &name) {
                                        fields.push((name, inferred));
                                    }
                                }
                            }
                        }
                        // Register computed fields so lowering can reference them
                        codegen
                            .class_fields
                            .borrow_mut()
                            .insert(class_name.clone(), fields.clone());
                        if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields) {
                            diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
                            return Err(anyhow::anyhow!(d.message));
                        }
                    }
                    _ => {}
                }
            }
            // Done emitting this class; clear current parent
            codegen.current_class_parent.borrow_mut().take();
        }

        // Also handle non-exported top-level class declarations: `class Foo {}`
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item_ref
            && let deno_ast::swc::ast::Stmt::Decl(deno_ast::swc::ast::Decl::Class(c)) = stmt
        {
            let class_name = c.ident.sym.to_string();
            let parent_name_opt = if let Some(sc) = &c.class.super_class {
                if let deno_ast::swc::ast::Expr::Ident(id) = &**sc {
                    Some(id.sym.to_string())
                } else {
                    None
                }
            } else {
                None
            };
            *codegen.current_class_parent.borrow_mut() = parent_name_opt.clone();
            for member in &c.class.body {
                use deno_ast::swc::ast::ClassMember;
                match member {
                    ClassMember::Method(m) => {
                        let mname = match &m.key {
                            deno_ast::swc::ast::PropName::Ident(id) => id.sym.to_string(),
                            deno_ast::swc::ast::PropName::Str(s) => s.value.to_string(),
                            _ => continue,
                        };
                        let mut method_symbols = SymbolTable::new();
                        if let Ok(sig) = check_function_strictness(&m.function, &mut method_symbols)
                        {
                            let mut params = Vec::new();
                            params.push(oats::types::OatsType::NominalStruct(class_name.clone()));
                            params.extend(sig.params.into_iter());
                            let ret = sig.ret;
                            let fname = format!("{}_{}", class_name, mname);
                            codegen
                                .gen_function_ir(&fname, &m.function, &params, &ret, Some("this"))
                                .map_err(|d| {
                                    oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                                    anyhow::anyhow!(d.message)
                                })?;
                        } else {
                            let mut method_symbols = SymbolTable::new();
                            if let Ok(sig2) =
                                check_function_strictness(&m.function, &mut method_symbols)
                            {
                                let mut params = Vec::new();
                                params
                                    .push(oats::types::OatsType::NominalStruct(class_name.clone()));
                                params.extend(sig2.params.into_iter());
                                let fname = format!("{}_{}", class_name, mname);
                                codegen
                                    .gen_function_ir(
                                        &fname,
                                        &m.function,
                                        &params,
                                        &oats::types::OatsType::Void,
                                        Some("this"),
                                    )
                                    .map_err(|d| {
                                        oats::diagnostics::emit_diagnostic(
                                            &d,
                                            Some(source.as_str()),
                                        );
                                        anyhow::anyhow!(d.message)
                                    })?;
                            }
                        }
                    }
                    ClassMember::Constructor(ctor) => {
                        // Compute fields for non-exported class similarly to exported case
                        let mut fields: Vec<(String, oats::types::OatsType)> = Vec::new();
                        use deno_ast::swc::ast::{
                            ClassMember, Expr, MemberProp, ParamOrTsParamProp, Stmt,
                            TsParamPropParam,
                        };
                        for m in &c.class.body {
                            if let ClassMember::ClassProp(prop) = m
                                && let deno_ast::swc::ast::PropName::Ident(id) = &prop.key
                            {
                                let fname = id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ftype = if let Some(type_ann) = &prop.type_ann {
                                        if let Some(mt) =
                                            oats::types::map_ts_type(&type_ann.type_ann)
                                        {
                                            mt
                                        } else {
                                            oats::types::OatsType::Number
                                        }
                                    } else {
                                        oats::types::OatsType::Number
                                    };
                                    fields.push((fname, ftype));
                                }
                            }
                        }
                        for p in &ctor.params {
                            if let ParamOrTsParamProp::TsParamProp(ts_param) = p
                                && let TsParamPropParam::Ident(binding_ident) = &ts_param.param
                            {
                                let fname = binding_ident.id.sym.to_string();
                                if fields.iter().all(|(n, _)| n != &fname) {
                                    let ty = oats::types::infer_type(
                                        binding_ident.type_ann.as_ref().map(|ann| &*ann.type_ann),
                                        None,
                                    );
                                    fields.push((fname, ty));
                                }
                            }
                        }
                        if let Some(body) = &ctor.body {
                            for stmt in &body.stmts {
                                if let Stmt::Expr(expr_stmt) = stmt
                                    && let Expr::Assign(assign) = &*expr_stmt.expr
                                    && let deno_ast::swc::ast::AssignTarget::Simple(simple_target) =
                                        &assign.left
                                    && let deno_ast::swc::ast::SimpleAssignTarget::Member(mem) =
                                        simple_target
                                    && matches!(&*mem.obj, Expr::This(_))
                                    && let MemberProp::Ident(ident) = &mem.prop
                                {
                                    let name = ident.sym.to_string();
                                    let inferred =
                                        oats::types::infer_type(None, Some(&assign.right));
                                    if fields.iter().all(|(n, _)| n != &name) {
                                        fields.push((name, inferred));
                                    }
                                }
                            }
                        }
                        codegen
                            .class_fields
                            .borrow_mut()
                            .insert(class_name.clone(), fields.clone());
                        if let Err(d) = codegen.gen_constructor_ir(&class_name, ctor, &fields) {
                            diagnostics::emit_diagnostic(&d, Some(parsed_mod.source.as_str()));
                            return Err(anyhow::anyhow!(d.message));
                        }
                    }
                    _ => {}
                }
            }
            // Clear parent after emitting this class
            codegen.current_class_parent.borrow_mut().take();
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
                codegen
                    .gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None)
                    .map_err(|d| {
                        oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                        anyhow::anyhow!("{}", d.message)
                    })?;
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
                codegen
                    .gen_function_ir(&fname, &inner_func, &fsig.params, &fsig.ret, None)
                    .map_err(|d| {
                        oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
                        anyhow::anyhow!("{}", d.message)
                    })?;
            }
        }
    }

    // Emit the user's exported `main` under an internal symbol name to avoid
    // conflicting with the C runtime entrypoint. The script must export
    // `main`, but we generate `oats_main` as the emitted symbol the host
    // runtime will call.
    codegen
        .gen_function_ir(
            "oats_main",
            &func_decl,
            &func_sig.params,
            &func_sig.ret,
            None,
        )
        .map_err(|d| {
            oats::diagnostics::emit_diagnostic(&d, Some(source.as_str()));
            anyhow::anyhow!("{}", d.message)
        })?;

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
    // Ensure output directory exists so File::create doesn't fail with ENOENT.
    if let Err(e) = std::fs::create_dir_all(&out_dir) {
        anyhow::bail!("failed to create output directory {}: {}", out_dir, e);
    }

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

    Ok(())
}
