use crate::diagnostics;
use anyhow::Result;
use deno_ast::swc::ast;
use deno_ast::{parse_module, MediaType, ParseParams, ParsedSource, SourceTextInfo};
use std::sync::Arc;
use url::Url;

pub fn parse_oats_module(source_code: &str, file_path: Option<&str>) -> Result<ParsedSource> {
    let sti = SourceTextInfo::from_string(source_code.to_string());
    let params = ParseParams {
        specifier: Url::parse("file:///file.ts")?,
        text: Arc::from(sti.text().clone()),
        media_type: MediaType::TypeScript,
        // Capture tokens so we can validate semicolon presence if needed.
        capture_tokens: true,
        scope_analysis: false,
        maybe_syntax: None,
    };

    let parsed = parse_module(params)?;

    // Enforce semicolons for statement-terminated constructs (conservative)
    // This behavior mirrors languages like Rust/C++ where semicolons are
    // required after expression statements, returns, and declarations.
    fn stmt_requires_semicolon(stmt: &ast::Stmt) -> bool {
        match stmt {
            ast::Stmt::Expr(_) => true,
            ast::Stmt::Decl(ast::Decl::Var(_)) => true,
            ast::Stmt::Return(_) => true,
            ast::Stmt::Break(_) => true,
            ast::Stmt::Continue(_) => true,
            ast::Stmt::Throw(_) => true,
            ast::Stmt::Debugger(_) => true,
            // Empty stmt is just a semicolon and is fine
            _ => false,
        }
    }

    fn span_hi_usize(span: &deno_ast::swc::common::Span) -> usize {
        // BytePos is a wrapper around u32
        span.hi.0 as usize
    }

    // Helper: scan forward from span.hi to find the next non-whitespace,
    // non-comment character and check whether it's a semicolon. This
    // handles trailing comments (// or /* */) more robustly.
    fn has_trailing_semicolon(span: &deno_ast::swc::common::Span, source: &str, parsed: &ParsedSource) -> bool {
        let hi = span_hi_usize(span);
        let lo = span.lo.0 as usize;
        let bytes = source.as_bytes();
        let len = bytes.len();
        // If a semicolon appears anywhere inside the span, consider it present.
        if lo < len {
            let end = if hi > len { len } else { hi };
            if lo < end && bytes[lo..end].iter().any(|&b| b == b';') {
                return true;
            }
        }
        // quick checks: if semicolon is exactly at hi or immediately before
        if hi < len && bytes[hi] == b';' { return true; }
        if hi > 0 && bytes[hi.saturating_sub(1)] == b';' { return true; }
        // If tokens were captured, try a token-based check: find the token
        // immediately after the statement span and see if it's a semicolon.
    use swc_ecma_lexer::token::Token as SwcToken;
    let tok_slice = parsed.tokens();
    if !tok_slice.is_empty() {
            // tokens is a Vec of Token objects with spans; find the first
            // token whose span.lo >= span.hi (i.e., token after statement)
            for tok in tok_slice.iter() {
                // TokenAndSpan has `span` with lo/hi
                let tspan = tok.span;
                let tlo = tspan.lo.0 as usize;
                if tlo >= hi {
                    // Prefer matching token kind directly
                    if tok.token == SwcToken::Semi {
                        return true;
                    }
                    break;
                }
            }
        }
        let mut i = if hi >= len { len } else { hi };
        // skip whitespace and comments forward
        while i < bytes.len() {
            // skip ASCII whitespace
            if bytes[i] == b' ' || bytes[i] == b'\t' || bytes[i] == b'\r' || bytes[i] == b'\n' {
                i += 1;
                continue;
            }
            // line comment // -> skip to end of line
            if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i+1] == b'/' {
                i += 2;
                while i < bytes.len() && bytes[i] != b'\n' { i += 1; }
                continue;
            }
            // block comment /* ... */
            if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i+1] == b'*' {
                i += 2;
                while i + 1 < bytes.len() {
                    if bytes[i] == b'*' && bytes[i+1] == b'/' { i += 2; break; }
                    i += 1;
                }
                continue;
            }
            // found a non-whitespace/comment char
            return bytes[i] == b';';
        }
        false
    }

    // Walk top-level module items and function bodies
    for item in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
            if stmt_requires_semicolon(stmt) {
                // Get stmt span and inspect source
                let span = match stmt {
                    ast::Stmt::Expr(es) => es.span,
                    ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                    ast::Stmt::Return(r) => r.span,
                    ast::Stmt::Break(b) => b.span,
                    ast::Stmt::Continue(c) => c.span,
                    ast::Stmt::Throw(t) => t.span,
                    ast::Stmt::Debugger(d) => d.span,
                    _ => continue,
                };
                        if !has_trailing_semicolon(&span, source_code, &parsed) {
                            let start = span.lo.0 as usize;
                            return diagnostics::report_error_span_and_bail(
                                file_path,
                                source_code,
                                start,
                                "missing semicolon: statements must end with ';'",
                                Some("oats requires semicolons like Rust/C++: add a trailing ';' to this statement."),
                            );
                        }
            }
            // If it's a function decl, also inspect its body
            if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt {
                if let Some(body) = &fdecl.function.body {
                    for s in &body.stmts {
                        if stmt_requires_semicolon(s) {
                            let span = match s {
                                ast::Stmt::Expr(es) => es.span,
                                ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                                ast::Stmt::Return(r) => r.span,
                                ast::Stmt::Break(b) => b.span,
                                ast::Stmt::Continue(c) => c.span,
                                ast::Stmt::Throw(t) => t.span,
                                ast::Stmt::Debugger(d) => d.span,
                                _ => continue,
                            };
                            if !has_trailing_semicolon(&span, source_code, &parsed) {
                                let start = span.lo.0 as usize;
                                return diagnostics::report_error_span_and_bail(
                                    file_path,
                                    source_code,
                                    start,
                                    "missing semicolon: statements must end with ';'",
                                    Some("oats requires semicolons like Rust/C++: add a trailing ';' to this statement."),
                                );
                            }
                        }
                    }
                }
            }
        }
        if let deno_ast::ModuleItemRef::ModuleDecl(module_decl) = item {
            // export declarations that are vars should also be terminated
            if let deno_ast::swc::ast::ModuleDecl::ExportDecl(decl) = module_decl {
                if let ast::Decl::Var(vdecl) = &decl.decl {
                    if !has_trailing_semicolon(&vdecl.span, source_code, &parsed) {
                        let start = vdecl.span.lo.0 as usize;
                        return diagnostics::report_error_span_and_bail(
                            file_path,
                            source_code,
                            start,
                            "missing semicolon after export declaration",
                            Some("oats requires semicolons like Rust/C++: add a trailing ';' to this export declaration."),
                        );
                    }
                }
                if let ast::Decl::Fn(fdecl) = &decl.decl {
                    if let Some(body) = &fdecl.function.body {
                        for s in &body.stmts {
                            if stmt_requires_semicolon(s) {
                                let span = match s {
                                    ast::Stmt::Expr(es) => es.span,
                                    ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                                    ast::Stmt::Return(r) => r.span,
                                    ast::Stmt::Break(b) => b.span,
                                    ast::Stmt::Continue(c) => c.span,
                                    ast::Stmt::Throw(t) => t.span,
                                    ast::Stmt::Debugger(d) => d.span,
                                    _ => continue,
                                };
                                if !has_trailing_semicolon(&span, source_code, &parsed) {
                                    let start = span.lo.0 as usize;
                                    return diagnostics::report_error_span_and_bail(
                                        file_path,
                                        source_code,
                                        start,
                                        "missing semicolon: statements must end with ';'",
                                        Some("oats requires semicolons like Rust/C++: add a trailing ';' to this statement."),
                                    );
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    Ok(parsed)
}
