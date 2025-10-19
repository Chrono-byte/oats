//! Oats parser utilities
//!
//! This module wraps `deno_ast` parsing and enforces a small set of
//! project-specific diagnostics and stylistic constraints (for example,
//! requiring semicolons and banning `var`). The parser returns a
//! `ParsedModule` containing both the parsed AST and the original source
//! text, which are used by later passes to report span-based diagnostics.
//!
//! # Resource Limits (Security Hardening)
//!
//! To prevent denial-of-service attacks, the parser enforces the following limits:
//!
//! - **MAX_SOURCE_SIZE**: Maximum source file size (default: 10 MB)
//!   - Configurable via `OATS_MAX_SOURCE_BYTES` environment variable
//!   - Prevents memory exhaustion from extremely large input files
//!
//! - **Runtime Recursion Depth**: Maximum recursion depth during execution (32 levels)
//!   - Enforced in the runtime (`crates/runtime/src/lib.rs::MAX_RECURSION_DEPTH`)
//!   - Not configurable (hard limit)
//!
//! These limits provide defense-in-depth against malicious or malformed input.

use crate::diagnostics;
use anyhow::Result;
use deno_ast::swc::ast;
use deno_ast::{MediaType, ParseParams, ParsedSource, SourceTextInfo, parse_module};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use url::Url;

/// AST depth checker to prevent deeply nested structures that could cause stack overflow
struct AstDepthChecker {
    max_depth: usize,
}

impl AstDepthChecker {
    fn check_program(&self, program: &deno_ast::ParsedSource) -> Result<()> {
        let mut depth = 0;
        for item in program.program_ref().body() {
            self.check_module_item(&item, &mut depth)?;
        }
        Ok(())
    }

    fn check_module_item(&self, item: &deno_ast::ModuleItemRef, depth: &mut usize) -> Result<()> {
        match item {
            deno_ast::ModuleItemRef::Stmt(stmt) => self.check_stmt(stmt, depth),
            deno_ast::ModuleItemRef::ModuleDecl(decl) => self.check_module_decl(decl, depth),
        }
    }

    fn check_module_decl(&self, decl: &ast::ModuleDecl, depth: &mut usize) -> Result<()> {
        match decl {
            ast::ModuleDecl::Import(_)
            | ast::ModuleDecl::ExportAll(_)
            | ast::ModuleDecl::ExportNamed(_)
            | ast::ModuleDecl::ExportDefaultDecl(_)
            | ast::ModuleDecl::ExportDefaultExpr(_) => Ok(()),
            ast::ModuleDecl::ExportDecl(export) => self.check_decl(&export.decl, depth),
            ast::ModuleDecl::TsImportEquals(_)
            | ast::ModuleDecl::TsExportAssignment(_)
            | ast::ModuleDecl::TsNamespaceExport(_) => Ok(()),
        }
    }

    fn check_decl(&self, decl: &ast::Decl, depth: &mut usize) -> Result<()> {
        match decl {
            ast::Decl::Class(class) => self.check_class(&class.class, depth),
            ast::Decl::Fn(func) => self.check_function(&func.function, depth),
            ast::Decl::Var(_)
            | ast::Decl::TsInterface(_)
            | ast::Decl::TsTypeAlias(_)
            | ast::Decl::TsEnum(_)
            | ast::Decl::TsModule(_)
            | ast::Decl::Using(_) => Ok(()),
        }
    }

    fn check_stmt(&self, stmt: &ast::Stmt, depth: &mut usize) -> Result<()> {
        *depth += 1;
        if *depth > self.max_depth {
            anyhow::bail!("AST nesting depth exceeded");
        }

        match stmt {
            ast::Stmt::Block(block) => {
                for stmt in &block.stmts {
                    self.check_stmt(stmt, depth)?;
                }
            }
            ast::Stmt::If(if_stmt) => {
                self.check_expr(&if_stmt.test, depth)?;
                self.check_stmt(&if_stmt.cons, depth)?;
                if let Some(alt) = &if_stmt.alt {
                    self.check_stmt(alt, depth)?;
                }
            }
            ast::Stmt::For(for_stmt) => {
                if let Some(init) = &for_stmt.init {
                    match init {
                        ast::VarDeclOrExpr::VarDecl(vd) => {
                            self.check_decl(&ast::Decl::Var(vd.clone()), depth)?
                        }
                        ast::VarDeclOrExpr::Expr(e) => self.check_expr(e, depth)?,
                    }
                }
                if let Some(test) = &for_stmt.test {
                    self.check_expr(test, depth)?;
                }
                if let Some(update) = &for_stmt.update {
                    self.check_expr(update, depth)?;
                }
                self.check_stmt(&for_stmt.body, depth)?;
            }
            ast::Stmt::ForIn(for_in) => {
                self.check_expr(&for_in.right, depth)?;
                self.check_stmt(&for_in.body, depth)?;
            }
            ast::Stmt::ForOf(for_of) => {
                self.check_expr(&for_of.right, depth)?;
                self.check_stmt(&for_of.body, depth)?;
            }
            ast::Stmt::While(while_stmt) => {
                self.check_expr(&while_stmt.test, depth)?;
                self.check_stmt(&while_stmt.body, depth)?;
            }
            ast::Stmt::DoWhile(do_while) => {
                self.check_stmt(&do_while.body, depth)?;
                self.check_expr(&do_while.test, depth)?;
            }
            ast::Stmt::Switch(switch) => {
                self.check_expr(&switch.discriminant, depth)?;
                for case in &switch.cases {
                    if let Some(test) = &case.test {
                        self.check_expr(test, depth)?;
                    }
                    for stmt in &case.cons {
                        self.check_stmt(stmt, depth)?;
                    }
                }
            }
            ast::Stmt::Try(try_stmt) => {
                for stmt in &try_stmt.block.stmts {
                    self.check_stmt(stmt, depth)?;
                }
                if let Some(handler) = &try_stmt.handler {
                    for stmt in &handler.body.stmts {
                        self.check_stmt(stmt, depth)?;
                    }
                }
                if let Some(finalizer) = &try_stmt.finalizer {
                    for stmt in &finalizer.stmts {
                        self.check_stmt(stmt, depth)?;
                    }
                }
            }
            ast::Stmt::Decl(decl) => {
                self.check_decl(decl, depth)?;
            }
            ast::Stmt::Expr(_) => {
                // Expression depth not checked to avoid recursion issues
            }
            _ => {} // Other statements don't increase depth significantly
        }

        *depth -= 1;
        Ok(())
    }

    fn check_class(&self, class: &ast::Class, depth: &mut usize) -> Result<()> {
        *depth += 1;
        if *depth > self.max_depth {
            anyhow::bail!("AST nesting depth exceeded");
        }

        for member in &class.body {
            match member {
                ast::ClassMember::Method(method) => {
                    if let Some(body) = &method.function.body {
                        for stmt in &body.stmts {
                            self.check_stmt(stmt, depth)?;
                        }
                    }
                }
                ast::ClassMember::PrivateMethod(method) => {
                    if let Some(body) = &method.function.body {
                        for stmt in &body.stmts {
                            self.check_stmt(stmt, depth)?;
                        }
                    }
                }
                _ => {}
            }
        }

        *depth -= 1;
        Ok(())
    }

    fn check_function(&self, func: &ast::Function, depth: &mut usize) -> Result<()> {
        *depth += 1;

        if *depth > self.max_depth {
            anyhow::bail!("AST nesting depth exceeded");
        }

        if let Some(body) = &func.body {
            for stmt in &body.stmts {
                self.check_stmt(stmt, depth)?;
            }
        }

        *depth -= 1;

        Ok(())
    }

    fn check_expr(&self, expr: &ast::Expr, depth: &mut usize) -> Result<()> {
        *depth += 1;
        if *depth > self.max_depth {
            anyhow::bail!("AST nesting depth exceeded");
        }

        match expr {
            ast::Expr::Lit(_) => {}
            ast::Expr::Ident(_) => {}
            ast::Expr::Unary(unary) => self.check_expr(&unary.arg, depth)?,
            ast::Expr::Bin(bin) => {
                self.check_expr(&bin.left, depth)?;
                self.check_expr(&bin.right, depth)?;
            }
            ast::Expr::Cond(cond) => {
                self.check_expr(&cond.test, depth)?;
                self.check_expr(&cond.cons, depth)?;
                self.check_expr(&cond.alt, depth)?;
            }
            ast::Expr::Call(call) => {
                if let ast::Callee::Expr(callee_expr) = &call.callee {
                    self.check_expr(callee_expr, depth)?;
                }
                for arg in &call.args {
                    self.check_expr(&arg.expr, depth)?;
                }
            }
            ast::Expr::Member(member) => {
                self.check_expr(&member.obj, depth)?;
                if let ast::MemberProp::Computed(computed) = &member.prop {
                    self.check_expr(&computed.expr, depth)?;
                }
            }
            ast::Expr::Array(array) => {
                for elem_expr in array.elems.iter().flatten() {
                    self.check_expr(&elem_expr.expr, depth)?;
                }
            }
            ast::Expr::Object(_obj) => {
                // Skip deep checking of object properties for now
            }
            ast::Expr::Fn(func) => self.check_function(&func.function, depth)?,
            ast::Expr::Arrow(arrow) => {
                for _ in &arrow.params {
                    // Params are patterns, but for depth, perhaps check if they have expr
                }
                match &*arrow.body {
                    ast::BlockStmtOrExpr::BlockStmt(block) => {
                        for stmt in &block.stmts {
                            self.check_stmt(stmt, depth)?;
                        }
                    }
                    ast::BlockStmtOrExpr::Expr(e) => self.check_expr(e, depth)?,
                }
            }
            ast::Expr::Tpl(tpl) => {
                for elem in &tpl.exprs {
                    self.check_expr(elem, depth)?;
                }
            }
            ast::Expr::Paren(paren) => self.check_expr(&paren.expr, depth)?,
            ast::Expr::Assign(assign) => {
                self.check_expr(&assign.right, depth)?;
            }
            ast::Expr::Seq(seq) => {
                for expr in &seq.exprs {
                    self.check_expr(expr, depth)?;
                }
            }
            _ => {} // Other expressions don't increase depth significantly
        }

        *depth -= 1;
        Ok(())
    }
}

/// Maximum source file size in bytes (default: 10 MB)
/// Override with OATS_MAX_SOURCE_BYTES environment variable
static MAX_SOURCE_SIZE: AtomicUsize = AtomicUsize::new(10 * 1024 * 1024);

/// Maximum AST nesting depth to prevent stack overflow attacks
/// Override with OATS_MAX_AST_DEPTH environment variable
static MAX_AST_DEPTH: AtomicUsize = AtomicUsize::new(100);

/// Flag indicating limits have been initialized
static LIMITS_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Initialize parser resource limits from environment variables
fn init_parser_limits() {
    if LIMITS_INITIALIZED.load(Ordering::Relaxed) {
        return;
    }

    // Parse OATS_MAX_SOURCE_BYTES (default: 10 MB)
    if let Ok(val) = std::env::var("OATS_MAX_SOURCE_BYTES")
        && let Ok(limit) = val.parse::<usize>()
    {
        MAX_SOURCE_SIZE.store(limit, Ordering::Relaxed);
    }

    // Parse OATS_MAX_AST_DEPTH (default: 100)
    if let Ok(val) = std::env::var("OATS_MAX_AST_DEPTH")
        && let Ok(limit) = val.parse::<usize>()
    {
        MAX_AST_DEPTH.store(limit, Ordering::Relaxed);
    }

    LIMITS_INITIALIZED.store(true, Ordering::Relaxed);
}

pub struct ParsedModule {
    pub parsed: ParsedSource,
    // Original source text (no preprocessing)
    pub source: String,
    // Set of VarDecl span start positions (byte index) that contain the
    // `mut` token. This is computed at parse time by scanning the captured
    // tokens inside each VarDecl span so downstream passes (codegen) can
    // consult mutability information without string-searching the source.
    pub mut_var_decls: std::collections::HashSet<usize>,
}

/// Parse a TypeScript source string into a `ParsedModule` and run
/// lightweight project-specific checks.
///
/// The function performs the following additional checks beyond parsing:
/// - Enforces maximum source size limit to prevent resource exhaustion
/// - Enforces that certain statement kinds end with semicolons (conservative
///   style choice to simplify span handling).
/// - Rejects usage of the `var` keyword in favor of `let`/`const`.
///
/// # Arguments
/// * `source_code` - source text to parse
/// * `file_path` - optional path used to create a `file://` URL for diagnostics
///
/// # Security
/// Checks source size against MAX_SOURCE_SIZE before parsing to prevent DoS.
pub fn parse_oats_module(source_code: &str, file_path: Option<&str>) -> Result<ParsedModule> {
    parse_oats_module_with_options(source_code, file_path, false)
}

pub fn parse_oats_module_with_options(
    source_code: &str,
    file_path: Option<&str>,
    enforce_semicolons: bool,
) -> Result<ParsedModule> {
    // Initialize resource limits on first call
    init_parser_limits();

    // SECURITY: Check source size limit before parsing
    let max_size = MAX_SOURCE_SIZE.load(Ordering::Relaxed);
    if source_code.len() > max_size {
        anyhow::bail!(
            "Source file too large: {} bytes (limit: {} bytes). \
             Set OATS_MAX_SOURCE_BYTES to increase.",
            source_code.len(),
            max_size
        );
    }

    // Strip BOM (Byte Order Mark) if present - deno_ast requires this
    // UTF-8 BOM is 0xEF 0xBB 0xBF
    let source_without_bom = if let Some(stripped) = source_code.strip_prefix('\u{FEFF}') {
        stripped
    } else {
        source_code
    };

    // Pre-scan for `let mut` occurrences so we can support the `let mut`
    // surface syntax at the parser layer without extending the external
    // TS parser. We will replace the `mut` token with whitespace of the
    // same length in the parsed source so that byte offsets (spans)
    // remain stable and align with the original source. Record the
    // byte index of the `let` start so we can mark those VarDecls as
    // mutable later.
    let mut precomputed_mut_positions: std::collections::HashSet<usize> =
        std::collections::HashSet::new();
    // Use a simple regex-like scan for `let` followed by whitespace and
    // `mut` to avoid pulling in regex crate; a manual search is ok here.
    let mut parse_source_chars: Vec<u8> = source_without_bom.as_bytes().to_vec();
    let sbytes = source_without_bom.as_bytes();
    let mut i = 0usize;
    while i + 4 < sbytes.len() {
        // match `let` at i
        if sbytes[i] == b'l' && sbytes.get(i + 1) == Some(&b'e') && sbytes.get(i + 2) == Some(&b't')
        {
            // ensure word boundary after `let`
            let mut j = i + 3;
            // skip whitespace
            while j < sbytes.len()
                && (sbytes[j] == b' '
                    || sbytes[j] == b'\t'
                    || sbytes[j] == b'\r'
                    || sbytes[j] == b'\n')
            {
                j += 1;
            }
            // check for `mut` starting at j
            if j + 3 <= sbytes.len()
                && sbytes.get(j) == Some(&b'm')
                && sbytes.get(j + 1) == Some(&b'u')
                && sbytes.get(j + 2) == Some(&b't')
            {
                // ensure following char is whitespace
                let after = j + 3;
                if after == sbytes.len()
                    || sbytes.get(after).is_none_or(|b| {
                        *b == b' '
                            || *b == b'\t'
                            || *b == b'\r'
                            || *b == b'\n'
                            || *b == b';'
                            || *b == b')'
                    })
                {
                    // record position of `let` (start index)
                    precomputed_mut_positions.insert(i);
                    // replace 'mut' with spaces in parse_source_chars so spans align
                    parse_source_chars[j] = b' ';
                    parse_source_chars[j + 1] = b' ';
                    parse_source_chars[j + 2] = b' ';
                    // advance i past the matched region
                    i = after;
                    continue;
                }
            }
        }
        i += 1;
    }
    let source_for_parse = String::from_utf8(parse_source_chars)
        .map_err(|_| anyhow::anyhow!("Invalid UTF-8 sequence in source code"))?;

    let sti = SourceTextInfo::from_string(source_for_parse.clone());
    // Use the provided file_path (if any) to create a proper file:// URL so
    // diagnostics and span-based tooling can show accurate paths. Fall back
    // to a generic placeholder when no path is available.
    let specifier = if let Some(p) = file_path {
        // Try to canonicalize to an absolute path where possible; if that
        // fails, still attempt to produce a file:// URL from the provided
        // path string.
        match std::fs::canonicalize(p) {
            Ok(abs) => Url::from_file_path(abs)
                .map_err(|()| anyhow::anyhow!("failed to convert path to file URL: {}", p))?,
            Err(_) => match Url::from_file_path(p) {
                Ok(url) => url,
                Err(_) => Url::parse("file://file.ts")
                    .map_err(|e| anyhow::anyhow!("failed to parse fallback URL: {}", e))?,
            },
        }
    } else {
        Url::parse("file://file.ts")?
    };

    let params = ParseParams {
        specifier,
        text: sti.text().clone(),
        media_type: MediaType::TypeScript,
        // Capture tokens so we can validate semicolon presence if needed.
        capture_tokens: true,
        scope_analysis: false,
        maybe_syntax: None,
    };

    let parsed = parse_module(params)?;

    // SECURITY: Check AST nesting depth to prevent stack overflow attacks
    let max_depth = MAX_AST_DEPTH.load(Ordering::Relaxed);
    let depth_checker = AstDepthChecker { max_depth };
    if depth_checker.check_program(&parsed).is_err() {
        anyhow::bail!(
            "AST nesting depth exceeds limit: {} levels. \
             Set OATS_MAX_AST_DEPTH to increase.",
            max_depth
        );
    }

    if enforce_semicolons {
        // Enforce semicolons for statement-terminated constructs (conservative)
        // This behavior mirrors languages like Rust/C++ where semicolons are
        // required after expression statements, returns, and declarations.
        fn stmt_requires_semicolon(stmt: &ast::Stmt) -> bool {
            match stmt {
                ast::Stmt::Expr(_) => true,
                ast::Stmt::Decl(ast::Decl::Var(_)) => true,
                ast::Stmt::Decl(ast::Decl::TsEnum(_)) => true,
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
        fn has_trailing_semicolon(
            span: &deno_ast::swc::common::Span,
            source: &str,
            parsed: &ParsedSource,
        ) -> bool {
            let hi = span_hi_usize(span);
            let lo = span.lo.0 as usize;
            let bytes = source.as_bytes();
            let len = bytes.len();
            // If a semicolon appears anywhere inside the span, consider it present.
            if lo < len {
                let end = if hi > len { len } else { hi };
                if lo < end && bytes[lo..end].contains(&b';') {
                    return true;
                }
            }
            // quick checks: if semicolon is exactly at hi or immediately before
            if hi < len && bytes[hi] == b';' {
                return true;
            }
            let prev_idx = hi.saturating_sub(1);
            if hi > 0 && prev_idx < len && bytes[prev_idx] == b';' {
                return true;
            }
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
                if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'/' {
                    i += 2;
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                    continue;
                }
                // block comment /* ... */
                if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
                    i += 2;
                    while i + 1 < bytes.len() {
                        if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                            i += 2;
                            break;
                        }
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
                if enforce_semicolons && stmt_requires_semicolon(stmt) {
                    // Get stmt span and inspect source
                    let span = match stmt {
                        ast::Stmt::Expr(es) => es.span,
                        ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                        ast::Stmt::Decl(ast::Decl::TsEnum(e)) => e.span,
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
                            Some(
                                "oats requires semicolons like Rust/C++: add a trailing ';' to this statement.",
                            ),
                        );
                    }
                }
                // Reject usage of the legacy `var` keyword: require `let`.
                if let ast::Stmt::Decl(ast::Decl::Var(vdecl)) = stmt
                    && matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Var)
                {
                    let start = vdecl.span.lo.0 as usize;
                    return diagnostics::report_error_span_and_bail(
                        file_path,
                        source_code,
                        start,
                        "the `var` keyword is not allowed; use `let` instead",
                        Some(
                            "oats requires modern variable declarations: use `let` or `const` instead of `var`.",
                        ),
                    );
                }
                // (no-op) `const` and `let` are both accepted; `var` is rejected.
                // If it's a function decl, also inspect its body
                if let ast::Stmt::Decl(ast::Decl::Fn(fdecl)) = stmt
                    && let Some(body) = &fdecl.function.body
                {
                    for s in &body.stmts {
                        if stmt_requires_semicolon(s) {
                            let span = match s {
                                ast::Stmt::Expr(es) => es.span,
                                ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                                ast::Stmt::Decl(ast::Decl::TsEnum(e)) => e.span,
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
                                    Some(
                                        "oats requires semicolons like Rust/C++: add a trailing ';' to this statement.",
                                    ),
                                );
                            }
                            // Reject `var` inside function bodies as well
                            if let ast::Stmt::Decl(ast::Decl::Var(vdecl)) = s
                                && matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Var)
                            {
                                let start = vdecl.span.lo.0 as usize;
                                return diagnostics::report_error_span_and_bail(
                                    file_path,
                                    source_code,
                                    start,
                                    "the `var` keyword is not allowed; use `let` instead",
                                    Some(
                                        "oats requires modern variable declarations: use `let` or `const` instead of `var`.",
                                    ),
                                );
                            }
                            // (no-op) `const` and `let` are both accepted in function bodies.
                        }
                    }
                }
            }
            if let deno_ast::ModuleItemRef::ModuleDecl(
                deno_ast::swc::ast::ModuleDecl::ExportDecl(decl),
            ) = item
            {
                // export declarations that are vars should also be terminated
                if let ast::Decl::Var(vdecl) = &decl.decl {
                    // Reject var in export decls as well
                    if matches!(vdecl.kind, deno_ast::swc::ast::VarDeclKind::Var) {
                        let start = vdecl.span.lo.0 as usize;
                        return diagnostics::report_error_span_and_bail(
                            file_path,
                            source_code,
                            start,
                            "the `var` keyword is not allowed; use `let` instead",
                            Some(
                                "`var` is disallowed in this project; use `let` (immutable by default) or `let mut` for mutable bindings.",
                            ),
                        );
                    }
                    // (no-op) export declarations can be `let` or `const`; only `var` is rejected.
                    if !has_trailing_semicolon(&vdecl.span, source_code, &parsed) {
                        let start = vdecl.span.lo.0 as usize;
                        return diagnostics::report_error_span_and_bail(
                            file_path,
                            source_code,
                            start,
                            "missing semicolon after export declaration",
                            Some(
                                "oats requires semicolons like Rust/C++: add a trailing ';' to this export declaration.",
                            ),
                        );
                    }
                }
                if let ast::Decl::Fn(fdecl) = &decl.decl
                    && let Some(body) = &fdecl.function.body
                {
                    for s in &body.stmts {
                        if stmt_requires_semicolon(s) {
                            let span = match s {
                                ast::Stmt::Expr(es) => es.span,
                                ast::Stmt::Decl(ast::Decl::Var(v)) => v.span,
                                ast::Stmt::Decl(ast::Decl::TsEnum(e)) => e.span,
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
                                    Some(
                                        "oats requires semicolons like Rust/C++: add a trailing ';' to this statement.",
                                    ),
                                );
                            }
                        }
                    }
                }
            }
        }
    }
    // Compute token-aware `mut` detection for var declarations. We use
    // token spans (when available) to avoid matching `mut` inside string
    // literals or comments. The detected set contains the `span.lo` byte
    // index of VarDecl nodes that include a `mut` token.
    // Start with any precomputed positions found by scanning for `let mut`.
    let mut mut_var_decls: std::collections::HashSet<usize> = precomputed_mut_positions;

    // Collect all VarDecl spans from the AST (including nested function
    // bodies handled above). A simple traversal suffices for now.
    fn collect_var_decl_spans(stmt: &ast::Stmt, out: &mut Vec<deno_ast::swc::common::Span>) {
        match stmt {
            ast::Stmt::Decl(ast::Decl::Var(v)) => {
                out.push(v.span);
            }
            ast::Stmt::Block(b) => {
                for s in &b.stmts {
                    collect_var_decl_spans(s, out);
                }
            }
            ast::Stmt::If(i) => {
                // `cons` is a Box<Stmt>
                collect_var_decl_spans(&i.cons, out);
                if let Some(alt) = &i.alt {
                    collect_var_decl_spans(alt, out);
                }
            }
            ast::Stmt::For(f) => {
                if let Some(init) = &f.init
                    && let deno_ast::swc::ast::VarDeclOrExpr::VarDecl(v) = init
                {
                    out.push(v.span);
                }
                // body is Box<Stmt>
                collect_var_decl_spans(&f.body, out);
            }
            ast::Stmt::While(w) => {
                collect_var_decl_spans(&w.body, out);
            }
            ast::Stmt::DoWhile(d) => {
                collect_var_decl_spans(&d.body, out);
            }
            ast::Stmt::Return(_)
            | ast::Stmt::Expr(_)
            | ast::Stmt::Break(_)
            | ast::Stmt::Continue(_)
            | ast::Stmt::Throw(_)
            | ast::Stmt::Debugger(_) => {}
            _ => {}
        }
    }

    // Gather var decl spans from the program body and module-level decls
    let mut var_spans: Vec<deno_ast::swc::common::Span> = Vec::new();
    for item in parsed.program_ref().body() {
        if let deno_ast::ModuleItemRef::Stmt(stmt) = item {
            collect_var_decl_spans(stmt, &mut var_spans);
        } else if let deno_ast::ModuleItemRef::ModuleDecl(
            deno_ast::swc::ast::ModuleDecl::ExportDecl(decl),
        ) = item
        {
            // export decls may contain VarDecls
            if let ast::Decl::Var(v) = &decl.decl {
                var_spans.push(v.span);
            }
            if let ast::Decl::Fn(fdecl) = &decl.decl
                && let Some(body) = &fdecl.function.body
            {
                for s in &body.stmts {
                    collect_var_decl_spans(s, &mut var_spans);
                }
            }
        }
    }

    // If tokens were captured, inspect token spans inside each VarDecl span
    // for a token whose source text is exactly "mut". This avoids false
    // positives inside strings/comments.
    let tok_slice = parsed.tokens();
    if !tok_slice.is_empty() {
        for span in var_spans.iter() {
            let slo = span.lo.0 as usize;
            let shi = span.hi.0 as usize;
            for tok in tok_slice.iter() {
                let tlo = tok.span.lo.0 as usize;
                let thi = tok.span.hi.0 as usize;
                if tlo >= slo && thi <= shi {
                    // safe slice bounds check
                    if thi <= source_code.len() && tlo < thi {
                        // Find the byte indices at char boundaries
                        let start_byte = source_code
                            .char_indices()
                            .find(|(byte_idx, _)| *byte_idx >= tlo)
                            .map(|(byte_idx, _)| byte_idx)
                            .unwrap_or(tlo);
                        let end_byte = source_code
                            .char_indices()
                            .find(|(byte_idx, _)| *byte_idx >= thi)
                            .map(|(byte_idx, _)| byte_idx)
                            .unwrap_or(thi);
                        if start_byte < end_byte {
                            let s = &source_code[start_byte..end_byte];
                            if s == "mut" {
                                mut_var_decls.insert(slo);
                                break;
                            }
                        }
                    }
                }
            }
        }
    } else {
        // Fallback: if tokens aren't available, conservatively scan the
        // source substring for `mut` (existing behavior). This is less
        // robust but lets the language extension work when tokens aren't
        // captured.
        for span in var_spans.iter() {
            let slo = span.lo.0 as usize;
            let shi = span.hi.0 as usize;
            if shi > slo && shi <= source_code.len() {
                let slice = &source_code[slo..shi];
                if slice.contains("mut ") || slice.contains("mut\t") || slice.contains("mut\n") {
                    mut_var_decls.insert(slo);
                }
            }
        }
    }

    Ok(ParsedModule {
        parsed,
        source: source_code.to_string(),
        mut_var_decls,
    })
}
