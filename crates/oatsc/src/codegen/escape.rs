use oats_ast::*;
use std::collections::{HashMap, HashSet};

/// Escape analysis results for a function.
///
/// Tracks which variables escape the function scope, enabling RC optimization.
#[derive(Debug, Clone, Default)]
pub struct EscapeInfo {
    /// Variables that escape the function
    pub escapes: HashSet<String>,
    /// Variable definitions found in the function
    pub definitions: HashSet<String>,
    /// Variables used before definition (parameters or captured)
    pub uses_before_def: HashSet<String>,
    /// Variables live across await points
    pub await_live: HashSet<String>,
    /// Variables captured by closures
    pub captured: HashSet<String>,
    /// Function calls that may cause escapes
    pub escape_calls: HashSet<String>,
}

impl EscapeInfo {
    /// Creates a new, empty `EscapeInfo`.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns whether `name` escapes the function.
    pub fn escapes(&self, name: &str) -> bool {
        self.escapes.contains(name)
    }

    /// Marks `name` as escaping.
    pub fn mark_escaping(&mut self, name: String) {
        self.escapes.insert(name);
    }

    /// Records a variable definition.
    pub fn define_var(&mut self, name: String) {
        self.definitions.insert(name);
    }

    /// Records a variable use, marking as use-before-def if not yet defined.
    pub fn use_var(&mut self, name: &str) {
        if !self.definitions.contains(name) {
            self.uses_before_def.insert(name.to_string());
        }
    }

    /// Marks `name` as live across an await point.
    pub fn mark_await_live(&mut self, name: String) {
        self.await_live.insert(name);
    }

    /// Marks `name` as captured by a closure.
    pub fn mark_captured(&mut self, name: String) {
        self.captured.insert(name);
    }

    /// Marks `func_name` as potentially causing escapes.
    pub fn mark_escape_call(&mut self, func_name: String) {
        self.escape_calls.insert(func_name);
    }

    /// Returns whether `name` is captured by closures.
    pub fn is_captured(&self, name: &str) -> bool {
        self.captured.contains(name)
    }
}

impl crate::codegen::CodeGen<'_> {
    /// Analyzes a function to determine which variables escape its scope.
    ///
    /// Tracks definitions vs uses, analyzes control flow, and identifies
    /// variables live across await points for async functions.
    pub fn analyze_fn(&self, func: &Function) -> EscapeInfo {
        let mut analyzer = EscapeAnalyzer::new();

        // First pass: Collect parameter names as initial definitions.
        for param in &func.params {
            analyzer.collect_param_names(&param.pat);
        }

        // Second pass: Analyze the function body to find uses and other definitions.
        if let Some(body) = &func.body {
            for stmt in &body.stmts {
                analyzer.analyze_stmt(stmt);
            }
        }

        // Third pass: For async functions, find variables live across `await` points.
        if func.is_async {
            analyzer.analyze_async_await_live(func);
        }

        analyzer.info
    }
}

/// Internal state for the escape analysis process.
struct EscapeAnalyzer {
    info: EscapeInfo,
    scope_depth: usize,
    scope_vars: HashMap<usize, HashSet<String>>,
}

impl EscapeAnalyzer {
    fn new() -> Self {
        Self {
            info: EscapeInfo::new(),
            scope_depth: 0,
            scope_vars: HashMap::new(),
        }
    }

    /// Enters a new lexical scope (e.g., a block or loop body).
    fn enter_scope(&mut self) {
        self.scope_depth += 1;
        self.scope_vars.insert(self.scope_depth, HashSet::new());
    }

    /// Exits the current lexical scope.
    fn exit_scope(&mut self) {
        self.scope_vars.remove(&self.scope_depth);
        if self.scope_depth > 0 {
            self.scope_depth -= 1;
        }
    }

    /// Checks if a variable is defined in the current or any parent scope.
    fn is_defined(&self, name: &str) -> bool {
        self.info.definitions.contains(name)
            || self.scope_vars.values().any(|vars| vars.contains(name))
    }

    /// Recursively collects variable names from declaration patterns.
    fn collect_param_names(&mut self, pat: &Pat) {
        match pat {
            Pat::Ident(ident) => self.info.define_var(ident.sym.clone()),
            Pat::Array(arr) => {
                for elem_pat in arr.elems.iter().flatten() {
                    self.collect_param_names(elem_pat);
                }
            }
            Pat::Object(obj) => {
                for prop in &obj.props {
                    match prop {
                        ObjectPatProp::KeyValue { value, .. } => {
                            self.collect_param_names(value);
                        }
                        ObjectPatProp::Rest { arg, .. } => {
                            self.info.define_var(arg.sym.clone());
                        }
                    }
                }
            }
            Pat::Rest(rest) => {
                self.collect_param_names(&rest.arg);
            }
        }
    }

    /// Analyzes a statement for escape patterns.
    fn analyze_stmt(&mut self, stmt: &Stmt) {
        self.analyze_stmt_with_depth(stmt, 0)
    }

    /// Internal helper with recursion depth tracking to prevent stack overflow.
    fn analyze_stmt_with_depth(&mut self, stmt: &Stmt, depth: usize) {
        // Prevent stack overflow from deeply nested AST structures
        const MAX_ESCAPE_ANALYSIS_DEPTH: usize = 1000;
        if depth > MAX_ESCAPE_ANALYSIS_DEPTH {
            return;
        }

        match stmt {
            Stmt::ExprStmt(expr_stmt) => self.analyze_expr_with_depth(&expr_stmt.expr, depth + 1),
            Stmt::Return(ret) => {
                if let Some(arg) = &ret.arg {
                    self.analyze_expr_as_escaping_with_depth(arg, depth + 1);
                }
            }
            Stmt::VarDecl(vd) => self.analyze_var_decl_with_depth(vd, depth + 1),
            Stmt::FnDecl(fn_decl) => self.info.define_var(fn_decl.ident.sym.clone()),
            Stmt::If(if_stmt) => {
                self.analyze_expr_with_depth(&if_stmt.test, depth + 1);
                self.enter_scope();
                self.analyze_stmt_with_depth(&if_stmt.cons, depth + 1);
                self.exit_scope();
                if let Some(alt) = &if_stmt.alt {
                    self.enter_scope();
                    self.analyze_stmt_with_depth(alt, depth + 1);
                    self.exit_scope();
                }
            }
            Stmt::Block(block) => {
                self.enter_scope();
                for s in &block.stmts {
                    self.analyze_stmt_with_depth(s, depth + 1);
                }
                self.exit_scope();
            }
            Stmt::While(while_stmt) => {
                self.analyze_expr_with_depth(&while_stmt.test, depth + 1);
                self.enter_scope();
                self.analyze_stmt_with_depth(&while_stmt.body, depth + 1);
                self.exit_scope();
            }
            Stmt::For(for_stmt) => {
                self.enter_scope();
                if let Some(init) = &for_stmt.init {
                    match init {
                        ForInit::VarDecl(vd) => self.analyze_var_decl_with_depth(vd, depth + 1),
                        ForInit::Expr(e) => self.analyze_expr_with_depth(e, depth + 1),
                    }
                }
                if let Some(test) = &for_stmt.test {
                    self.analyze_expr_with_depth(test, depth + 1);
                }
                if let Some(update) = &for_stmt.update {
                    self.analyze_expr_with_depth(update, depth + 1);
                }
                self.analyze_stmt_with_depth(&for_stmt.body, depth + 1);
                self.exit_scope();
            }
            Stmt::ForIn(for_in) => {
                self.enter_scope();
                // Simplified: just analyze the expression parts for now
                self.analyze_expr_with_depth(&for_in.right, depth + 1);
                self.analyze_stmt_with_depth(&for_in.body, depth + 1);
                self.exit_scope();
            }
            Stmt::ForOf(for_of) => {
                self.enter_scope();
                // Simplified: just analyze the expression parts for now
                self.analyze_expr_with_depth(&for_of.right, depth + 1);
                self.analyze_stmt_with_depth(&for_of.body, depth + 1);
                self.exit_scope();
            }
            Stmt::Try(try_stmt) => {
                self.enter_scope();
                self.analyze_block_stmt_with_depth(&try_stmt.block, depth + 1);
                self.exit_scope();
                if let Some(handler) = &try_stmt.handler {
                    self.enter_scope();
                    if let Some(param) = &handler.param {
                        self.collect_param_names(param);
                    }
                    self.analyze_block_stmt_with_depth(&handler.body, depth + 1);
                    self.exit_scope();
                }
                if let Some(finalizer) = &try_stmt.finalizer {
                    self.enter_scope();
                    self.analyze_block_stmt_with_depth(finalizer, depth + 1);
                    self.exit_scope();
                }
            }
            _ => {}
        }
    }

    /// Analyzes a variable declaration list.
    fn analyze_var_decl(&mut self, var_decl: &VarDecl) {
        self.analyze_var_decl_with_depth(var_decl, 0)
    }

    fn analyze_var_decl_with_depth(&mut self, var_decl: &VarDecl, depth: usize) {
        for declarator in &var_decl.decls {
            self.collect_param_names(&declarator.name);
            if let Some(init) = &declarator.init {
                self.analyze_expr_with_depth(init, depth + 1);
            }
        }
    }

    /// Analyzes a block statement.
    fn analyze_block_stmt(&mut self, block: &BlockStmt) {
        self.analyze_block_stmt_with_depth(block, 0)
    }

    fn analyze_block_stmt_with_depth(&mut self, block: &BlockStmt, depth: usize) {
        for stmt in &block.stmts {
            self.analyze_stmt_with_depth(stmt, depth + 1);
        }
    }

    /// Analyzes an expression for variable uses and escape patterns.
    fn analyze_expr(&mut self, expr: &Expr) {
        self.analyze_expr_with_depth(expr, 0)
    }

    /// Internal helper with recursion depth tracking to prevent stack overflow.
    fn analyze_expr_with_depth(&mut self, expr: &Expr, depth: usize) {
        // Prevent stack overflow from deeply nested expressions
        const MAX_ESCAPE_ANALYSIS_DEPTH: usize = 1000;
        if depth > MAX_ESCAPE_ANALYSIS_DEPTH {
            return;
        }

        match expr {
            Expr::Ident(ident) => self.info.use_var(&ident.sym),
            Expr::Call(call) => {
                // Check if this is a call that might cause escapes
                if let Callee::Expr(callee_expr) = &call.callee {
                    if let Expr::Ident(ident) = &**callee_expr {
                        let func_name = ident.sym.clone();
                        // Mark certain functions as escape-causing
                        if matches!(
                            func_name.as_str(),
                            "console.log" | "setTimeout" | "setInterval"
                        ) {
                            self.info.mark_escape_call(func_name.clone());
                            // All arguments to escape calls are considered escaping
                            for arg in &call.args {
                                self.analyze_expr_as_escaping_with_depth(arg, depth + 1);
                            }
                            return;
                        }
                    }
                    self.analyze_expr_with_depth(callee_expr, depth + 1);
                }
                for arg in &call.args {
                    self.analyze_expr_as_escaping_with_depth(arg, depth + 1);
                }
            }
            Expr::Member(member) => {
                self.analyze_expr_with_depth(&member.obj, depth + 1);
                if let MemberProp::Computed(comp) = &member.prop {
                    self.analyze_expr_with_depth(comp, depth + 1);
                }
            }
            Expr::Assign(assign) => {
                // oats_ast::AssignTarget only supports Pat for now
                if let AssignTarget::Pat(Pat::Ident(ident)) = &assign.left {
                    let name = ident.sym.clone();
                    if !self.is_defined(&name) {
                        self.info.define_var(name);
                    }
                }
                self.analyze_expr_with_depth(&assign.right, depth + 1);
            }
            Expr::Bin(bin) => {
                self.analyze_expr_with_depth(&bin.left, depth + 1);
                self.analyze_expr_with_depth(&bin.right, depth + 1);
            }
            Expr::Unary(unary) => self.analyze_expr_with_depth(&unary.arg, depth + 1),
            Expr::Cond(cond) => {
                self.analyze_expr_with_depth(&cond.test, depth + 1);
                self.analyze_expr_with_depth(&cond.cons, depth + 1);
                self.analyze_expr_with_depth(&cond.alt, depth + 1);
            }
            Expr::Array(arr) => {
                for elem in arr.elems.iter().flatten() {
                    self.analyze_expr_with_depth(elem, depth + 1);
                }
            }
            Expr::Object(obj) => {
                for prop in &obj.props {
                    match prop {
                        PropOrSpread::Spread(spread) => {
                            self.analyze_expr_with_depth(&spread.expr, depth + 1)
                        }
                        PropOrSpread::Prop(Prop::KeyValue(kv)) => {
                            // PropName doesn't have Computed in oats_ast, skip for now
                            self.analyze_expr_with_depth(&kv.value, depth + 1);
                        }
                        PropOrSpread::Prop(Prop::Shorthand(_)) => {}
                    }
                }
            }
            Expr::New(new) => {
                self.analyze_expr_with_depth(&new.callee, depth + 1);
                for arg in &new.args {
                    self.analyze_expr_as_escaping_with_depth(arg, depth + 1);
                }
            }
            Expr::Await(await_expr) => self.analyze_expr_with_depth(&await_expr.arg, depth + 1),
            Expr::Paren(paren) => self.analyze_expr_with_depth(&paren.expr, depth + 1),
            Expr::Arrow(arrow) => {
                // Analyze arrow function body for captured variables
                let mut captured = HashSet::new();
                Self::collect_vars_from_expr(&Expr::Arrow(arrow.clone()), &mut captured);

                // Remove parameters from captured set (they're not captured)
                let mut params = HashSet::new();
                for param in &arrow.params {
                    Self::collect_vars_from_pat(&param.pat, &mut params);
                }

                for param in params {
                    captured.remove(&param);
                }

                // Mark remaining variables as captured
                for var in captured {
                    if self.is_defined(&var) {
                        self.info.mark_captured(var);
                    }
                }

                // Analyze the body
                match &arrow.body {
                    ArrowBody::Expr(e) => self.analyze_expr_with_depth(e, depth + 1),
                    ArrowBody::Block(block) => {
                        self.enter_scope();
                        self.analyze_block_stmt_with_depth(block, depth + 1);
                        self.exit_scope();
                    }
                }
            }
            _ => {}
        }
    }

    /// Analyzes an expression where all contained variables are considered escaping.
    fn analyze_expr_as_escaping(&mut self, expr: &Expr) {
        self.analyze_expr_as_escaping_with_depth(expr, 0)
    }

    fn analyze_expr_as_escaping_with_depth(&mut self, expr: &Expr, depth: usize) {
        let mut vars = HashSet::new();
        Self::collect_vars_from_expr_with_depth(expr, &mut vars, depth);
        for var in vars {
            self.info.mark_escaping(var);
        }
    }

    /// Recursively collects all variable names from an expression.
    fn collect_vars_from_expr(expr: &Expr, vars: &mut HashSet<String>) {
        Self::collect_vars_from_expr_with_depth(expr, vars, 0)
    }

    fn collect_vars_from_expr_with_depth(expr: &Expr, vars: &mut HashSet<String>, depth: usize) {
        // Prevent stack overflow from deeply nested expressions
        const MAX_VAR_COLLECTION_DEPTH: usize = 1000;
        if depth > MAX_VAR_COLLECTION_DEPTH {
            return;
        }

        match expr {
            Expr::Ident(ident) => {
                vars.insert(ident.sym.clone());
            }
            Expr::Call(call) => {
                if let Callee::Expr(callee_expr) = &call.callee {
                    Self::collect_vars_from_expr_with_depth(callee_expr, vars, depth + 1);
                }
                for arg in &call.args {
                    Self::collect_vars_from_expr_with_depth(arg, vars, depth + 1);
                }
            }
            Expr::Member(member) => {
                Self::collect_vars_from_expr_with_depth(&member.obj, vars, depth + 1);
                if let MemberProp::Computed(comp) = &member.prop {
                    Self::collect_vars_from_expr_with_depth(comp, vars, depth + 1);
                }
            }
            Expr::Bin(bin) => {
                Self::collect_vars_from_expr_with_depth(&bin.left, vars, depth + 1);
                Self::collect_vars_from_expr_with_depth(&bin.right, vars, depth + 1);
            }
            Expr::Unary(unary) => {
                Self::collect_vars_from_expr_with_depth(&unary.arg, vars, depth + 1)
            }
            Expr::Cond(cond) => {
                Self::collect_vars_from_expr_with_depth(&cond.test, vars, depth + 1);
                Self::collect_vars_from_expr_with_depth(&cond.cons, vars, depth + 1);
                Self::collect_vars_from_expr_with_depth(&cond.alt, vars, depth + 1);
            }
            Expr::Array(arr) => {
                for elem in arr.elems.iter().flatten() {
                    Self::collect_vars_from_expr_with_depth(elem, vars, depth + 1);
                }
            }
            Expr::Assign(assign) => {
                Self::collect_vars_from_expr_with_depth(&assign.right, vars, depth + 1)
            }
            Expr::Paren(paren) => {
                Self::collect_vars_from_expr_with_depth(&paren.expr, vars, depth + 1)
            }
            _ => {}
        }
    }

    /// Recursively collects all variable names from a pattern.
    fn collect_vars_from_pat(pat: &Pat, vars: &mut HashSet<String>) {
        match pat {
            Pat::Ident(ident) => {
                vars.insert(ident.sym.clone());
            }
            Pat::Array(arr) => {
                for elem_pat in arr.elems.iter().flatten() {
                    Self::collect_vars_from_pat(elem_pat, vars);
                }
            }
            Pat::Object(obj) => {
                for prop in &obj.props {
                    match prop {
                        ObjectPatProp::KeyValue { value, .. } => {
                            Self::collect_vars_from_pat(value, vars);
                        }
                        ObjectPatProp::Rest { arg, .. } => {
                            vars.insert(arg.sym.clone());
                        }
                    }
                }
            }
            Pat::Rest(rest) => {
                Self::collect_vars_from_pat(&rest.arg, vars);
            }
        }
    }

    /// Analyzes an async function to find variables live across `await` points.
    fn analyze_async_await_live(&mut self, func: &Function) {
        if let Some(body) = &func.body {
            let mut await_analyzer = AwaitLiveAnalyzer::new();
            await_analyzer.analyze_stmts(&body.stmts);

            for var in await_analyzer.await_live {
                self.info.mark_await_live(var.clone());
                self.info.mark_escaping(var);
            }
        }
    }
}

/// Helper analyzer for identifying variables live across `await` points.
struct AwaitLiveAnalyzer {
    await_live: HashSet<String>,
    current_vars: HashSet<String>,
}

impl AwaitLiveAnalyzer {
    fn new() -> Self {
        Self {
            await_live: HashSet::new(),
            current_vars: HashSet::new(),
        }
    }

    fn analyze_stmts(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.analyze_stmt(stmt);
        }
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::ExprStmt(expr_stmt) => self.analyze_expr(&expr_stmt.expr),
            Stmt::Block(block) => self.analyze_stmts(&block.stmts),
            Stmt::If(if_stmt) => {
                self.analyze_expr(&if_stmt.test);
                self.analyze_stmt(&if_stmt.cons);
                if let Some(alt) = &if_stmt.alt {
                    self.analyze_stmt(alt);
                }
            }
            _ => {}
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Ident(ident) => {
                self.current_vars.insert(ident.sym.clone());
            }
            Expr::Await(_) => {
                for var in &self.current_vars {
                    self.await_live.insert(var.clone());
                }
                self.current_vars.clear();
            }
            Expr::Call(call) => {
                if let Callee::Expr(callee_expr) = &call.callee {
                    self.analyze_expr(callee_expr);
                }
                for arg in &call.args {
                    self.analyze_expr(arg);
                }
            }
            Expr::Member(member) => {
                self.analyze_expr(&member.obj);
                if let MemberProp::Computed(comp) = &member.prop {
                    self.analyze_expr(comp);
                }
            }
            Expr::Bin(bin) => {
                self.analyze_expr(&bin.left);
                self.analyze_expr(&bin.right);
            }
            Expr::Unary(unary) => self.analyze_expr(&unary.arg),
            _ => {}
        }
    }
}
