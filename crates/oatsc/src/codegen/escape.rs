use oxc_ast::ast::*;
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
        for param in &func.params.items {
            analyzer.collect_param_names(&param.pattern);
        }

        // Second pass: Analyze the function body to find uses and other definitions.
        if let Some(body) = &func.body {
            for stmt in &body.statements {
                analyzer.analyze_stmt(stmt);
            }
        }

        // Third pass: For async functions, find variables live across `await` points.
        if func.r#async {
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
    fn collect_param_names(&mut self, pat: &BindingPattern) {
        match &pat.kind {
            BindingPatternKind::BindingIdentifier(ident) => {
                self.info.define_var(ident.name.to_string());
            }
            BindingPatternKind::ArrayPattern(arr) => {
                for elem_pat in arr.elements.iter().flatten() {
                    if let Some(elem) = elem_pat {
                        self.collect_param_names(elem);
                    }
                }
            }
            BindingPatternKind::ObjectPattern(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPatternProperty::ObjectProperty(prop) => {
                            self.collect_param_names(&prop.value);
                        }
                        ObjectPatternProperty::RestElement(rest) => {
                            self.collect_param_names(&rest.argument);
                        }
                    }
                }
            }
            BindingPatternKind::AssignmentPattern(assign) => {
                self.collect_param_names(&assign.left);
            }
            _ => {}
        }
    }

    /// Analyzes a statement for escape patterns.
    fn analyze_stmt(&mut self, stmt: &Statement) {
        self.analyze_stmt_with_depth(stmt, 0)
    }

    /// Internal helper with recursion depth tracking to prevent stack overflow.
    fn analyze_stmt_with_depth(&mut self, stmt: &Statement, depth: usize) {
        // Prevent stack overflow from deeply nested AST structures
        const MAX_ESCAPE_ANALYSIS_DEPTH: usize = 1000;
        if depth > MAX_ESCAPE_ANALYSIS_DEPTH {
            return;
        }

        match stmt {
            Statement::ExpressionStatement(expr_stmt) => {
                self.analyze_expr_with_depth(&expr_stmt.expression, depth + 1)
            }
            Statement::ReturnStatement(ret) => {
                if let Some(arg) = &ret.argument {
                    self.analyze_expr_as_escaping_with_depth(arg, depth + 1);
                }
            }
            Statement::VariableDeclaration(vd) => self.analyze_var_decl_with_depth(vd, depth + 1),
            Statement::FunctionDeclaration(fn_decl) => {
                if let Some(id) = &fn_decl.id {
                    self.info.define_var(id.name.to_string());
                }
            }
            Statement::IfStatement(if_stmt) => {
                self.analyze_expr_with_depth(&if_stmt.test, depth + 1);
                self.enter_scope();
                self.analyze_stmt_with_depth(&if_stmt.consequent, depth + 1);
                self.exit_scope();
                if let Some(alt) = &if_stmt.alternate {
                    self.enter_scope();
                    self.analyze_stmt_with_depth(alt, depth + 1);
                    self.exit_scope();
                }
            }
            Statement::BlockStatement(block) => {
                self.enter_scope();
                for s in &block.body {
                    self.analyze_stmt_with_depth(s, depth + 1);
                }
                self.exit_scope();
            }
            Statement::WhileStatement(while_stmt) => {
                self.analyze_expr_with_depth(&while_stmt.test, depth + 1);
                self.enter_scope();
                self.analyze_stmt_with_depth(&while_stmt.body, depth + 1);
                self.exit_scope();
            }
            Statement::ForStatement(for_stmt) => {
                self.enter_scope();
                if let Some(init) = &for_stmt.init {
                    match init {
                        ForStatementInit::VariableDeclaration(vd) => {
                            self.analyze_var_decl_with_depth(vd, depth + 1)
                        }
                        ForStatementInit::Expression(e) => {
                            self.analyze_expr_with_depth(e, depth + 1)
                        }
                        ForStatementInit::ForUsingDeclaration(_) => {}
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
            Statement::ForInStatement(for_in) => {
                self.enter_scope();
                // Simplified: just analyze the expression parts for now
                self.analyze_expr_with_depth(&for_in.right, depth + 1);
                self.analyze_stmt_with_depth(&for_in.body, depth + 1);
                self.exit_scope();
            }
            Statement::ForOfStatement(for_of) => {
                self.enter_scope();
                // Simplified: just analyze the expression parts for now
                self.analyze_expr_with_depth(&for_of.right, depth + 1);
                self.analyze_stmt_with_depth(&for_of.body, depth + 1);
                self.exit_scope();
            }
            Statement::TryStatement(try_stmt) => {
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

    fn analyze_var_decl_with_depth(&mut self, var_decl: &VariableDeclaration, depth: usize) {
        for declarator in &var_decl.declarations {
            self.collect_param_names(&declarator.id);
            if let Some(init) = &declarator.init {
                self.analyze_expr_with_depth(init, depth + 1);
            }
        }
    }

    fn analyze_block_stmt_with_depth(&mut self, block: &BlockStatement, depth: usize) {
        for stmt in &block.body {
            self.analyze_stmt_with_depth(stmt, depth + 1);
        }
    }

    /// Internal helper with recursion depth tracking to prevent stack overflow.
    fn analyze_expr_with_depth(&mut self, expr: &Expression, depth: usize) {
        // Prevent stack overflow from deeply nested expressions
        const MAX_ESCAPE_ANALYSIS_DEPTH: usize = 1000;
        if depth > MAX_ESCAPE_ANALYSIS_DEPTH {
            return;
        }

        match expr {
            Expression::Identifier(ident) => {
                self.info.use_var(&ident.name.to_string());
            }
            Expression::CallExpression(call) => {
                // Check if this is a call that might cause escapes
                match &call.callee {
                    Callee::Expression(callee_expr) => {
                        if let Expression::Identifier(ident) = &**callee_expr {
                            let func_name = ident.name.to_string();
                            // Mark certain functions as escape-causing
                            if matches!(
                                func_name.as_str(),
                                "console.log" | "setTimeout" | "setInterval"
                            ) {
                                self.info.mark_escape_call(func_name.clone());
                                // All arguments to escape calls are considered escaping
                                for arg in &call.arguments {
                                    self.analyze_expr_as_escaping_with_depth(&arg, depth + 1);
                                }
                                return;
                            }
                        }
                        self.analyze_expr_with_depth(callee_expr, depth + 1);
                    }
                    _ => {}
                }
                for arg in &call.arguments {
                    self.analyze_expr_as_escaping_with_depth(arg, depth + 1);
                }
            }
            Expression::MemberExpression(member) => {
                self.analyze_expr_with_depth(&member.object, depth + 1);
                if let MemberProperty::ComputedPropertyName(comp) = &member.property {
                    self.analyze_expr_with_depth(&comp.expression, depth + 1);
                }
            }
            Expression::AssignmentExpression(assign) => {
                match &assign.left {
                    AssignmentTarget::SimpleAssignmentTarget(simple) => {
                        if let SimpleAssignmentTarget::IdentifierReference(ident) = simple {
                            let name = ident.name.to_string();
                            if !self.is_defined(&name) {
                                self.info.define_var(name);
                            }
                        }
                    }
                    AssignmentTarget::AssignmentTargetPattern(_) => {}
                }
                self.analyze_expr_with_depth(&assign.right, depth + 1);
            }
            Expression::BinaryExpression(bin) => {
                self.analyze_expr_with_depth(&bin.left, depth + 1);
                self.analyze_expr_with_depth(&bin.right, depth + 1);
            }
            Expression::UnaryExpression(unary) => {
                self.analyze_expr_with_depth(&unary.argument, depth + 1)
            }
            Expression::ConditionalExpression(cond) => {
                self.analyze_expr_with_depth(&cond.test, depth + 1);
                self.analyze_expr_with_depth(&cond.consequent, depth + 1);
                self.analyze_expr_with_depth(&cond.alternate, depth + 1);
            }
            Expression::ArrayExpression(arr) => {
                for elem in arr.elements.iter().flatten() {
                    if let Some(elem) = elem {
                        self.analyze_expr_with_depth(elem, depth + 1);
                    }
                }
            }
            Expression::ObjectExpression(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPropertyKind::SpreadProperty(spread) => {
                            self.analyze_expr_with_depth(&spread.argument, depth + 1)
                        }
                        ObjectPropertyKind::ObjectProperty(prop) => {
                            self.analyze_expr_with_depth(&prop.value, depth + 1);
                        }
                    }
                }
            }
            Expression::NewExpression(new) => {
                self.analyze_expr_with_depth(&new.callee, depth + 1);
                for arg in &new.arguments {
                    self.analyze_expr_as_escaping_with_depth(arg, depth + 1);
                }
            }
            Expression::AwaitExpression(await_expr) => {
                self.analyze_expr_with_depth(&await_expr.argument, depth + 1)
            }
            Expression::ParenthesizedExpression(paren) => {
                self.analyze_expr_with_depth(&paren.expression, depth + 1)
            }
            Expression::ArrowFunctionExpression(arrow) => {
                // Analyze arrow function body for captured variables
                let mut captured = HashSet::new();
                Self::collect_vars_from_expr(expr, &mut captured);

                // Remove parameters from captured set (they're not captured)
                let mut params = HashSet::new();
                for param in &arrow.params.items {
                    Self::collect_vars_from_pat(&param.pattern, &mut params);
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
                if arrow.expression {
                    if let Some(expr) = &arrow.body.expression {
                        self.analyze_expr_with_depth(expr, depth + 1);
                    }
                } else {
                    self.enter_scope();
                    self.analyze_block_stmt_with_depth(&arrow.body, depth + 1);
                    self.exit_scope();
                }
            }
            _ => {}
        }
    }

    fn analyze_expr_as_escaping_with_depth(&mut self, expr: &Expression, depth: usize) {
        let mut vars = HashSet::new();
        Self::collect_vars_from_expr_with_depth(expr, &mut vars, depth);
        for var in vars {
            self.info.mark_escaping(var);
        }
    }

    /// Recursively collects all variable names from an expression.
    fn collect_vars_from_expr(expr: &Expression, vars: &mut HashSet<String>) {
        Self::collect_vars_from_expr_with_depth(expr, vars, 0)
    }

    fn collect_vars_from_expr_with_depth(
        expr: &Expression,
        vars: &mut HashSet<String>,
        depth: usize,
    ) {
        // Prevent stack overflow from deeply nested expressions
        const MAX_VAR_COLLECTION_DEPTH: usize = 1000;
        if depth > MAX_VAR_COLLECTION_DEPTH {
            return;
        }

        match expr {
            Expression::Identifier(ident) => {
                vars.insert(ident.name.to_string());
            }
            Expression::CallExpression(call) => {
                if let Callee::Expression(callee_expr) = &call.callee {
                    Self::collect_vars_from_expr_with_depth(callee_expr, vars, depth + 1);
                }
                for arg in &call.arguments {
                    Self::collect_vars_from_expr_with_depth(&arg, vars, depth + 1);
                }
            }
            Expression::MemberExpression(member) => {
                Self::collect_vars_from_expr_with_depth(&member.object, vars, depth + 1);
                if let MemberProperty::ComputedPropertyName(comp) = &member.property {
                    Self::collect_vars_from_expr_with_depth(&comp.expression, vars, depth + 1);
                }
            }
            Expression::BinaryExpression(bin) => {
                Self::collect_vars_from_expr_with_depth(&bin.left, vars, depth + 1);
                Self::collect_vars_from_expr_with_depth(&bin.right, vars, depth + 1);
            }
            Expression::UnaryExpression(unary) => {
                Self::collect_vars_from_expr_with_depth(&unary.argument, vars, depth + 1)
            }
            Expression::ConditionalExpression(cond) => {
                Self::collect_vars_from_expr_with_depth(&cond.test, vars, depth + 1);
                Self::collect_vars_from_expr_with_depth(&cond.consequent, vars, depth + 1);
                Self::collect_vars_from_expr_with_depth(&cond.alternate, vars, depth + 1);
            }
            Expression::ArrayExpression(arr) => {
                for elem in arr.elements.iter().flatten() {
                    if let Some(elem) = elem {
                        Self::collect_vars_from_expr_with_depth(elem, vars, depth + 1);
                    }
                }
            }
            Expression::AssignmentExpression(assign) => {
                Self::collect_vars_from_expr_with_depth(&assign.right, vars, depth + 1)
            }
            Expression::ParenthesizedExpression(paren) => {
                Self::collect_vars_from_expr_with_depth(&paren.expression, vars, depth + 1)
            }
            _ => {}
        }
    }

    /// Recursively collects all variable names from a pattern.
    fn collect_vars_from_pat(pat: &BindingPattern, vars: &mut HashSet<String>) {
        match &pat.kind {
            BindingPatternKind::BindingIdentifier(ident) => {
                vars.insert(ident.name.to_string());
            }
            BindingPatternKind::ArrayPattern(arr) => {
                for elem_pat in arr.elements.iter().flatten() {
                    if let Some(elem) = elem_pat {
                        Self::collect_vars_from_pat(elem, vars);
                    }
                }
            }
            BindingPatternKind::ObjectPattern(obj) => {
                for prop in &obj.properties {
                    match prop {
                        ObjectPatternProperty::ObjectProperty(prop) => {
                            Self::collect_vars_from_pat(&prop.value, vars);
                        }
                        ObjectPatternProperty::RestElement(rest) => {
                            Self::collect_vars_from_pat(&rest.argument, vars);
                        }
                    }
                }
            }
            BindingPatternKind::AssignmentPattern(assign) => {
                Self::collect_vars_from_pat(&assign.left, vars);
            }
            _ => {}
        }
    }

    /// Analyzes an async function to find variables live across `await` points.
    fn analyze_async_await_live(&mut self, func: &Function) {
        if let Some(body) = &func.body {
            let mut await_analyzer = AwaitLiveAnalyzer::new();
            await_analyzer.analyze_stmts(&body.statements);

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

    fn analyze_stmts(&mut self, stmts: &[Statement]) {
        for stmt in stmts {
            self.analyze_stmt(stmt);
        }
    }

    fn analyze_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::ExpressionStatement(expr_stmt) => self.analyze_expr(&expr_stmt.expression),
            Statement::BlockStatement(block) => self.analyze_stmts(&block.body),
            Statement::IfStatement(if_stmt) => {
                self.analyze_expr(&if_stmt.test);
                self.analyze_stmt(if_stmt.consequent.as_ref());
                if let Some(alt) = &if_stmt.alternate {
                    self.analyze_stmt(alt.as_ref());
                }
            }
            _ => {}
        }
    }

    fn analyze_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Identifier(ident) => {
                self.current_vars.insert(ident.name.to_string());
            }
            Expression::AwaitExpression(_) => {
                for var in &self.current_vars {
                    self.await_live.insert(var.clone());
                }
                self.current_vars.clear();
            }
            Expression::CallExpression(call) => {
                if let Callee::Expression(callee_expr) = &call.callee {
                    self.analyze_expr(callee_expr);
                }
                for arg in &call.arguments {
                    self.analyze_expr(&arg);
                }
            }
            Expression::MemberExpression(member) => {
                self.analyze_expr(&member.object);
                if let MemberProperty::ComputedPropertyName(comp) = &member.property {
                    self.analyze_expr(&comp.expression);
                }
            }
            Expression::BinaryExpression(bin) => {
                self.analyze_expr(&bin.left);
                self.analyze_expr(&bin.right);
            }
            Expression::UnaryExpression(unary) => self.analyze_expr(&unary.argument),
            _ => {}
        }
    }
}
