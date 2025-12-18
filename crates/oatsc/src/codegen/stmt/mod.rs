//! Statement lowering helpers
//!
//! This module contains the lowering logic for AST statement nodes into
//! LLVM IR using the `CodeGen` helpers. It implements a minimal but
//! well-documented subset of statement lowering used by the test-suite and
//! examples: variable declarations, expression statements, control-flow
//! constructs (if/for/while/do-while/for-of), returns and block scoping.
//!
//! The file documents non-obvious decisions such as union boxing and
//! reference-counting management so future contributors understand the
//! rationale behind emitted IR.

pub mod block;
pub mod break_continue;
pub mod control_flow;
pub mod decl;
pub mod expr_stmt;
pub mod for_in;
pub mod for_of;
pub mod labeled;
pub mod return_stmt;
pub mod switch;
pub mod try_catch;

use inkwell::values::FunctionValue;
use std::collections::HashMap;

#[allow(dead_code)]
type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);
// A stack of per-scope local maps.
//
// Each entry in the vector is a HashMap representing a lexical scope's
// locals. The maps store `LocalEntry` tuples describing the alloca pointer,
// the ABI type of the slot, initialization flag, const flag, whether the
// local is a Weak<T> (affects RC semantics), an optional nominal type
// name used to guide member access lowering, and an optional OatsType
// for union tracking.
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Lower a sequence of statements into `function`.
    ///
    /// Returns `Ok(true)` if lowering emitted a terminator instruction
    /// (for example `return` or an unconditional branch that ends the
    /// current block). This signals callers to stop emitting fall-through
    /// code for the current basic block.
    pub(crate) fn lower_stmts(
        &self,
        stmts: &[oxc_ast::ast::Statement<'_>],
        function: inkwell::values::FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        for stmt in stmts {
            if self.lower_stmt(stmt, function, param_map, locals_stack)? {
                return Ok(true); // Terminator found, stop processing.
            }
        }
        Ok(false)
    }

    pub(crate) fn lower_stmt(
        &self,
        stmt: &oxc_ast::ast::Statement<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // A small statement lowering implementation that covers the test
        // cases: variable declarations with initializers, expression
        // statements, return statements and blocks. This is intentionally
        // minimal: more statements can be added into `stmt.rs` later.
        match stmt {
            oxc_ast::ast::Statement::VariableDeclaration(vd) => {
                self.lower_decl_stmt(vd, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::ExpressionStatement(expr_stmt) => {
                self.lower_expr_stmt(expr_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::SwitchStatement(switch_stmt) => {
                self.lower_switch_stmt(switch_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::TryStatement(try_stmt) => {
                self.lower_try_stmt(try_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::ThrowStatement(throw_stmt) => {
                self.lower_throw_stmt(throw_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::BlockStatement(block) => {
                self.lower_block_stmt(block, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::ReturnStatement(ret_stmt) => {
                self.lower_return_stmt(ret_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::IfStatement(if_stmt) => {
                self.lower_if_stmt(if_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::WhileStatement(while_stmt) => {
                self.lower_while_stmt(while_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::DoWhileStatement(do_while) => {
                self.lower_do_while_stmt(do_while, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::ForStatement(for_stmt) => {
                self.lower_for_stmt(for_stmt, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::ForInStatement(for_in) => {
                self.lower_for_in_stmt(for_in, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::ForOfStatement(for_of) => {
                self.lower_for_of_stmt(for_of, function, param_map, locals_stack)
            }
            oxc_ast::ast::Statement::BreakStatement(break_stmt) => {
                self.lower_break_stmt(break_stmt, locals_stack)
            }
            oxc_ast::ast::Statement::ContinueStatement(continue_stmt) => {
                self.lower_continue_stmt(continue_stmt, locals_stack)
            }
            oxc_ast::ast::Statement::LabeledStatement(labeled) => {
                self.lower_labeled_stmt(labeled, function, param_map, locals_stack)
            }
            _ => {
                // These are handled at module level, not in statement lowering
                Ok(false)
            }
        }
    }
}
