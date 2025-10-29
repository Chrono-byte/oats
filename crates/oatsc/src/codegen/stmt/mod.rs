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
pub mod for_of;
pub mod labeled;
pub mod return_stmt;

use crate::diagnostics::Severity;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

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
        stmts: &[deno_ast::swc::ast::Stmt],
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
        stmt: &deno_ast::swc::ast::Stmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // A small statement lowering implementation that covers the test
        // cases: variable declarations with initializers, expression
        // statements, return statements and blocks. This is intentionally
        // minimal: more statements can be added into `stmt.rs` later.
        match stmt {
            deno_ast::swc::ast::Stmt::Decl(d) => {
                self.lower_decl_stmt(d, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::Expr(expr_stmt) => {
                self.lower_expr_stmt(expr_stmt, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::Return(ret) => {
                self.lower_return_stmt(ret, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::Break(break_stmt) => {
                self.lower_break_stmt(break_stmt, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::Continue(continue_stmt) => {
                self.lower_continue_stmt(continue_stmt, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::Block(block) => {
                self.lower_block_stmt(block, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::If(ifstmt) => {
                self.lower_if_stmt(ifstmt, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::Labeled(labeled) => {
                self.lower_labeled_stmt(labeled, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::ForOf(forof) => {
                self.lower_for_of_stmt(forof, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::For(forstmt) => {
                self.lower_for_stmt(forstmt, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::While(while_stmt) => {
                self.lower_while_stmt(while_stmt, function, param_map, locals_stack)
            }
            deno_ast::swc::ast::Stmt::DoWhile(dowhile_stmt) => {
                self.lower_do_while_stmt(dowhile_stmt, function, param_map, locals_stack)
            }
            _ => Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "statement not supported",
            )),
        }
    }
}
