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
        stmts: &[oats_ast::Stmt],
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
        stmt: &oats_ast::Stmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        use oats_ast::*;
        // A small statement lowering implementation that covers the test
        // cases: variable declarations with initializers, expression
        // statements, return statements and blocks. This is intentionally
        // minimal: more statements can be added into `stmt.rs` later.
        match stmt {
            Stmt::VarDecl(vd) => self.lower_decl_stmt(vd, function, param_map, locals_stack),
            Stmt::ExprStmt(expr_stmt) => {
                self.lower_expr_stmt(expr_stmt, function, param_map, locals_stack)
            }
            Stmt::Return(ret) => self.lower_return_stmt(ret, function, param_map, locals_stack),
            Stmt::Break(break_stmt) => self.lower_break_stmt(break_stmt, locals_stack),
            Stmt::Continue(continue_stmt) => self.lower_continue_stmt(continue_stmt, locals_stack),
            Stmt::Block(block) => self.lower_block_stmt(block, function, param_map, locals_stack),
            Stmt::If(ifstmt) => self.lower_if_stmt(ifstmt, function, param_map, locals_stack),
            Stmt::Labeled(labeled) => {
                self.lower_labeled_stmt(labeled, function, param_map, locals_stack)
            }
            Stmt::ForOf(forof) => self.lower_for_of_stmt(forof, function, param_map, locals_stack),
            Stmt::ForIn(for_in) => {
                self.lower_for_in_stmt(for_in, function, param_map, locals_stack)
            }
            Stmt::For(forstmt) => self.lower_for_stmt(forstmt, function, param_map, locals_stack),
            Stmt::While(while_stmt) => {
                self.lower_while_stmt(while_stmt, function, param_map, locals_stack)
            }
            Stmt::DoWhile(dowhile_stmt) => {
                self.lower_do_while_stmt(dowhile_stmt, function, param_map, locals_stack)
            }
            Stmt::Switch(switch_stmt) => {
                self.lower_switch_stmt(switch_stmt, function, param_map, locals_stack)
            }
            Stmt::Try(try_stmt) => self.lower_try_stmt(try_stmt, function, param_map, locals_stack),
            Stmt::Throw(throw_stmt) => {
                self.lower_throw_stmt(throw_stmt, function, param_map, locals_stack)
            }
            Stmt::FnDecl(_) | Stmt::ClassDecl(_) => {
                // Function and class declarations are handled at module level
                Ok(false)
            }
            Stmt::Debugger(_) => {
                // Debugger statement is a no-op in codegen
                // It's a debugging aid that doesn't generate any code
                Ok(false)
            }
            Stmt::Import(_)
            | Stmt::TypeAlias(_)
            | Stmt::InterfaceDecl(_)
            | Stmt::EnumDecl(_)
            | Stmt::NamespaceDecl(_)
            | Stmt::DeclareFn(_) => {
                // These are handled at module level, not in statement lowering
                Ok(false)
            }
            _ => Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "statement not supported",
            )),
        }
    }
}
