use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::types::OatsType;
use oxc_ast::ast::{
    DoWhileStatement, ForStatement, ForStatementInit, IfStatement, Statement, WhileStatement,
};

type LocalEntry<'a> = (
    inkwell::values::PointerValue<'a>,
    inkwell::types::BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<OatsType>,
);
// A stack of per-scope local maps.
//
// Each entry in the vector is a HashMap representing a lexical scope's
// locals. The maps store `LocalEntry` tuples describing the alloca pointer,
// the ABI type of the slot, initialization flag, const flag, whether the
// local is a Weak<T> (affects RC semantics), an optional nominal type
// name used to guide member access lowering, and an optional OatsType
// for union tracking.

impl<'a> crate::codegen::CodeGen<'a> {
    pub(crate) fn lower_if_stmt(
        &self,
        ifstmt: &IfStatement<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Lower condition - errors must be propagated
        let cond_val = self.lower_expr(&ifstmt.test, function, param_map, locals_stack)?;
        if let Some(cond_bool) = self.to_condition_i1(cond_val) {
            let then_bb = self.context.append_basic_block(function, "if.then");
            let else_bb = self.context.append_basic_block(function, "if.else");
            let merge_bb = self.context.append_basic_block(function, "if.merge");

            // Conditional branch
            let _ = self
                .builder
                .build_conditional_branch(cond_bool, then_bb, else_bb);

            // Build then block
            self.builder.position_at_end(then_bb);
            let then_terminated: bool = match &ifstmt.consequent {
                oxc_ast::ast::Statement::BlockStatement(block) => {
                    self.lower_stmts(&block.body, function, param_map, locals_stack)?
                }
                _ => self.lower_stmt(&ifstmt.consequent, function, param_map, locals_stack)?,
            };
            if !then_terminated {
                let _ = self.builder.build_unconditional_branch(merge_bb);
            }

            // Build else block
            self.builder.position_at_end(else_bb);
            let else_terminated: bool = match &ifstmt.alternate {
                Some(alt) => self.lower_stmt(alt, function, param_map, locals_stack)?,
                None => false,
            };
            if !else_terminated {
                let _ = self.builder.build_unconditional_branch(merge_bb);
            }

            // Position at merge
            self.builder.position_at_end(merge_bb);

            // If both branches terminated, the merge block is unreachable.
            // Add an unreachable instruction to make LLVM happy.
            if then_terminated && else_terminated {
                let _ = self.builder.build_unreachable();
            }

            // If both branches terminated, the if statement terminates
            Ok(then_terminated && else_terminated)
        } else {
            Err(crate::diagnostics::Diagnostic::simple_boxed(
                crate::diagnostics::Severity::Error,
                "if condition must be coercible to boolean",
            ))
        }
    }

    pub(crate) fn lower_for_stmt(
        &self,
        forstmt: &ForStatement<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Push new scope for the loop (init vars live in this scope)
        locals_stack.push(HashMap::new());

        // Lower init (can be VarDecl or Expr)
        if let Some(init) = &forstmt.init {
            match init {
                ForStatementInit::VariableDeclaration(var_decl) => {
                    self.lower_decl_stmt(var_decl, function, param_map, locals_stack)?;
                }
                _ => {
                    // ForStatementInit can be various expression types
                    // Use as_expression() to get the expression if available
                    if let Some(expr) = init.as_expression() {
                        self.lower_expr(expr, function, param_map, locals_stack)?;
                    }
                }
            }
        }

        // Create basic blocks
        let loop_cond_bb = self.context.append_basic_block(function, "for.cond");
        let loop_body_bb = self.context.append_basic_block(function, "for.body");
        let loop_incr_bb = self.context.append_basic_block(function, "for.incr");
        let loop_after_bb = self.context.append_basic_block(function, "for.after");

        // Push loop context (continue jumps to increment, break jumps to after)
        self.loop_context_stack
            .borrow_mut()
            .push(crate::codegen::LoopContext {
                continue_block: loop_incr_bb,
                break_block: loop_after_bb,
                locals_start: locals_stack.len(),
                label: self.current_label.borrow().clone(),
            });

        // Branch to condition
        let _ = self.builder.build_unconditional_branch(loop_cond_bb);

        // Build condition block
        self.builder.position_at_end(loop_cond_bb);
        if let Some(test) = &forstmt.test {
            // Lower test expression - errors must be propagated
            let cond_val = self.lower_expr(test, function, param_map, locals_stack)?;
            // Coerce to i1 boolean
            if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                let _ =
                    self.builder
                        .build_conditional_branch(cond_bool, loop_body_bb, loop_after_bb);
            } else {
                // Failed to coerce, propagate error
                locals_stack.pop();
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    crate::diagnostics::Severity::Error,
                    "for loop condition must be coercible to boolean",
                ));
            }
        } else {
            // No test means infinite loop: always branch to body
            let _ = self.builder.build_unconditional_branch(loop_body_bb);
        }

        // Build body block
        self.builder.position_at_end(loop_body_bb);
        let body_terminated: bool = match &forstmt.body {
            Statement::BlockStatement(block) => {
                self.lower_stmts(&block.body, function, param_map, locals_stack)?
            }
            _ => self.lower_stmt(&forstmt.body, function, param_map, locals_stack)?,
        };

        // If body didn't terminate, branch to increment
        if !body_terminated {
            let _ = self.builder.build_unconditional_branch(loop_incr_bb);
        }

        // Build increment block
        self.builder.position_at_end(loop_incr_bb);
        if let Some(update) = &forstmt.update {
            self.lower_expr(update, function, param_map, locals_stack)?;
        }
        let _ = self.builder.build_unconditional_branch(loop_cond_bb);

        // Position after loop
        self.builder.position_at_end(loop_after_bb);

        // Pop loop context
        self.loop_context_stack.borrow_mut().pop();

        // Pop loop scope
        locals_stack.pop();

        Ok(false) // loops don't terminate unless body always returns
    }

    pub(crate) fn lower_while_stmt(
        &self,
        while_stmt: &WhileStatement<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Create basic blocks
        let loop_cond_bb = self.context.append_basic_block(function, "while.cond");
        let loop_body_bb = self.context.append_basic_block(function, "while.body");
        let loop_after_bb = self.context.append_basic_block(function, "while.after");

        // Push loop context (continue jumps to condition, break jumps to after)
        self.loop_context_stack
            .borrow_mut()
            .push(crate::codegen::LoopContext {
                continue_block: loop_cond_bb,
                break_block: loop_after_bb,
                locals_start: locals_stack.len(),
                label: self.current_label.borrow().clone(),
            });

        // Branch to condition
        let _ = self.builder.build_unconditional_branch(loop_cond_bb);

        // Build condition block
        self.builder.position_at_end(loop_cond_bb);
        // Lower condition expression - errors must be propagated
        let cond_val = self.lower_expr(&while_stmt.test, function, param_map, locals_stack)?;
        // Coerce to i1 boolean
        if let Some(cond_bool) = self.to_condition_i1(cond_val) {
            let _ = self
                .builder
                .build_conditional_branch(cond_bool, loop_body_bb, loop_after_bb);
        } else {
            self.loop_context_stack.borrow_mut().pop();
            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                crate::diagnostics::Severity::Error,
                "while loop condition must be coercible to boolean",
            ));
        }

        // Build body block
        self.builder.position_at_end(loop_body_bb);
        let body_terminated: bool = match &while_stmt.body {
            Statement::BlockStatement(block) => {
                self.lower_stmts(&block.body, function, param_map, locals_stack)?
            }
            _ => self.lower_stmt(&while_stmt.body, function, param_map, locals_stack)?,
        };

        // If body didn't terminate, branch back to condition
        if !body_terminated {
            let _ = self.builder.build_unconditional_branch(loop_cond_bb);
        }

        // Pop loop context
        self.loop_context_stack.borrow_mut().pop();

        // Position after loop
        self.builder.position_at_end(loop_after_bb);

        Ok(false) // while loops don't terminate unless body always returns
    }

    pub(crate) fn lower_do_while_stmt(
        &self,
        dowhile_stmt: &DoWhileStatement<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Create basic blocks
        let loop_body_bb = self.context.append_basic_block(function, "dowhile.body");
        let loop_cond_bb = self.context.append_basic_block(function, "dowhile.cond");
        let loop_after_bb = self.context.append_basic_block(function, "dowhile.after");

        // Push loop context (continue jumps to condition, break jumps to after)
        self.loop_context_stack
            .borrow_mut()
            .push(crate::codegen::LoopContext {
                continue_block: loop_cond_bb,
                break_block: loop_after_bb,
                locals_start: locals_stack.len(),
                label: self.current_label.borrow().clone(),
            });

        // Branch directly to body (execute at least once)
        let _ = self.builder.build_unconditional_branch(loop_body_bb);

        // Build body block
        self.builder.position_at_end(loop_body_bb);
        let body_terminated: bool = match &dowhile_stmt.body {
            Statement::BlockStatement(block) => {
                self.lower_stmts(&block.body, function, param_map, locals_stack)?
            }
            _ => self.lower_stmt(&dowhile_stmt.body, function, param_map, locals_stack)?,
        };

        // If body didn't terminate, branch to condition
        if !body_terminated {
            let _ = self.builder.build_unconditional_branch(loop_cond_bb);
        }

        // Build condition block (executed after body)
        self.builder.position_at_end(loop_cond_bb);
        // Lower condition expression - errors must be propagated
        let cond_val = self.lower_expr(&dowhile_stmt.test, function, param_map, locals_stack)?;
        // Coerce to i1 boolean
        if let Some(cond_bool) = self.to_condition_i1(cond_val) {
            let _ = self.builder.build_conditional_branch(
                cond_bool,
                loop_body_bb, // Loop back to body
                loop_after_bb,
            );
        } else {
            self.loop_context_stack.borrow_mut().pop();
            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                crate::diagnostics::Severity::Error,
                "do-while loop condition must be coercible to boolean",
            ));
        }

        // Pop loop context
        self.loop_context_stack.borrow_mut().pop();

        // Position after loop
        self.builder.position_at_end(loop_after_bb);

        Ok(false) // do-while loops don't terminate unless body always returns
    }
}
