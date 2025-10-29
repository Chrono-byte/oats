use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::types::OatsType;

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
type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(crate) fn lower_if_stmt(
        &self,
        ifstmt: &deno_ast::swc::ast::IfStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Lower condition
        if let Ok(cond_val) = self.lower_expr(&ifstmt.test, function, param_map, locals_stack) {
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
                let then_terminated: bool = match &*ifstmt.cons {
                    deno_ast::swc::ast::Stmt::Block(block) => {
                        self.lower_stmts(&block.stmts, function, param_map, locals_stack)?
                    }
                    _ => self.lower_stmt(&ifstmt.cons, function, param_map, locals_stack)?,
                };
                if !then_terminated {
                    let _ = self.builder.build_unconditional_branch(merge_bb);
                }

                // Build else block
                self.builder.position_at_end(else_bb);
                let else_terminated: bool = if let Some(alt) = &ifstmt.alt {
                    match &**alt {
                        deno_ast::swc::ast::Stmt::Block(block) => {
                            self.lower_stmts(&block.stmts, function, param_map, locals_stack)?
                        }
                        _ => self.lower_stmt(alt, function, param_map, locals_stack)?,
                    }
                } else {
                    false // no else branch
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
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    pub(crate) fn lower_for_stmt(
        &self,
        forstmt: &deno_ast::swc::ast::ForStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Push new scope for the loop (init vars live in this scope)
        locals_stack.push(HashMap::new());

        // Lower init (can be VarDecl or Expr)
        if let Some(init) = &forstmt.init {
            match init {
                deno_ast::swc::ast::VarDeclOrExpr::VarDecl(var_decl) => {
                    let _ = self.lower_stmt(
                        &deno_ast::swc::ast::Stmt::Decl(deno_ast::swc::ast::Decl::Var(Box::new(
                            (**var_decl).clone(),
                        ))),
                        function,
                        param_map,
                        locals_stack,
                    );
                }
                deno_ast::swc::ast::VarDeclOrExpr::Expr(expr) => {
                    // Handle expression (e.g., i = 0)
                    let _ = self.lower_expr(expr, function, param_map, locals_stack);
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
            if let Ok(cond_val) = self.lower_expr(test, function, param_map, locals_stack) {
                // Coerce to i1 boolean
                if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                    let _ = self.builder.build_conditional_branch(
                        cond_bool,
                        loop_body_bb,
                        loop_after_bb,
                    );
                } else {
                    // Failed to coerce, bail out
                    locals_stack.pop();
                    return Ok(false);
                }
            } else {
                // Failed to lower test, bail out
                let _ = self.builder.build_unconditional_branch(loop_after_bb);
                locals_stack.pop();
                return Ok(false);
            }
        } else {
            // No test means infinite loop: always branch to body
            let _ = self.builder.build_unconditional_branch(loop_body_bb);
        }

        // Build body block
        self.builder.position_at_end(loop_body_bb);
        let body_terminated: bool = match &*forstmt.body {
            deno_ast::swc::ast::Stmt::Block(block) => {
                self.lower_stmts(&block.stmts, function, param_map, locals_stack)?
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
            let _ = self.lower_expr(update, function, param_map, locals_stack);
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
        while_stmt: &deno_ast::swc::ast::WhileStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
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
        if let Ok(cond_val) = self.lower_expr(&while_stmt.test, function, param_map, locals_stack) {
            // Coerce to i1 boolean
            if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                let _ =
                    self.builder
                        .build_conditional_branch(cond_bool, loop_body_bb, loop_after_bb);
            } else {
                self.loop_context_stack.borrow_mut().pop();
                return Ok(false);
            }
        } else {
            let _ = self.builder.build_unconditional_branch(loop_after_bb);
            self.loop_context_stack.borrow_mut().pop();
            return Ok(false);
        }

        // Build body block
        self.builder.position_at_end(loop_body_bb);
        let body_terminated: bool = match &*while_stmt.body {
            deno_ast::swc::ast::Stmt::Block(block) => {
                self.lower_stmts(&block.stmts, function, param_map, locals_stack)?
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
        dowhile_stmt: &deno_ast::swc::ast::DoWhileStmt,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut LocalsStackLocal<'a>,
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
        let body_terminated: bool = match &*dowhile_stmt.body {
            deno_ast::swc::ast::Stmt::Block(block) => {
                self.lower_stmts(&block.stmts, function, param_map, locals_stack)?
            }
            _ => self.lower_stmt(&dowhile_stmt.body, function, param_map, locals_stack)?,
        };

        // If body didn't terminate, branch to condition
        if !body_terminated {
            let _ = self.builder.build_unconditional_branch(loop_cond_bb);
        }

        // Build condition block (executed after body)
        self.builder.position_at_end(loop_cond_bb);
        if let Ok(cond_val) = self.lower_expr(&dowhile_stmt.test, function, param_map, locals_stack)
        {
            // Coerce to i1 boolean
            if let Some(cond_bool) = self.to_condition_i1(cond_val) {
                let _ = self.builder.build_conditional_branch(
                    cond_bool,
                    loop_body_bb, // Loop back to body
                    loop_after_bb,
                );
            } else {
                self.loop_context_stack.borrow_mut().pop();
                return Ok(false);
            }
        } else {
            let _ = self.builder.build_unconditional_branch(loop_after_bb);
            self.loop_context_stack.borrow_mut().pop();
            return Ok(false);
        }

        // Pop loop context
        self.loop_context_stack.borrow_mut().pop();

        // Position after loop
        self.builder.position_at_end(loop_after_bb);

        Ok(false) // do-while loops don't terminate unless body always returns
    }
}
