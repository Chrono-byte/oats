use crate::diagnostics::Severity;
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
    pub(crate) fn lower_break_stmt(
        &self,
        break_stmt: &deno_ast::swc::ast::BreakStmt,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Find the target loop context
        let target_ctx = if let Some(label) = break_stmt.label.as_ref() {
            // Labeled break: find the loop with matching label
            let label_str = label.sym.as_str();
            self.loop_context_stack
                .borrow()
                .iter()
                .rev()
                .find(|ctx| ctx.label.as_deref() == Some(label_str))
                .cloned()
        } else {
            // Unlabeled break: use innermost loop
            self.loop_context_stack.borrow().last().cloned()
        };

        if let Some(loop_ctx) = target_ctx {
            // Emit RC decrements for locals owned by the target loop
            self._emit_rc_dec_for_locals_from(locals_stack, loop_ctx.locals_start);

            // Branch to break block
            let _ = self
                .builder
                .build_unconditional_branch(loop_ctx.break_block);
        } else {
            return Err(crate::diagnostics::Diagnostic::simple_boxed(
                Severity::Error,
                "break statement outside of loop",
            ));
        }
        Ok(true)
    }

    pub(crate) fn lower_continue_stmt(
        &self,
        continue_stmt: &deno_ast::swc::ast::ContinueStmt,
        locals_stack: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Find the target loop context
        let target_ctx = if let Some(label) = continue_stmt.label.as_ref() {
            // Labeled continue: find the loop with matching label
            let label_str = label.sym.as_str();
            self.loop_context_stack
                .borrow()
                .iter()
                .rev()
                .find(|ctx| ctx.label.as_deref() == Some(label_str))
                .cloned()
        } else {
            // Unlabeled continue: use innermost loop
            self.loop_context_stack.borrow().last().cloned()
        };

        if let Some(loop_ctx) = target_ctx {
            // Emit RC decrements for locals owned by the target loop
            self._emit_rc_dec_for_locals_from(locals_stack, loop_ctx.locals_start);

            // Branch to continue block
            let _ = self
                .builder
                .build_unconditional_branch(loop_ctx.continue_block);

            Ok(true) // continue terminates the current block
        } else {
            // Continue outside of loop is a semantic error. Currently the
            // implementation ignores it; it should instead emit a
            // diagnostic. See issue #TODO (add issue) to track reporting
            // of control-flow errors.
            Ok(false)
        }
    }
}
