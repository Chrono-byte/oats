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

impl<'a> crate::codegen::CodeGen<'a> {
    pub(crate) fn lower_expr_stmt(
        &self,
        expr_stmt: &oxc_ast::ast::ExpressionStatement,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals_stack: &mut Vec<HashMap<String, LocalEntry<'a>>>,
    ) -> crate::diagnostics::DiagnosticResult<bool> {
        // Evaluate expression for side-effects. If lowering fails,
        // emit a diagnostic and continue rather than aborting the
        // whole compile. This avoids silent empty functions while
        // keeping compilation resilient for constructs like
        // `this.x = ...` during constructor emission.
        match self.lower_expr(&expr_stmt.expression, function, param_map, locals_stack) {
            Ok(_val) => { /* success, continue */ }
            Err(d) => {
                crate::diagnostics::emit_diagnostic(&d, Some(self.source));
                // don't propagate error further; continue lowering
            }
        }
        Ok(false)
    }
}
