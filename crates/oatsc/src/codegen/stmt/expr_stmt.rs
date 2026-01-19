use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::codegen::common_types::LocalEntry;
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
        // Evaluate expression for side-effects.
        let val = self.lower_expr(&expr_stmt.expression, function, param_map, locals_stack)?;

        // If the expression returned a new ownership stake (+1) but the result is discarded,
        // we must decrement its reference count to avoid a memory leak.
        if self.returned_ownership.get() && val.is_pointer_value() {
            let rc_dec = self.get_rc_dec();
            let _ = self
                .builder
                .build_call(rc_dec, &[val.into()], "rc_dec_discarded");
        }

        Ok(false)
    }
}
