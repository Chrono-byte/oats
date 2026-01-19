use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::codegen::common_types::LocalsStackLocal;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(super) fn lower_this_expr(
        &self,
        this_expr: &oxc_ast::ast::ThisExpression,
        function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // `this` is always the first parameter in method functions
        function.get_nth_param(0).map_or_else(
            || {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "'this' used in function with no parameters",
                    this_expr.span.start as usize,
                ))
            },
            |pv| {
                // Record that this expression originated from 'this'
                self.last_expr_origin_local
                    .borrow_mut()
                    .replace("this".to_string());
                Ok(pv)
            },
        )
    }
}
