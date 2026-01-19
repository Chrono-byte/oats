use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::codegen::common_types::LocalsStackLocal;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(super) fn lower_paren_expr(
        &self,
        paren: &oxc_ast::ast::ParenthesizedExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // Unwrap parenthesized expression
        self.lower_expr(&paren.expression, function, param_map, locals)
    }
}
