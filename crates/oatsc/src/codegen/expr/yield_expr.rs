use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::codegen::common_types::LocalsStackLocal;

impl<'a> crate::codegen::CodeGen<'a> {
    pub(super) fn lower_yield_expr(
        &self,
        _yield_expr: &oxc_ast::ast::YieldExpression<'_>,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        // TODO: Implement yield expression lowering
        // For now, return an error as yield is not yet supported
        Err(Diagnostic::simple_boxed(
            Severity::Error,
            "`yield` expressions are not yet supported",
        ))
    }
}
