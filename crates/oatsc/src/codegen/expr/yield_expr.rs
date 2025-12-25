use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
#[allow(dead_code)]
type LocalEntry<'a> = (
    PointerValue<'a>,
    BasicTypeEnum<'a>,
    bool,
    bool,
    bool,
    Option<String>,
    Option<crate::types::OatsType>,
);

type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

impl<'a> crate::codegen::CodeGen<'a> {
    #[allow(clippy::result_large_err)]
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