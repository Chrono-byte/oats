use crate::diagnostics::Diagnostic;
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;

// LocalEntry now includes an Option<String> for an optional nominal type name
// LocalEntry now includes an Option<OatsType> for union tracking
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
    pub(super) fn lower_member_expr(
        &self,
        _member: &deno_ast::swc::ast::MemberExpr,
        _function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        // TODO: Implement member access (dot-notation and computed)
        Err(Diagnostic::simple("member access not implemented yet"))
    }
}
