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
    pub(super) fn lower_this_expr(
        &self,
        this_expr: &deno_ast::swc::ast::ThisExpr,
        function: FunctionValue<'a>,
        _param_map: &HashMap<String, u32>,
        _locals: &mut LocalsStackLocal<'a>,
    ) -> Result<BasicValueEnum<'a>, Diagnostic> {
        // `this` is always the first parameter in method functions
        if let Some(pv) = function.get_nth_param(0) {
            // Record that this expression originated from 'this'
            self.last_expr_origin_local
                .borrow_mut()
                .replace("this".to_string());
            Ok(pv)
        } else {
            Err(Diagnostic::simple_with_span(
                "'this' used in function with no parameters",
                this_expr.span.lo.0 as usize,
            ))
        }
    }
}