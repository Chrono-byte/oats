use crate::codegen::CodeGen;
use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue};
use oxc_ast::ast::{Expression, NewExpression};
use std::collections::HashMap;

use crate::codegen::common_types::LocalsStackLocal;

impl<'a> CodeGen<'a> {
    pub(super) fn lower_new_expr(
        &self,
        new_expr: &NewExpression<'_>,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        if let Expression::Identifier(ident) = &new_expr.callee {
            let name = ident.name.to_string();

            // Check if this is a generic class instantiation
            let actual_class_name = if let Some(type_args) = &new_expr.type_arguments {
                // This is a generic instantiation - monomorphize the class
                let type_arg_types = type_args
                    .params
                    .iter()
                    .filter_map(|ta| {
                        crate::types::map_ts_type_with_aliases(
                            ta,
                            Some(&*self.type_aliases.borrow()),
                            Some(&*self.symbol_table.borrow()),
                        )
                    })
                    .collect::<Vec<_>>();

                if type_arg_types.len() == type_args.params.len() {
                    // Successfully parsed all type arguments
                    match self.monomorphize_class(&name, &type_arg_types) {
                        Ok(specialized_name) => specialized_name,
                        Err(diag) => {
                            return Err(diag);
                        }
                    }
                } else {
                    // Fallback to original name if type args couldn't be parsed
                    name
                }
            } else {
                name
            };

            // Resolve class name through symbol table
            let class_name = if let Some(crate::types::Symbol::Type { mangled_name }) =
                self.symbol_table.borrow().get(&actual_class_name)
            {
                mangled_name.clone()
            } else {
                actual_class_name
            };

            let ctor_name = format!("{class_name}_ctor");
            if let Some(fv) = self.module.get_function(&ctor_name) {
                let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();

                // Get constructor parameter types for coercion
                let param_types = self.fn_param_types.borrow().get(&ctor_name).cloned();

                for (i, arg) in new_expr.arguments.iter().enumerate() {
                    // Spread arguments are not yet supported in lowering.
                    let expr = arg.as_expression().ok_or_else(|| {
                        Diagnostic::simple_with_span_boxed(
                            Severity::Error,
                            "spread arguments are not supported",
                            new_expr.span.start as usize,
                        )
                    })?;
                    let mut val = self.lower_expr(expr, function, param_map, locals)?;

                    // Always box floats in constructor calls for debugging
                    if val.get_type().is_float_type() {
                        let union_box_f64 = self.get_union_box_f64();
                        let call = self
                            .builder
                            .build_call(union_box_f64, &[val.into()], "union_box_number_ctor")
                            .map_err(|_| {
                                Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to box number into union in constructor",
                                )
                            })?;
                        val = match call.try_as_basic_value() {
                            inkwell::values::ValueKind::Basic(bv) => bv,
                            _ => self.f64_t.const_float(0.0).as_basic_value_enum(),
                        };
                    }

                    // Also try the normal coercion
                    if let Some(ref param_types_vec) = param_types
                        && let Some(param_type) = param_types_vec.get(i)
                    {
                        val = self.coerce_argument_to_parameter_type(val, param_type)?;
                    }

                    lowered_args.push(val.into());
                }
                // Ensure the number of arguments matches the constructor's
                // declared parameter count. Truncate extra args or pad with
                // nulls if the function expects more parameters. This fixes
                // ABI mismatches where the constructor signature and callsite
                // disagree.
                let expected = fv.count_params() as usize;
                let mut call_args = lowered_args.clone();
                if call_args.len() > expected {
                    call_args.truncate(expected);
                } else if call_args.len() < expected {
                    // pad with null pointers
                    while call_args.len() < expected {
                        call_args.push(self.i8ptr_t.const_null().as_basic_value_enum().into());
                    }
                }

                let cs = self
                    .builder
                    .build_call(fv, &call_args, "new_call")
                    .map_err(|_| Diagnostic::error("LLVM builder error"))?;
                match cs.try_as_basic_value() {
                    inkwell::values::ValueKind::Basic(bv) => Ok(bv),
                    _ => Err(Diagnostic::simple_boxed(
                        Severity::Error,
                        "operation not supported",
                    )),
                }
            } else {
                Err(Diagnostic::simple_with_span_boxed(
                    Severity::Error,
                    "unknown constructor or missing `<Name>_ctor` function",
                    new_expr.span.start as usize,
                ))
            }
        } else {
            Err(Diagnostic::simple_with_span_boxed(
                Severity::Error,
                "unsupported `new` callee: only identifier constructors are supported",
                new_expr.span.start as usize,
            ))
        }
    }
}
