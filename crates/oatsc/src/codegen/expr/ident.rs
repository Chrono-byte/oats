use crate::diagnostics::{Diagnostic, Severity};
use inkwell::values::BasicValueEnum;
use inkwell::values::FunctionValue;
use std::collections::HashMap;

use crate::types::OatsType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, PointerValue};

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
    #[allow(clippy::result_large_err)]
    pub(super) fn lower_ident_expr(
        &self,
        id: &deno_ast::swc::ast::Ident,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
        locals: &mut LocalsStackLocal<'a>,
    ) -> crate::diagnostics::DiagnosticResult<BasicValueEnum<'a>> {
        let name = id.sym.to_string();

        // First, check if the identifier is a function parameter.
        if let Some(idx) = param_map.get(&name)
            && let Some(pv) = function.get_nth_param(*idx)
        {
            // Record that this expression originated from a parameter
            // so later lowering (e.g., array access) can consult
            // `fn_param_types` for more accurate element type info.
            self.last_expr_origin_local
                .borrow_mut()
                .replace(name.clone());
            return Ok(pv);
        }

        // Check if the identifier refers to an enum type (enums themselves are not values)
        if let Some(OatsType::Enum(_, _)) = self.symbol_table.borrow().get(&name) {
            return Err(Diagnostic::simple_with_span_boxed(Severity::Error, format!(
                    "'{}' is an enum type and cannot be used as a value. Use 'enum.member' syntax instead.",
                    name
                ),
                id.span.lo.0 as usize,
            ));
        }

        // Next, check if the identifier is a compile-time const we evaluated earlier.
        if let Some(cv) = self.const_items.borrow().get(&name) {
            match cv {
                crate::codegen::const_eval::ConstValue::Number(n) => {
                    let fv = self.f64_t.const_float(*n);
                    return Ok(fv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::F32(n) => {
                    let fv = self.f32_t.const_float(*n as f64);
                    return Ok(fv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::I64(n) => {
                    let iv = self.i64_t.const_int(*n as u64, true);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::I32(n) => {
                    let iv = self.i32_t.const_int(*n as u64, true);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::I16(n) => {
                    let iv = self.i16_t.const_int(*n as u64, true);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::I8(n) => {
                    let iv = self.i8_t.const_int(*n as u64, true);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::U64(n) => {
                    let iv = self.i64_t.const_int(*n, false);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::U32(n) => {
                    let iv = self.i32_t.const_int(*n as u64, false);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::U16(n) => {
                    let iv = self.i16_t.const_int(*n as u64, false);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::U8(n) => {
                    let iv = self.i8_t.const_int(*n as u64, false);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::Char(c) => {
                    let iv = self.i8_t.const_int(*c as u64, false);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::Bool(b) => {
                    let iv = self.bool_t.const_int(if *b { 1 } else { 0 }, false);
                    return Ok(iv.as_basic_value_enum());
                }
                crate::codegen::const_eval::ConstValue::Str(s) => {
                    // Intern the string literal and return its pointer
                    if let Some(ptr_val) = self.string_literals.borrow().get(s) {
                        return Ok(ptr_val.as_basic_value_enum());
                    }

                    // Create the same global struct as the literal-lowering code
                    let bytes = s.as_bytes();
                    let str_len = bytes.len();
                    let header_ty = self.i64_t;
                    let len_ty = self.i64_t;
                    let data_ty = self.context.i8_type().array_type((str_len + 1) as u32);
                    let struct_ty = self
                        .context
                        .struct_type(&[header_ty.into(), len_ty.into(), data_ty.into()], false);

                    let id = self.next_str_id.get();
                    let name = format!("strlit.{}", id);
                    self.next_str_id.set(id.wrapping_add(1));
                    let gv = self.module.add_global(struct_ty, None, &name);

                    let static_header = self.i64_t.const_int(1u64 << 32, false);
                    let length_val = self.i64_t.const_int(str_len as u64, false);
                    let data_val = self.context.const_string(bytes, true);
                    let initializer = self.context.const_struct(
                        &[static_header.into(), length_val.into(), data_val.into()],
                        false,
                    );
                    gv.set_initializer(&initializer);

                    let zero = self.i32_t.const_int(0, false);
                    let two = self.i32_t.const_int(2, false);
                    let indices = &[zero, two, zero];
                    let gep = unsafe {
                        self.builder
                            .build_gep(struct_ty, gv.as_pointer_value(), indices, "strptr")
                    };
                    if let Ok(ptr) = gep {
                        self.string_literals.borrow_mut().insert(s.clone(), ptr);
                        return Ok(ptr.as_basic_value_enum());
                    }
                    return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "failed to lower const string literal", id as usize,
                    ));
                }
                crate::codegen::const_eval::ConstValue::Array(_)
                | crate::codegen::const_eval::ConstValue::Object(_) => {
                    // If we emitted a global for this const, return its pointer
                    if let Some(gptr) = self.const_globals.borrow().get(&name) {
                        return Ok(gptr.as_basic_value_enum());
                    }
                    return Err(Diagnostic::simple_with_span_boxed(Severity::Error, "const global not emitted", id.span.lo.0 as usize,
                    ));
                }
            }
        }

        // If not a parameter, then it must be a local variable (`let` or `const`).
        if let Some((ptr, ty, initialized, _is_const, _extra, _nominal, _oats_type)) =
            self.find_local(locals, &name)
        {
            // If not initialized -> TDZ: generate a trap (unreachable).
            //
            // Emitting `unreachable` here is a simple way to guard
            // against invalid reads in generated code. In future we
            // could lower a runtime diagnostic or insert a proper
            // throw.
            if !initialized {
                // Emit a call to unreachable to trap at runtime
                let _ = self.builder.build_unreachable();
                return Err(Diagnostic::simple_boxed(Severity::Error, "expression lowering failed"))?;
            }
            let loaded = match self.builder.build_load(ty, ptr, &name) {
                Ok(v) => v,
                Err(_) => return Err(Diagnostic::simple_boxed(Severity::Error, "operation failed")),
            };
            // Record that this expression originated from local `name`.
            self.last_expr_origin_local
                .borrow_mut()
                .replace(name.clone());
            return Ok(loaded);
        }

        Err(Diagnostic::simple_with_span_boxed(Severity::Error, format!("undefined identifier '{}'", name),
            id.span.lo.0 as usize,
        ))
    }
}
