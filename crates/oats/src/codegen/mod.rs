use deno_ast::swc::ast;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::FunctionValue;
use inkwell::values::PointerValue;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

pub mod emit;
pub mod expr;
pub mod helpers;
pub mod stmt;

// Locals are represented as a tuple (ptr, ty, initialized, is_const) in many
// helper modules. Use the same alias here so different files agree on the
// in-memory representation of the locals stack.
type LocalEntry<'a> = (PointerValue<'a>, BasicTypeEnum<'a>, bool, bool);
type LocalsStackLocal<'a> = Vec<std::collections::HashMap<String, LocalEntry<'a>>>;

// Loop context for tracking break/continue targets
#[derive(Clone, Copy)]
pub struct LoopContext<'a> {
    pub continue_block: inkwell::basic_block::BasicBlock<'a>,
    pub break_block: inkwell::basic_block::BasicBlock<'a>,
}

// The main code generation structure, holding the LLVM context, module,
// builder, and various caches for types and functions.
pub struct CodeGen<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
    pub next_str_id: Cell<u32>,
    pub string_literals: RefCell<HashMap<String, PointerValue<'a>>>,
    pub f64_t: inkwell::types::FloatType<'a>,
    pub i64_t: inkwell::types::IntType<'a>,
    pub i32_t: inkwell::types::IntType<'a>,
    pub bool_t: inkwell::types::IntType<'a>,
    pub i8ptr_t: inkwell::types::PointerType<'a>,
    pub fn_print_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_print_str: RefCell<Option<FunctionValue<'a>>>,
    pub fn_strlen: RefCell<Option<FunctionValue<'a>>>,
    pub fn_malloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_memcpy: RefCell<Option<FunctionValue<'a>>>,
    pub fn_free: RefCell<Option<FunctionValue<'a>>>,
    pub fn_array_alloc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_inc: RefCell<Option<FunctionValue<'a>>>,
    pub fn_rc_dec: RefCell<Option<FunctionValue<'a>>>,
    pub fn_number_to_string: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_box_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_box_ptr: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_unbox_f64: RefCell<Option<FunctionValue<'a>>>,
    pub fn_union_unbox_ptr: RefCell<Option<FunctionValue<'a>>>,
    pub class_fields: RefCell<HashMap<String, Vec<(String, crate::types::OatsType)>>>,
    pub fn_param_types: RefCell<HashMap<String, Vec<crate::types::OatsType>>>,
    pub loop_context_stack: RefCell<Vec<LoopContext<'a>>>,
    pub source: &'a str,
}

impl<'a> CodeGen<'a> {
    // --- Runtime Helper Function Getters ---

    fn get_array_alloc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_array_alloc.borrow() {
            return f;
        }
        let fn_type = self.i8ptr_t.fn_type(
            &[self.i64_t.into(), self.i32_t.into(), self.i32_t.into()],
            false,
        );
        let f = self.module.add_function("array_alloc", fn_type, None);
        *self.fn_array_alloc.borrow_mut() = Some(f);
        f
    }

    pub fn get_rc_inc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_inc.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_inc", fn_type, None);
        *self.fn_rc_inc.borrow_mut() = Some(f);
        f
    }

    fn get_math_random(&self) -> FunctionValue<'a> {
        self.module.get_function("math_random").unwrap_or_else(|| {
            let fn_type = self.f64_t.fn_type(&[], false);
            self.module.add_function("math_random", fn_type, None)
        })
    }

    fn get_rc_dec(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_rc_dec.borrow() {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("rc_dec", fn_type, None);
        *self.fn_rc_dec.borrow_mut() = Some(f);
        f
    }

    fn get_number_to_string(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_number_to_string.borrow() {
            return f;
        }
        // number_to_string(f64) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function("number_to_string", fn_type, None);
        *self.fn_number_to_string.borrow_mut() = Some(f);
        f
    }

    fn get_union_box_f64(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_box_f64.borrow() {
            return f;
        }
        // union_box_f64(f64) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.f64_t.into()], false);
        let f = self.module.add_function("union_box_f64", fn_type, None);
        *self.fn_union_box_f64.borrow_mut() = Some(f);
        f
    }

    fn get_union_box_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_box_ptr.borrow() {
            return f;
        }
        // union_box_ptr(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("union_box_ptr", fn_type, None);
        *self.fn_union_box_ptr.borrow_mut() = Some(f);
        f
    }

    fn get_union_unbox_f64(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_unbox_f64.borrow() {
            return f;
        }
        // union_unbox_f64(i8*) -> f64
        let fn_type = self.f64_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("union_unbox_f64", fn_type, None);
        *self.fn_union_unbox_f64.borrow_mut() = Some(f);
        f
    }

    fn get_union_unbox_ptr(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_union_unbox_ptr.borrow() {
            return f;
        }
        // union_unbox_ptr(i8*) -> i8*
        let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
        let f = self.module.add_function("union_unbox_ptr", fn_type, None);
        *self.fn_union_unbox_ptr.borrow_mut() = Some(f);
        f
    }

    fn get_array_get_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_get_f64")
            .unwrap_or_else(|| {
                let fn_type = self
                    .f64_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_f64", fn_type, None)
            })
    }

    fn get_array_get_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_get_ptr")
            .unwrap_or_else(|| {
                let fn_type = self
                    .i8ptr_t
                    .fn_type(&[self.i8ptr_t.into(), self.i64_t.into()], false);
                self.module.add_function("array_get_ptr", fn_type, None)
            })
    }

    fn get_array_push_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_push_f64")
            .unwrap_or_else(|| {
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[self.i8ptr_t.into(), self.f64_t.into()], false);
                self.module.add_function("array_push_f64", fn_type, None)
            })
    }

    fn get_array_pop_f64(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_pop_f64")
            .unwrap_or_else(|| {
                let fn_type = self.f64_t.fn_type(&[self.i8ptr_t.into()], false);
                self.module.add_function("array_pop_f64", fn_type, None)
            })
    }

    fn get_array_push_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_push_ptr")
            .unwrap_or_else(|| {
                let fn_type = self
                    .context
                    .void_type()
                    .fn_type(&[self.i8ptr_t.into(), self.i8ptr_t.into()], false);
                self.module.add_function("array_push_ptr", fn_type, None)
            })
    }

    fn get_array_pop_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_pop_ptr")
            .unwrap_or_else(|| {
                let fn_type = self.i8ptr_t.fn_type(&[self.i8ptr_t.into()], false);
                self.module.add_function("array_pop_ptr", fn_type, None)
            })
    }

    fn get_array_set_ptr(&self) -> FunctionValue<'a> {
        self.module
            .get_function("array_set_ptr")
            .unwrap_or_else(|| {
                // array_set_ptr(arr: i8*, idx: i64, p: i8*) -> void
                let fn_type = self.context.void_type().fn_type(
                    &[self.i8ptr_t.into(), self.i64_t.into(), self.i8ptr_t.into()],
                    false,
                );
                self.module.add_function("array_set_ptr", fn_type, None)
            })
    }

    // --- Main Function Generation ---

    // Function generation moved to emit.rs

    // --- Function Generation Helpers ---

    // Helper to build the LLVM function type from parameter and return types.
    fn build_llvm_fn_type(
        &self,
        llvm_param_types: &[BasicTypeEnum<'a>],
        ret_type: &crate::types::OatsType,
    ) -> inkwell::types::FunctionType<'a> {
        let args: Vec<_> = llvm_param_types.iter().map(|&t| t.into()).collect();
        match ret_type {
            crate::types::OatsType::Void => self.context.void_type().fn_type(&args, false),
            _ => self.map_type_to_llvm(ret_type).fn_type(&args, false),
        }
    }

    // Creates stack allocations (`alloca`) for all function parameters, making them
    // accessible like local variables and handling initial reference counting.
    fn create_param_allocas(
        &self,
        function: FunctionValue<'a>,
        func_decl: &ast::Function,
        llvm_param_types: &[BasicTypeEnum<'a>],
        receiver_name: Option<&str>,
    ) -> Result<(HashMap<String, u32>, LocalsStackLocal<'a>), crate::diagnostics::Diagnostic> {
        let mut param_map = HashMap::new();
        if let Some(rname) = receiver_name {
            param_map.insert(rname.to_string(), 0u32);
        }
        for (i, p) in func_decl.params.iter().enumerate() {
            if let ast::Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                let idx = (i + receiver_name.map_or(0, |_| 1)) as u32;
                param_map.insert(name, idx);
            }
        }

        // Initialize an empty locals stack. Allocas for `let` and `const` will be added later.
        let locals_stack: LocalsStackLocal<'a> = vec![HashMap::new()];

        // We no longer create allocas for parameters. Still, increment RC for
        // pointer-typed parameters so ownership is consistent with previous behavior.
        for &idx in param_map.values() {
            if let Some(param_ty) = llvm_param_types.get(idx as usize)
                && param_ty.is_pointer_type()
                && let Some(pv) = function.get_nth_param(idx)
            {
                let rc_inc = self.get_rc_inc();
                if self
                    .builder
                    .build_call(rc_inc, &[pv.into()], "rc_inc_param")
                    .is_err()
                {
                    return Err(crate::diagnostics::Diagnostic::simple(
                        "rc_inc param call failed",
                    ));
                }
            }
        }
        Ok((param_map, locals_stack))
    }

    // Lowers a slice of AST statements into the current basic block.
    // Statement lowering functions have been moved to stmt.rs

    // --- Host Main Generation ---

    pub fn emit_host_main(
        &self,
        _params: &[crate::types::OatsType],
        _ret: &crate::types::OatsType,
    ) -> bool {
        // Emit a simple C-compatible `main` function that calls the
        // generated `oats_main` symbol. This covers the common case where
        // user scripts export `function main(): number` with no params.
        // Return `true` to indicate we emitted a host main so the driver
        // will skip linking an external `rt_main.o`.

        // Only handle the simple case: no params, integer return or void.
        // Build: int main(int argc, char** argv) { int r = oats_main(); return r; }
        let i32_t = self.i32_t;
        let i8ptr_t = self.i8ptr_t;

        // Build function type: i32 (int) with (i32, i8**) params
        let fn_type = i32_t.fn_type(&[i32_t.into(), i8ptr_t.as_basic_type_enum().into()], false);
        let main_fn = self.module.add_function("main", fn_type, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Try to look up oats_main; it should be present if gen_function_ir emitted it.
        let oats_main_fn = match self.module.get_function("oats_main") {
            Some(f) => f,
            None => {
                // No oats_main available; emit an empty main that returns 1
                let const_one = i32_t.const_int(1, false);
                let _ = self.builder.build_return(Some(&const_one));
                return true;
            }
        };

        // Call oats_main(); we only support a no-arg oats_main here.
        let call_site = match self.builder.build_call(oats_main_fn, &[], "call_oats_main") {
            Ok(cs) => cs,
            Err(_) => {
                crate::diagnostics::emit_diagnostic(
                    &crate::diagnostics::Diagnostic::simple("failed to build call to oats_main"),
                    Some(self.source),
                );
                let const_zero = i32_t.const_int(0, false);
                let _ = self.builder.build_return(Some(&const_zero));
                return true;
            }
        };
        // Interpret the result depending on its type. If the function returns an i64 or f64
        // we coerce/truncate to i32; if void, return 0.
        let either = call_site.try_as_basic_value();
        if let inkwell::Either::Left(bv) = either {
            let ret_val = if bv.get_type().is_int_type() {
                // Truncate or bitcast to i32 if needed
                let rv_int = bv.into_int_value();
                let cast = match self
                    .builder
                    .build_int_truncate_or_bit_cast(rv_int, i32_t, "ret_i32")
                {
                    Ok(c) => c,
                    Err(_) => {
                        crate::diagnostics::emit_diagnostic(
                            &crate::diagnostics::Diagnostic::simple(
                                "int cast failed when building host main",
                            ),
                            Some(self.source),
                        );
                        i32_t.const_int(0, false)
                    }
                };
                inkwell::values::BasicValueEnum::IntValue(cast)
            } else if bv.get_type().is_float_type() {
                // cast float to i32 via fptosi
                let fv = bv.into_float_value();
                let conv = match self.builder.build_float_to_signed_int(fv, i32_t, "f_to_i") {
                    Ok(c) => c,
                    Err(_) => {
                        crate::diagnostics::emit_diagnostic(
                            &crate::diagnostics::Diagnostic::simple(
                                "float->int conversion failed in host main",
                            ),
                            Some(self.source),
                        );
                        i32_t.const_int(0, false)
                    }
                };
                inkwell::values::BasicValueEnum::IntValue(conv)
            } else if bv.get_type().is_pointer_type() {
                // pointer return -> return 0
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            } else {
                inkwell::values::BasicValueEnum::IntValue(i32_t.const_int(0, false))
            };
            let _ = self.builder.build_return(Some(&ret_val));
        } else {
            // No basic return (void), return 0
            let const_zero = i32_t.const_int(0, false);
            let _ = self.builder.build_return(Some(&const_zero));
        }

        true
    }

    // Generate a complete constructor function for a class.
    // The constructor allocates memory for the header + fields, initializes the header
    // with refcount=1, and runs the constructor body (which may initialize fields via
    // assignments or use constructor parameters).
    //
    // # Arguments
    // * `class_name` - Name of the class (e.g., "Point")
    // * `ctor` - The constructor AST node
    // * `fields` - Ordered list of (field_name, field_type) tuples
    //
    // The emitted function signature is:
    //   `ClassName_ctor(param1, param2, ...) -> i8*`

    fn get_malloc(&self) -> FunctionValue<'a> {
        if let Some(f) = *self.fn_malloc.borrow() {
            return f;
        }
        let fn_type = self.i8ptr_t.fn_type(&[self.i64_t.into()], false);
        let f = self.module.add_function("malloc", fn_type, None);
        *self.fn_malloc.borrow_mut() = Some(f);
        f
    }
}
