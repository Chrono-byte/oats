use crate::types::OatsType;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicType;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValue, BasicValueEnum, CallSiteValue, FunctionValue};
use inkwell::AddressSpace;
use std::collections::HashMap;

pub struct CodeGen<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
}

impl<'a> CodeGen<'a> {
    fn declare_libc(&self) {
        // declare malloc(i64) -> i8*
        if self.module.get_function("malloc").is_none() {
            let i8ptr = self.context.ptr_type(AddressSpace::default());
            let i64t = self.context.i64_type();
            let malloc_ty = i8ptr.fn_type(&[i64t.into()], false);
            self.module.add_function("malloc", malloc_ty, None);
        }

        // declare strlen(i8*) -> i64
        if self.module.get_function("strlen").is_none() {
            let i8ptr = self.context.ptr_type(AddressSpace::default());
            let i64t = self.context.i64_type();
            let strlen_ty = i64t.fn_type(&[i8ptr.into()], false);
            self.module.add_function("strlen", strlen_ty, None);
        }

        // declare memcpy(i8*, i8*, i64) -> i8*
        if self.module.get_function("memcpy").is_none() {
            let i8ptr = self.context.ptr_type(AddressSpace::default());
            let i64t = self.context.i64_type();
            let memcpy_ty = i8ptr.fn_type(&[i8ptr.into(), i8ptr.into(), i64t.into()], false);
            self.module.add_function("memcpy", memcpy_ty, None);
        }

        // declare free(i8*) -> void
        if self.module.get_function("free").is_none() {
            let voidt = self.context.void_type();
            let i8ptr = self.context.ptr_type(AddressSpace::default());
            let free_ty = voidt.fn_type(&[i8ptr.into()], false);
            self.module.add_function("free", free_ty, None);
        }
    }

    fn gen_str_concat(&self) {
        if self.module.get_function("str_concat").is_some() {
            return;
        }
        // Instead of emitting a full definition here (which can cause
        // duplicate-symbol linker errors when we also link the runtime
        // staticlib that provides `str_concat`), only declare the symbol
        // so the runtime implementation will be used at link time.
        let i8ptr = self.context.ptr_type(AddressSpace::default());
        let fn_type = i8ptr.fn_type(&[i8ptr.into(), i8ptr.into()], false);

        // Ensure libc declarations exist so calls can be built elsewhere.
        self.declare_libc();

        // Add the function declaration (no body). If a definition is needed
        // later (for a self-contained IR mode) we can change this behavior.
        let _function = self.module.add_function("str_concat", fn_type, None);
    }

    pub fn map_type_to_llvm(&self, ty: &OatsType) -> BasicTypeEnum<'a> {
        match ty {
            OatsType::Number => self.context.f64_type().as_basic_type_enum(),
            OatsType::Boolean => self.context.bool_type().as_basic_type_enum(),
            OatsType::Void => panic!("Void type is not valid for function parameters"),
            OatsType::String => {
                // represent strings as i8* (pointer) using context.ptr_type
                self.context
                    .ptr_type(AddressSpace::default())
                    .as_basic_type_enum()
            }
            OatsType::NominalStruct(_) => self
                .context
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
        }
    }

    fn lower_expr(
        &self,
        expr: &deno_ast::swc::ast::Expr,
        function: FunctionValue<'a>,
        param_map: &HashMap<String, u32>,
    ) -> Option<BasicValueEnum<'a>> {
        use deno_ast::swc::ast;

        match expr {
            ast::Expr::Bin(bin) => {
                // Handle only `+` for prototype
                use deno_ast::swc::ast::BinaryOp;
                if let BinaryOp::Add = bin.op {
                    let l = self.lower_expr(&bin.left, function, param_map)?;
                    let r = self.lower_expr(&bin.right, function, param_map)?;

                    match (l, r) {
                        (BasicValueEnum::FloatValue(lf), BasicValueEnum::FloatValue(rf)) => {
                            let sum = self
                                .builder
                                .build_float_add(lf, rf, "sum")
                                .expect("build_float_add failed");
                            Some(sum.as_basic_value_enum())
                        }
                        (BasicValueEnum::PointerValue(lp), BasicValueEnum::PointerValue(rp)) => {
                            if let Some(strcat) = self.module.get_function("str_concat") {
                                let call_site = self.builder.build_call(
                                    strcat,
                                    &[lp.into(), rp.into()],
                                    "concat",
                                );
                                let call_site: CallSiteValue =
                                    call_site.expect("build_call failed");
                                let either = call_site.try_as_basic_value();
                                match either {
                                    inkwell::Either::Left(bv) => Some(bv),
                                    _ => None,
                                }
                            } else {
                                None
                            }
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            ast::Expr::Ident(id) => {
                let name = id.sym.to_string();
                if let Some(idx) = param_map.get(&name) {
                    return Some(function.get_nth_param(*idx).unwrap().into());
                }
                None
            }
            ast::Expr::Call(call) => {
                // Support simple identifier callees (calls to nested or module-level functions)
                if let ast::Callee::Expr(boxed_expr) = &call.callee {
                    if let ast::Expr::Ident(ident) = &**boxed_expr {
                        let fname = ident.sym.to_string();
                        if let Some(fv) = self.module.get_function(&fname) {
                            // Lower args
                            let mut lowered_args: Vec<inkwell::values::BasicMetadataValueEnum> = Vec::new();
                            for a in &call.args {
                                if let Some(val) = self.lower_expr(&a.expr, function, param_map) {
                                    lowered_args.push(val.into());
                                } else {
                                    // unsupported arg lowering
                                    return None;
                                }
                            }
                            let cs = self.builder.build_call(fv, &lowered_args, "call_internal").expect("build_call failed");
                            let either = cs.try_as_basic_value();
                            match either {
                                inkwell::Either::Left(bv) => Some(bv),
                                _ => None,
                            }
                        } else {
                            
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            ast::Expr::Lit(lit) => {
                use deno_ast::swc::ast::Lit;
                match &*lit {
                    Lit::Num(n) => {
                        let fv = self.context.f64_type().const_float(n.value);
                        Some(fv.as_basic_value_enum())
                    }
                    Lit::Str(s) => {
                        let bytes = s.value.as_bytes();
                        let array_ty = self.context.i8_type().array_type((bytes.len() + 1) as u32);
                        let gv = self.module.add_global(array_ty, None, "strlit");
                        let const_array = self.context.const_string(bytes, true);
                        gv.set_initializer(&const_array);
                        let zero = self.context.i32_type().const_int(0, false);
                        let indices = &[zero, zero];
                        // build_gep is unsafe in inkwell; use the pointer value
                        let gep = unsafe {
                            self.builder.build_gep(
                                array_ty,
                                gv.as_pointer_value(),
                                indices,
                                "strptr",
                            )
                        };
                        if let Ok(ptr) = gep {
                            Some(ptr.as_basic_value_enum())
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn gen_function_ir(
        &self,
        func_name: &str,
        func_decl: &deno_ast::swc::ast::Function,
        param_types: &[crate::types::OatsType],
        ret_type: &crate::types::OatsType,
    ) -> FunctionValue<'a> {
        let llvm_param_types: Vec<inkwell::types::BasicTypeEnum> = param_types
            .iter()
            .map(|t| self.map_type_to_llvm(t))
            .collect();

        // Build function type, supporting Void return.
        let fn_type = match ret_type {
            crate::types::OatsType::Number => {
                let ft = self.context.f64_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                ft.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Boolean => {
                let it = self.context.bool_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                it.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::String | crate::types::OatsType::NominalStruct(_) => {
                let pt = self.context.ptr_type(AddressSpace::default());
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                pt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            crate::types::OatsType::Void => {
                let vt = self.context.void_type();
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                vt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
        };

        // Ensure helper runtime functions (like str_concat) are emitted into the module
        self.gen_str_concat();

        let function = self.module.add_function(func_name, fn_type, None);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // str_concat will be provided by emitted IR helpers at AOT time.

        // Build param name -> index map from function declaration params
        let mut param_map: HashMap<String, u32> = HashMap::new();
        for (i, p) in func_decl.params.iter().enumerate() {
            use deno_ast::swc::ast::Pat;
            if let Pat::Ident(ident) = &p.pat {
                let name = ident.id.sym.to_string();
                param_map.insert(name, i as u32);
            }
        }

        if let Some(body) = &func_decl.body {
            use deno_ast::swc::ast;
            for stmt in &body.stmts {
                if let ast::Stmt::Return(ret) = stmt {
                    if let Some(arg) = &ret.arg {
                        if let Some(val) = self.lower_expr(arg, function, &param_map) {
                            let _ = self.builder.build_return(Some(&val));
                        } else {
                            let _ = self.builder.build_return(None);
                        }
                        break;
                    }
                }
            }
        } else {
            let _ = self.builder.build_return(None);
        }

        function
    }

    /// Emit a small C-compatible `main` function in the module that calls the
    /// generated `oats_main`. This avoids needing a separate host shim object.
    ///
    /// Supported script signatures:
    /// - `main()` -> calls `oats_main()` and ignores return value
    /// - `main(a: number, b: number)` -> calls `oats_main(1.5, 2.25)` and prints the
    ///   returned double via `print_f64` (provided by the runtime staticlib).
    /// Returns true if a host `main` was emitted, false if the signature is
    /// unsupported.
    pub fn emit_host_main(&self, param_types: &[crate::types::OatsType], _ret_type: &crate::types::OatsType) -> bool {
        // If main already present, nothing to do
        if self.module.get_function("main").is_some() {
            return true;
        }

        // Ensure the oats_main symbol exists
        let oats_fn = match self.module.get_function("oats_main") {
            Some(f) => f,
            None => return false,
        };

        // Do not declare or call print_f64 here; printing is the host's
        // responsibility. This keeps generated modules free of output
        // side-effects and makes the host agnostic.

        // Build main: int main()
        let i32t = self.context.i32_type();
        let main_ty = i32t.fn_type(&[], false);
        let main_fn = self.module.add_function("main", main_ty, None);
        let entry = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(entry);

        // Prepare call args based on signature
    let _call_site = match param_types.len() {
            0 => {
                // call oats_main()
                self.builder.build_call(oats_fn, &[], "call_oats_main").expect("build_call failed")
            }
            2 => {
                // Both params must be numbers for our simple host
                use crate::types::OatsType;
                if param_types[0] != OatsType::Number || param_types[1] != OatsType::Number {
                    // unsupported
                    let zero = i32t.const_int(1, false);
                    let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
                    return false;
                }
                let a = self.context.f64_type().const_float(1.5);
                let b = self.context.f64_type().const_float(2.25);
                self.builder.build_call(oats_fn, &[a.into(), b.into()], "call_oats_main").expect("build_call failed")
            }
            _ => {
                // unsupported signature
                let zero = i32t.const_int(1, false);
                let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
                return false;
            }
        };


        // Do not perform any automatic printing of return values here.
        // The host (rt_main) or a user-level helper should handle I/O.

        let zero = i32t.const_int(0, false);
        let _ = self.builder.build_return(Some(&zero.as_basic_value_enum()));
        // Also emit a small uniform entrypoint `oats_entry()` which the
        // external host object can call without knowing the script
        // signature. `oats_entry` will call `oats_main` with the same
        // argument handling as `main` above but will not attempt printing;
        // it simply invokes the generated function and returns.
        if self.module.get_function("oats_entry").is_none() {
            // Emit a silent no-arg oats_entry() that calls the generated oats_main
            // using the same simple argument preparation as the host `main`.
            let voidt = self.context.void_type();
            let entry_ty = voidt.fn_type(&[], false);
            let entry_fn = self.module.add_function("oats_entry", entry_ty, None);
            let entry_bb = self.context.append_basic_block(entry_fn, "entry");
            self.builder.position_at_end(entry_bb);

            match param_types.len() {
                0 => {
                    // call oats_main()
                    let _ = self.builder.build_call(oats_fn, &[], "call_oats_main").expect("build_call failed");
                }
                2 => {
                    use crate::types::OatsType;
                    if param_types[0] == OatsType::Number && param_types[1] == OatsType::Number {
                        let a = self.context.f64_type().const_float(1.5);
                        let b = self.context.f64_type().const_float(2.25);
                        let _ = self.builder.build_call(oats_fn, &[a.into(), b.into()], "call_oats_main").expect("build_call failed");
                    }
                }
                _ => {
                    // unsupported signature for automatic invocation; do nothing
                }
            }

            let _ = self.builder.build_return(None);
        }

        true
    }
}
