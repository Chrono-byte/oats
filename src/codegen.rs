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

        self.declare_libc();

        // define i8* @str_concat(i8* %a, i8* %b)
        let i8ptr = self.context.ptr_type(AddressSpace::default());
        let fn_type = i8ptr.fn_type(&[i8ptr.into(), i8ptr.into()], false);
        let function = self.module.add_function("str_concat", fn_type, None);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        let a = function.get_nth_param(0).unwrap().into_pointer_value();
        let b = function.get_nth_param(1).unwrap().into_pointer_value();

        // call strlen(a)
        let strlen_fn = self.module.get_function("strlen").unwrap();
        let la_cs = self
            .builder
            .build_call(strlen_fn, &[a.into()], "la_call")
            .expect("build_call failed");
        let la = la_cs.try_as_basic_value().left().unwrap().into_int_value();

        // call strlen(b)
        let lb_cs = self
            .builder
            .build_call(strlen_fn, &[b.into()], "lb_call")
            .expect("build_call failed");
        let lb = lb_cs.try_as_basic_value().left().unwrap().into_int_value();

        // total = la + lb + 1
        let sum = self
            .builder
            .build_int_add(la, lb, "sum_len")
            .expect("build_int_add failed");
        let one = self.context.i64_type().const_int(1, false);
        let total = self
            .builder
            .build_int_add(sum, one, "total_len")
            .expect("build_int_add failed");

        // call malloc(total)
        let malloc_fn = self.module.get_function("malloc").unwrap();
        let malloc_cs = self
            .builder
            .build_call(malloc_fn, &[total.into()], "malloc_call")
            .expect("build_call failed");
        let ptr = malloc_cs
            .try_as_basic_value()
            .left()
            .unwrap()
            .into_pointer_value();

        // memcpy(ptr, a, la)
        let memcpy_fn = self.module.get_function("memcpy").unwrap();
        let _ = self
            .builder
            .build_call(memcpy_fn, &[ptr.into(), a.into(), la.into()], "copy1");

        // dst = ptr + la
        let dst = unsafe {
            self.builder
                .build_gep(self.context.i8_type(), ptr, &[la], "dst")
        }
        .unwrap();

        // memcpy(dst, b, lb)
        let _ = self
            .builder
            .build_call(memcpy_fn, &[dst.into(), b.into(), lb.into()], "copy2");

        // store null terminator at ptr + la + lb
        let idx = self
            .builder
            .build_int_add(la, lb, "idx")
            .expect("build_int_add failed");
        let term_ptr = unsafe {
            self.builder
                .build_gep(self.context.i8_type(), ptr, &[idx], "term")
        }
        .unwrap();
        let zero = self.context.i8_type().const_int(0, false);
        let _ = self
            .builder
            .build_store(term_ptr, zero.as_basic_value_enum());

        let _ = self.builder.build_return(Some(&ptr.as_basic_value_enum()));
    }

    pub fn map_type_to_llvm(&self, ty: &OatsType) -> BasicTypeEnum<'a> {
        match ty {
            OatsType::Number => self.context.f64_type().as_basic_type_enum(),
            OatsType::Boolean => self.context.bool_type().as_basic_type_enum(),
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

        let ret_basic = self.map_type_to_llvm(ret_type);

        let fn_type = match ret_basic {
            BasicTypeEnum::FloatType(ft) => {
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                ft.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            BasicTypeEnum::IntType(it) => {
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                it.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            BasicTypeEnum::PointerType(pt) => {
                let args: Vec<inkwell::types::BasicTypeEnum> = llvm_param_types.clone();
                pt.fn_type(&args.iter().map(|a| (*a).into()).collect::<Vec<_>>(), false)
            }
            _ => panic!("unsupported return type"),
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
}
