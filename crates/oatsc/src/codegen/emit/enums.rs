//! Enum generation for code generation.
//!
//! This module contains code generation logic for Rust-like enums.

use crate::diagnostics::Severity;
use oats_ast::*;

impl<'a> crate::codegen::CodeGen<'a> {
    /// Generate IR for a Rust-like enum declaration.
    ///
    /// Enums are represented as discriminated unions at runtime:
    /// - A struct with a discriminant (tag) field and a union of variant data
    /// - Each variant gets a unique tag value (0, 1, 2, ...)
    /// - Variants with no data only store the tag
    /// - Variants with data store the tag + the data as a tuple
    ///
    /// Runtime layout:
    /// - Header (8 bytes) - reference count and flags
    /// - Metadata pointer (8 bytes) - points to field map
    /// - Discriminant (i32, 4 bytes) - variant tag
    /// - Union of variant data (variable size) - largest variant's data size
    pub fn gen_enum_ir(
        &self,
        enum_name: &str,
        enum_decl: &EnumDecl,
    ) -> crate::diagnostics::DiagnosticResult<()> {
        use crate::types::OatsType;

        // Map variant field types to OatsType
        let mut variant_types: Vec<(String, Vec<OatsType>)> = Vec::new();
        for member in &enum_decl.members {
            let variant_name = member.ident.sym.clone();
            let field_types: Vec<OatsType> = if let Some(fields) = &member.fields {
                fields
                    .iter()
                    .filter_map(|ts_type| crate::types::map_ts_type_with_aliases(ts_type, Some(&*self.type_aliases.borrow())))
                    .collect()
            } else {
                Vec::new() // Unit variant, no fields
            };
            variant_types.push((variant_name, field_types));
        }

        // Store enum metadata for type checking and codegen
        self.enum_variants.borrow_mut().insert(enum_name.to_string(), variant_types.clone());

        // Create the enum struct type with discriminated union layout
        // Layout: [header (i64), meta_ptr (i8*), discriminant (i32), data union]

        // Find the largest variant to determine union size
        // For each variant, compute the total size of its fields
        // We use fixed sizes: f64/i64 = 8 bytes, f32/i32 = 4 bytes, i8* = 8 bytes, etc.
        let mut max_data_size = 0u64;
        let mut variant_data_layouts: Vec<Vec<inkwell::types::BasicTypeEnum>> = Vec::new();

        for (_, field_types) in &variant_types {
            let mut variant_fields: Vec<inkwell::types::BasicTypeEnum> = Vec::new();
            let mut variant_size = 0u64;

            for field_type in field_types {
                let llvm_type = self.map_type_to_llvm(field_type);
                // Calculate size based on type
                // Use known sizes: f64 = 8, f32 = 4, i64 = 8, i32 = 4, i8* = 8, etc.
                let field_size = match llvm_type {
                    inkwell::types::BasicTypeEnum::FloatType(ft) => {
                        // Compare with known types
                        if ft == self.f64_t { 8 } else if ft == self.f32_t { 4 } else { 8 }
                    }
                    inkwell::types::BasicTypeEnum::IntType(it) => {
                        if it == self.i64_t { 8 }
                        else if it == self.i32_t { 4 }
                        else if it == self.i16_t { 2 }
                        else if it == self.i8_t || it == self.bool_t { 1 }
                        else { 8 } // Default
                    }
                    inkwell::types::BasicTypeEnum::PointerType(_) => 8, // Pointer size
                    _ => 8, // Default to 8 bytes
                };
                variant_size += field_size;
                variant_fields.push(llvm_type);
            }

            variant_data_layouts.push(variant_fields);
            if variant_size > max_data_size {
                max_data_size = variant_size;
            }
        }

        // Ensure minimum size of 8 bytes for alignment
        if max_data_size < 8 {
            max_data_size = 8;
        }

        // Create enum struct type:
        // - Header: i64 (RC and flags)
        // - Meta pointer: i8* (field map for cycle collection)
        // - Discriminant: i32 (variant tag)
        // - Data: union of all variant data (padded to max size)
        let enum_struct_members = vec![
            self.i64_t.into(),      // header
            self.i8ptr_t.into(),    // meta pointer
            self.i32_t.into(),      // discriminant
            // Data union: use i8 array for now, will be properly typed later
            self.i8_t.array_type((max_data_size as u32).max(1)).into(),
        ];

        let _enum_struct_type = self.context.struct_type(&enum_struct_members, false);

        // Generate constructor functions for each variant
        for (variant_idx, (variant_name, field_types)) in variant_types.iter().enumerate() {
            let ctor_name = format!("{}_{}", enum_name, variant_name);

            // Build parameter types for constructor
            let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = Vec::new();
            let mut param_oats_types: Vec<OatsType> = Vec::new();

            for field_type in field_types {
                let llvm_type = self.map_type_to_llvm(field_type);
                param_types.push(llvm_type.into());
                param_oats_types.push(field_type.clone());
            }

            // Constructor returns i8* (pointer to enum instance)
            let return_type = self.i8ptr_t;
            let fn_type = return_type.fn_type(&param_types, false);
            let ctor_fn = self.module.add_function(&ctor_name, fn_type, None);

            // Create function body
            let entry_bb = self.context.append_basic_block(ctor_fn, "entry");
            self.builder.position_at_end(entry_bb);

            // Allocate enum instance
            // Size: header (8) + meta (8) + discriminant (4, rounded to 8) + data (max_data_size)
            let enum_size = 8 + 8 + 8 + max_data_size; // Align to 8-byte boundaries

            let malloc_fn = self.get_malloc();
            let size_val = self.i64_t.const_int(enum_size, false);
            let malloc_call = self.builder.build_call(malloc_fn, &[size_val.into()], "enum_malloc")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to allocate enum instance"
                    )
                })?;

            let enum_ptr = if let inkwell::Either::Left(bv) = malloc_call.try_as_basic_value() {
                bv.into_pointer_value()
            } else {
                return Err(crate::diagnostics::Diagnostic::simple_boxed(
                    Severity::Error,
                    "malloc returned unexpected type"
                ));
            };

            // Cast enum pointer to i8* for byte-level access
            let enum_i8ptr = self.builder.build_pointer_cast(enum_ptr, self.i8ptr_t, "enum_i8ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to cast enum pointer"
                    )
                })?;

            // Initialize header (RC = 1, no flags) at offset 0
            let header_val = self.i64_t.const_int(1, false);
            let header_i8ptr = enum_i8ptr;
            let header_ptr = self.builder.build_pointer_cast(header_i8ptr, self.context.ptr_type(inkwell::AddressSpace::default()), "header_ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to get header pointer"
                    )
                })?;
            let _ = self.builder.build_store(header_ptr, header_val);

            // Initialize metadata pointer (null for now) at offset 8
            let meta_offset = self.i64_t.const_int(8, false);
            let meta_i8ptr = unsafe { self.builder.build_gep(self.i8_t, enum_i8ptr, &[meta_offset], "meta_i8ptr") }
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to get meta pointer offset"
                    )
                })?;
            let meta_ptr = self.builder.build_pointer_cast(meta_i8ptr, self.i8ptr_t, "meta_ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to cast meta pointer"
                    )
                })?;
            let null_meta = self.i8ptr_t.const_null();
            let _ = self.builder.build_store(meta_ptr, null_meta);

            // Set discriminant (variant tag) at offset 16
            let disc_offset = self.i64_t.const_int(16, false);
            let disc_i8ptr = unsafe { self.builder.build_gep(self.i8_t, enum_i8ptr, &[disc_offset], "disc_i8ptr") }
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to get discriminant pointer offset"
                    )
                })?;
            let disc_ptr = self.builder.build_pointer_cast(disc_i8ptr, self.context.ptr_type(inkwell::AddressSpace::default()), "disc_ptr")
                .map_err(|_| {
                    crate::diagnostics::Diagnostic::simple_boxed(
                        Severity::Error,
                        "failed to cast discriminant pointer"
                    )
                })?;
            let disc_val = self.i32_t.const_int(variant_idx as u64, false);
            let _ = self.builder.build_store(disc_ptr, disc_val);

            // Store variant data if any (starting at offset 20, after 4-byte discriminant with padding)
            if !field_types.is_empty() {
                let data_offset = self.i64_t.const_int(24, false); // header(8) + meta(8) + disc(4) + padding(4) = 24
                let data_i8ptr = unsafe { self.builder.build_gep(self.i8_t, enum_i8ptr, &[data_offset], "data_i8ptr") }
                    .map_err(|_| {
                        crate::diagnostics::Diagnostic::simple_boxed(
                            Severity::Error,
                            "failed to get data pointer offset"
                        )
                    })?;

                // Store each field parameter into the data union
                let mut current_offset = 0u64;
                for (param_idx, field_type) in field_types.iter().enumerate() {
                    // Get the parameter value
                    if let Some(param_val) = ctor_fn.get_nth_param(param_idx as u32) {
                        let llvm_field_type = self.map_type_to_llvm(field_type);

                        // Calculate field offset within data union
                        let field_offset_val = self.i64_t.const_int(current_offset, false);
                        let field_i8ptr = unsafe { self.builder.build_gep(self.i8_t, data_i8ptr, &[field_offset_val], "field_i8ptr") }
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to get field pointer"
                                )
                            })?;

                        // Cast to appropriate type and store
                        let field_ptr_type = self.context.ptr_type(inkwell::AddressSpace::default());
                        let field_ptr = self.builder.build_pointer_cast(field_i8ptr, field_ptr_type, "field_ptr")
                            .map_err(|_| {
                                crate::diagnostics::Diagnostic::simple_boxed(
                                    Severity::Error,
                                    "failed to cast field pointer"
                                )
                            })?;

                        // Handle reference counting for pointer types
                        if matches!(field_type, OatsType::String | OatsType::NominalStruct(_) | OatsType::Array(_) | OatsType::Option(_)) {
                            let rc_inc = self.get_rc_inc();
                            let _ = self.builder.build_call(rc_inc, &[param_val.into()], "rc_inc_field");
                        }

                        let _ = self.builder.build_store(field_ptr, param_val);

                        // Update offset for next field (align to 8 bytes)
                        let field_size = match llvm_field_type {
                            inkwell::types::BasicTypeEnum::FloatType(ft) => {
                                if ft == self.f64_t { 8 } else { 4 }
                            }
                            inkwell::types::BasicTypeEnum::IntType(it) => {
                                if it == self.i64_t { 8 }
                                else if it == self.i32_t { 4 }
                                else if it == self.i16_t { 2 }
                                else if it == self.i8_t || it == self.bool_t { 1 }
                                else { 8 }
                            }
                            inkwell::types::BasicTypeEnum::PointerType(_) => 8,
                            _ => 8,
                        };
                        current_offset += (field_size + 7) & !7; // Align to 8 bytes
                    }
                }
            }

            // Return enum pointer as i8*
            let _ = self.builder.build_return(Some(&enum_i8ptr));
        }

        Ok(())
    }
}
