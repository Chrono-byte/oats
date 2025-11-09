//! External dependency resolution and metadata handling.
//!
//! This module handles parsing and validation of external module dependencies,
//! including reading metadata files and declaring external functions in the LLVM module.

use anyhow::Result;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use std::collections::HashMap;

use crate::types::{FunctionSig, OatsType};

/// Resolves external dependencies and declares them in the LLVM module.
///
/// This function reads metadata files for external dependencies, parses function
/// signatures, and declares external functions in the LLVM module with appropriate
/// types.
///
/// # Arguments
/// * `context` - LLVM context for creating types
/// * `module` - LLVM module to add external function declarations to
/// * `extern_oats` - Map of import paths to metadata file paths
///
/// # Returns
/// Map of external function signatures for use during code generation
pub fn resolve_external_dependencies<'ctx>(
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    extern_oats: &HashMap<String, String>,
) -> Result<HashMap<String, (Vec<OatsType>, OatsType)>> {
    let mut external_function_signatures: HashMap<String, (Vec<OatsType>, OatsType)> =
        HashMap::new();

    // First pass: read metadata files and extract function signatures
    for meta_path in extern_oats.values() {
        let meta_content = match std::fs::read_to_string(meta_path) {
            Ok(content) => content,
            Err(e) => {
                eprintln!("Warning: Failed to read metadata file {}: {}", meta_path, e);
                continue;
            }
        };

        // Parse TypeScript-style function declarations from metadata
        // Format: function name(param1: type1, param2: type2): returnType;
        for line in meta_content.lines() {
            let line = line.trim();
            if line.starts_with("function ")
                && line.ends_with(";")
                && let Some((name, func_sig)) = parse_function_signature_from_metadata(line)
            {
                external_function_signatures.insert(name, (func_sig.params, func_sig.ret));
            }
        }
    }

    // Second pass: declare external functions in LLVM module
    for meta_path in extern_oats.values() {
        let symbols: Vec<String> = if std::path::Path::new(meta_path).exists() {
            // It's a file path - extract symbols from parsed signatures
            external_function_signatures.keys().cloned().collect()
        } else {
            // Backward compatibility: treat as comma-separated symbol list
            meta_path.split(',').map(|s| s.trim().to_string()).collect()
        };

        for symbol in symbols {
            // Get function signature or use default
            let (param_types, ret_type) = external_function_signatures
                .get(&symbol)
                .cloned()
                .unwrap_or_else(|| (vec![], OatsType::Number));

            // Convert OatsType to LLVM types
            let llvm_param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = param_types
                .iter()
                .map(|ty| map_oats_type_to_llvm(context, ty))
                .collect();

            let fn_type = match ret_type {
                OatsType::Void => {
                    let void_t = context.void_type();
                    void_t.fn_type(&llvm_param_types, false)
                }
                _ => {
                    // Convert BasicMetadataTypeEnum to BasicTypeEnum for fn_type
                    let ret_basic_type: BasicTypeEnum =
                        match map_oats_type_to_llvm(context, &ret_type) {
                            inkwell::types::BasicMetadataTypeEnum::FloatType(ft) => ft.into(),
                            inkwell::types::BasicMetadataTypeEnum::IntType(it) => it.into(),
                            inkwell::types::BasicMetadataTypeEnum::PointerType(pt) => pt.into(),
                            inkwell::types::BasicMetadataTypeEnum::ArrayType(at) => at.into(),
                            inkwell::types::BasicMetadataTypeEnum::VectorType(vt) => vt.into(),
                            inkwell::types::BasicMetadataTypeEnum::StructType(st) => st.into(),
                            inkwell::types::BasicMetadataTypeEnum::ScalableVectorType(svt) => {
                                svt.into()
                            }
                            inkwell::types::BasicMetadataTypeEnum::MetadataType(_) => {
                                // Metadata types can't be return types, default to f64
                                context.f64_type().into()
                            }
                        };
                    ret_basic_type.fn_type(&llvm_param_types, false)
                }
            };
            module.add_function(&symbol, fn_type, None);
        }
    }

    Ok(external_function_signatures)
}

/// Maps an OatsType to the corresponding LLVM BasicMetadataTypeEnum.
fn map_oats_type_to_llvm<'ctx>(
    context: &'ctx Context,
    ty: &OatsType,
) -> inkwell::types::BasicMetadataTypeEnum<'ctx> {
    match ty {
        OatsType::Number | OatsType::F64 => context.f64_type().into(),
        OatsType::F32 => context.f32_type().into(),
        OatsType::I64 => context.i64_type().into(),
        OatsType::I32 => context.i32_type().into(),
        OatsType::I8 => context.i8_type().into(),
        OatsType::Boolean => context.bool_type().into(),
        OatsType::String => context.ptr_type(inkwell::AddressSpace::default()).into(),
        _ => context.f64_type().into(), // Default to f64 for complex types
    }
}

/// Parses a function signature from metadata string.
///
/// Expected format: `function name(param1: type1, param2: type2): returnType;`
///
/// # Arguments
/// * `line` - Line containing the function signature
///
/// # Returns
/// Tuple of (function name, FunctionSig) if parsing succeeds
pub fn parse_function_signature_from_metadata(line: &str) -> Option<(String, FunctionSig)> {
    let line = line.trim().strip_suffix(';')?;
    if !line.starts_with("function ") {
        return None;
    }
    let after_fn = &line[9..]; // Skip "function "
    let paren_pos = after_fn.find('(')?;
    let name = after_fn[..paren_pos].trim().to_string();

    let after_paren = &after_fn[paren_pos + 1..];
    let close_paren_pos = after_paren.find(')')?;
    let params_str = &after_paren[..close_paren_pos];

    // Parse return type
    let ret_str = after_paren[close_paren_pos + 1..].trim();
    let ret_type = if let Some(stripped) = ret_str.strip_prefix(": ") {
        parse_oats_type_from_string(stripped)
    } else {
        OatsType::Void
    };

    // Parse parameters
    let mut param_types = Vec::new();
    if !params_str.trim().is_empty() {
        for param in params_str.split(',') {
            let param = param.trim();
            if let Some(colon_pos) = param.find(':') {
                let type_str = param[colon_pos + 1..].trim();
                param_types.push(parse_oats_type_from_string(type_str));
            }
        }
    }

    Some((
        name,
        FunctionSig {
            type_params: vec![],
            params: param_types,
            ret: ret_type,
        },
    ))
}

/// Parses an Oats type from a string representation.
fn parse_oats_type_from_string(type_str: &str) -> OatsType {
    match type_str.trim() {
        "number" => OatsType::Number,
        "f64" => OatsType::F64,
        "f32" => OatsType::F32,
        "i64" => OatsType::I64,
        "i32" => OatsType::I32,
        "i8" => OatsType::I8,
        "boolean" => OatsType::Boolean,
        "string" => OatsType::String,
        "void" => OatsType::Void,
        _ => OatsType::Number, // Default
    }
}
