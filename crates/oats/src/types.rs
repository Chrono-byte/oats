use std::collections::HashMap;
use deno_ast::swc::ast;
use anyhow::Result;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OatsType {
    Number,
    Boolean,
    Void,
    String,
    NominalStruct(String),
}

#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub params: Vec<OatsType>,
    pub ret: OatsType,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, OatsType>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn get(&self, name: &str) -> Option<&OatsType> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    pub fn insert(&mut self, name: String, ty: OatsType) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }
}

pub fn check_function_strictness(
    func_decl: &ast::Function,
    _symbols: &mut SymbolTable,
) -> Result<FunctionSig> {
    // Return type annotation required
    if func_decl.return_type.is_none() {
        return Err(anyhow::anyhow!("Function missing return type annotation"));
    }

    // Map a swc TypeScript type to OatsType
    fn map_ts_type(ty: &ast::TsType) -> Option<OatsType> {
        match ty {
            ast::TsType::TsKeywordType(keyword) => match keyword.kind {
                ast::TsKeywordTypeKind::TsNumberKeyword => Some(OatsType::Number),
                ast::TsKeywordTypeKind::TsVoidKeyword => Some(OatsType::Void),
                ast::TsKeywordTypeKind::TsBooleanKeyword => Some(OatsType::Boolean),
                ast::TsKeywordTypeKind::TsStringKeyword => Some(OatsType::String),
                _ => None,
            },
            ast::TsType::TsTypeRef(type_ref) => {
                // e.g., nominal type like Foo -> map to NominalStruct("Foo")
                if let Some(type_name) = type_ref.type_name.as_ident() {
                    Some(OatsType::NominalStruct(type_name.sym.to_string()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    // Collect param types
    let mut param_types = Vec::new();
    for param in &func_decl.params {
        match &param.pat {
            ast::Pat::Ident(ident) => {
                if let Some(type_ann) = &ident.type_ann {
                    let ast::TsTypeAnn { type_ann: ts_type, .. } = &**type_ann;
                    if let Some(mapped) = map_ts_type(&ts_type) {
                        param_types.push(mapped);
                        continue;
                    }
                }
                return Err(anyhow::anyhow!("Function parameter missing or unsupported type annotation"));
            }
            _ => {
                return Err(anyhow::anyhow!("Unsupported parameter pattern; only simple idents supported"));
            }
        }
    }

    // Map return type
    let ret_ty = if let Some(rt) = &func_decl.return_type {
        let ast::TsTypeAnn { type_ann: ts_type, .. } = &**rt;
        if let Some(mapped) = map_ts_type(&ts_type) {
            mapped
        } else {
            return Err(anyhow::anyhow!("Unsupported return type annotation"));
        }
    } else {
        return Err(anyhow::anyhow!("Function missing return type annotation"));
    };

    Ok(FunctionSig { params: param_types, ret: ret_ty })
}
