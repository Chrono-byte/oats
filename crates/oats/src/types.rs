use anyhow::Result;
use deno_ast::swc::ast;
use std::collections::HashMap;

/// Type alias for the locals stack used in codegen
pub type LocalsStack = Vec<HashMap<String, OatsType>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OatsType {
    Number,
    Boolean,
    /// Array of element type (e.g. number[])
    Array(Box<OatsType>),
    Void,
    String,
    NominalStruct(String),
    /// Promise type wrapping a result type (e.g. Promise<number>)
    Promise(Box<OatsType>),
}

#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub params: Vec<OatsType>,
    pub ret: OatsType,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, OatsType>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
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

/// Map a TypeScript AST type to an OatsType.
/// Returns None if the type is not supported.
pub fn map_ts_type(ty: &ast::TsType) -> Option<OatsType> {
    match ty {
        ast::TsType::TsKeywordType(keyword) => match keyword.kind {
            ast::TsKeywordTypeKind::TsNumberKeyword => Some(OatsType::Number),
            ast::TsKeywordTypeKind::TsVoidKeyword => Some(OatsType::Void),
            ast::TsKeywordTypeKind::TsBooleanKeyword => Some(OatsType::Boolean),
            ast::TsKeywordTypeKind::TsStringKeyword => Some(OatsType::String),
            _ => None,
        },
        ast::TsType::TsTypeRef(type_ref) => {
            // Check if this is a Promise<T> type
            if let Some(ident) = type_ref.type_name.as_ident() {
                if ident.sym.as_ref() == "Promise" {
                    // Extract the type parameter
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type(first_param)
                            .map(|inner| OatsType::Promise(Box::new(inner)));
                    }
                    // Promise without type parameter defaults to Promise<void>
                    return Some(OatsType::Promise(Box::new(OatsType::Void)));
                }
                // Otherwise, it's a nominal type like Foo -> map to NominalStruct("Foo")
                return Some(OatsType::NominalStruct(ident.sym.to_string()));
            }
            None
        }
        ast::TsType::TsArrayType(arr) => {
            // element type
            map_ts_type(&arr.elem_type).map(|elem| OatsType::Array(Box::new(elem)))
        }
        _ => None,
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

    // Collect param types
    let mut param_types = Vec::new();
    for param in &func_decl.params {
        match &param.pat {
            ast::Pat::Ident(ident) => {
                if let Some(type_ann) = &ident.type_ann {
                    let ast::TsTypeAnn {
                        type_ann: ts_type, ..
                    } = &**type_ann;
                    if let Some(mapped) = map_ts_type(ts_type) {
                        param_types.push(mapped);
                        continue;
                    }
                }
                return Err(anyhow::anyhow!(
                    "Function parameter missing or unsupported type annotation"
                ));
            }
            _ => {
                return Err(anyhow::anyhow!(
                    "Unsupported parameter pattern; only simple idents supported"
                ));
            }
        }
    }

    // Map return type
    let ret_ty = if let Some(rt) = &func_decl.return_type {
        let ast::TsTypeAnn {
            type_ann: ts_type, ..
        } = &**rt;
        if let Some(mapped) = map_ts_type(ts_type) {
            mapped
        } else {
            return Err(anyhow::anyhow!("Unsupported return type annotation"));
        }
    } else {
        return Err(anyhow::anyhow!("Function missing return type annotation"));
    };

    Ok(FunctionSig {
        params: param_types,
        ret: ret_ty,
    })
}

/// Infer an OatsType from an AST expression (literals, arrays, etc.)
/// Returns None if the expression type cannot be inferred.
pub fn infer_type_from_expr(expr: &ast::Expr) -> Option<OatsType> {
    match expr {
        ast::Expr::Lit(lit) => match lit {
            ast::Lit::Num(_) => Some(OatsType::Number),
            ast::Lit::Str(_) => Some(OatsType::String),
            ast::Lit::Bool(_) => Some(OatsType::Boolean),
            _ => None,
        },
        ast::Expr::Array(arr) => {
            // Infer array element type from first element
            if let Some(Some(first_elem)) = arr.elems.first() {
                infer_type_from_expr(&first_elem.expr)
                    .map(|elem_type| OatsType::Array(Box::new(elem_type)))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Comprehensive type inference that tries multiple sources:
/// 1. TypeScript type annotations
/// 2. Expression-based inference
/// 3. Fallback to default
pub fn infer_type(ts_type: Option<&ast::TsType>, expr: Option<&ast::Expr>) -> OatsType {
    // First priority: explicit TypeScript type annotation
    if let Some(ts_ty) = ts_type
        && let Some(oats_type) = map_ts_type(ts_ty) {
            return oats_type;
        }

    // Second priority: infer from expression
    if let Some(expr) = expr
        && let Some(oats_type) = infer_type_from_expr(expr) {
            return oats_type;
        }

    // Fallback: default to Number (most common type)
    OatsType::Number
}

impl OatsType {
    /// Check if this type is a Promise
    pub fn is_promise(&self) -> bool {
        matches!(self, OatsType::Promise(_))
    }

    /// Unwrap the inner type of a Promise, if this is a Promise type
    pub fn unwrap_promise_inner(&self) -> Option<&OatsType> {
        match self {
            OatsType::Promise(inner) => Some(inner),
            _ => None,
        }
    }

    /// Create a Promise wrapping the given type
    pub fn wrap_in_promise(inner: OatsType) -> Self {
        OatsType::Promise(Box::new(inner))
    }
}
