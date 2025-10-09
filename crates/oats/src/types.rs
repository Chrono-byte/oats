use anyhow::Result;
use deno_ast::swc::ast;
use std::collections::HashMap;

// Type alias for the locals stack used in codegen
pub type LocalsStack = Vec<HashMap<String, OatsType>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OatsType {
    Number,
    Boolean,
    // Union of multiple possible types (e.g. number | string)
    Union(Vec<OatsType>),
    // Array of element type (e.g. number[])
    Array(Box<OatsType>),
    // Weak reference wrapper (non-owning)
    Weak(Box<OatsType>),
    // Optional wrapper, represented as nullable/Option
    Option(Box<OatsType>),
    Void,
    String,
    NominalStruct(String),
    // Promise type wrapping a result type (e.g. Promise<number>)
    Promise(Box<OatsType>),
    // Anonymous struct-like type (object literal type) with named fields
    StructLiteral(Vec<(String, OatsType)>),
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

// Map a TypeScript AST type to an OatsType.
// Returns None if the type is not supported.
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
                // Support generic Array<T> written as Array<T>
                if ident.sym.as_ref() == "Array" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type(first_param)
                            .map(|inner| OatsType::Array(Box::new(inner)));
                    }
                    // No type param -> default to Array<number>
                    return Some(OatsType::Array(Box::new(OatsType::Number)));
                }
                // Support Weak<T> and Option<T> as language-level generics
                if ident.sym.as_ref() == "Weak" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type(first_param)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_ref() == "Option" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type(first_param)
                            .map(|inner| OatsType::Option(Box::new(inner)));
                    }
                    return None;
                }
                return Some(OatsType::NominalStruct(ident.sym.to_string()));
            }
            None
        }
        ast::TsType::TsUnionOrIntersectionType(ut) => {
            // SWC wraps unions and intersections in TsUnionOrIntersectionType;
            // try to extract union types specifically.
            if let ast::TsUnionOrIntersectionType::TsUnionType(un) = ut {
                // Special-case common pattern: `T | null` -> Option<T>
                if un.types.len() == 2 {
                    // Try to detect `null` in one of the union arms and map the other
                    // arm to an OatsType; if successful, return Option<that_type>.
                    let mut seen_null = false;
                    let mut other: Option<&ast::TsType> = None;
                    for tbox in &un.types {
                        let t = &**tbox;
                        if let ast::TsType::TsKeywordType(k) = t {
                            use deno_ast::swc::ast::TsKeywordTypeKind;
                            if matches!(k.kind, TsKeywordTypeKind::TsNullKeyword) {
                                seen_null = true;
                                continue;
                            }
                        }
                        other = Some(t);
                    }
                    if seen_null
                        && let Some(o) = other
                        && let Some(mapped) = map_ts_type(o)
                    {
                        return Some(OatsType::Option(Box::new(mapped)));
                    }
                }

                let mut parts = Vec::new();
                for t in &un.types {
                    if let Some(mapped) = map_ts_type(t) {
                        parts.push(mapped);
                    } else {
                        return None;
                    }
                }
                return Some(OatsType::Union(parts));
            }
            None
        }
        ast::TsType::TsArrayType(arr) => {
            // element type
            map_ts_type(&arr.elem_type).map(|elem| OatsType::Array(Box::new(elem)))
        }
        ast::TsType::TsTypeLit(typelit) => {
            // Object literal type: collect property signatures where possible
            use deno_ast::swc::ast;
            let mut fields: Vec<(String, OatsType)> = Vec::new();
            for member in &typelit.members {
                if let ast::TsTypeElement::TsPropertySignature(prop) = member
                    && let ast::Expr::Ident(id) = &*prop.key
                {
                    let fname = id.sym.to_string();
                    if let Some(type_ann) = &prop.type_ann
                        && let Some(mapped) = map_ts_type(&type_ann.type_ann)
                    {
                        fields.push((fname, mapped));
                        continue;
                    }
                    // default when type can't be mapped
                    fields.push((fname, OatsType::Number));
                }
            }
            Some(OatsType::StructLiteral(fields))
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

// Infer an OatsType from an AST expression (literals, arrays, etc.)
// Returns None if the expression type cannot be inferred.
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

// Comprehensive type inference that tries multiple sources:
// 1. TypeScript type annotations
// 2. Expression-based inference
// 3. Fallback to default
pub fn infer_type(ts_type: Option<&ast::TsType>, expr: Option<&ast::Expr>) -> OatsType {
    // First priority: explicit TypeScript type annotation
    if let Some(ts_ty) = ts_type
        && let Some(oats_type) = map_ts_type(ts_ty)
    {
        return oats_type;
    }

    // Second priority: infer from expression
    if let Some(expr) = expr
        && let Some(oats_type) = infer_type_from_expr(expr)
    {
        return oats_type;
    }

    // Fallback: default to Number (most common type)
    OatsType::Number
}

impl OatsType {
    // Check if this type is a Promise
    pub fn is_promise(&self) -> bool {
        matches!(self, OatsType::Promise(_))
    }

    // Unwrap the inner type of a Promise, if this is a Promise type
    pub fn unwrap_promise_inner(&self) -> Option<&OatsType> {
        match self {
            OatsType::Promise(inner) => Some(inner),
            _ => None,
        }
    }

    // Create a Promise wrapping the given type
    pub fn wrap_in_promise(inner: OatsType) -> Self {
        OatsType::Promise(Box::new(inner))
    }
}
