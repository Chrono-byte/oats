//! Representation of Oats (subset of TypeScript) types used by the compiler.
//!
//! `OatsType` is a compact enum used during parsing and codegen to convey
//! the static type information that the compiler currently understands. The
//! code generator maps these variants to LLVM ABI types and uses this
//! information to decide on boxing/unboxing, union representation, and
//! reference-counting behavior.

use anyhow::Result;
use deno_ast::swc::ast;
use std::collections::HashMap;

// Type alias for the locals stack used in codegen
pub type LocalsStack = Vec<HashMap<String, OatsType>>;

/// Oats type system (simplified subset of TypeScript used by the compiler)
///
/// Key variants and their meaning:
/// - `Number` -> numeric `f64` values.
/// - `Boolean` -> boolean values (represented as i1/i64 in places).
/// - `Union(Vec<OatsType>)` -> tagged unions. Codegen represents unions as
///   either `f64` (numeric-only unions) or `i8*` pointer slots when any arm
///   is pointer-like. Numeric arms can be boxed into runtime union objects
///   to fit pointer-like ABI slots.
/// - `Array(Box<OatsType>)` -> runtime array with element type metadata used
///   by helpers like `array_get_f64` / `array_get_ptr`.
/// - `Weak(T)` -> non-owning references (affects whether `rc_inc` or
///   `rc_weak_inc` is used when storing values).
/// - `Option(T)` -> nullable/optional values.
/// - `NominalStruct(name)` -> nominal class/struct identified by name.
/// - `StructLiteral(fields)` -> anonymous object shape inferred from an
///   object literal type; the emitter may register such shapes under a
///   generated nominal name when needed.
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
    // Fixed-size tuple type with ordered element types
    Tuple(Vec<OatsType>),
    NominalStruct(String),
    // Promise type wrapping a result type (e.g. Promise<number>)
    Promise(Box<OatsType>),
    // Anonymous struct-like type (object literal type) with named fields
    StructLiteral(Vec<(String, OatsType)>),
    // Generic type with type parameters
    Generic(Vec<OatsType>),
    // Enum type with name and variant names
    Enum(String, Vec<String>),
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
    /// Creates a new symbol table with an initial empty scope.
    ///
    /// The symbol table begins with a single scope representing the global
    /// namespace. Additional scopes can be pushed and popped to handle
    /// lexical scoping during compilation.
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    /// Retrieves a symbol from the symbol table using lexical scoping rules.
    ///
    /// This function searches through the scope stack from innermost to outermost,
    /// returning the first matching symbol found. This implements proper lexical
    /// scoping where inner scopes shadow outer scopes for symbols with the same name.
    ///
    /// # Arguments
    /// * `name` - Symbol name to look up
    ///
    /// # Returns
    /// The `OatsType` for the symbol if found, or `None` if not present
    pub fn get(&self, name: &str) -> Option<&OatsType> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        None
    }

    /// Inserts a symbol into the current (innermost) scope.
    ///
    /// This function adds a new symbol binding to the most recently created scope.
    /// If multiple scopes exist, the symbol is added to the innermost scope,
    /// allowing it to shadow symbols with the same name in outer scopes.
    ///
    /// # Arguments
    /// * `name` - Symbol name to bind
    /// * `ty` - Type information for the symbol
    pub fn insert(&mut self, name: String, ty: OatsType) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    /// Returns all symbols from all scopes as a flattened collection.
    ///
    /// This function collects symbols from all scopes in the table, which can
    /// be useful for debugging or analysis purposes. Note that symbols in inner
    /// scopes that shadow outer symbols will both be included in the result.
    ///
    /// # Returns
    /// A vector of (name, type) pairs for all symbols in the table
    pub fn all_symbols(&self) -> Vec<(String, OatsType)> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.iter().map(|(k, v)| (k.clone(), v.clone())))
            .collect()
    }
}

/// Maps a TypeScript AST type annotation to the corresponding Oats type system representation.
///
/// This function serves as the primary bridge between TypeScript's type system
/// and Oats' internal type representation. It handles the conversion of common
/// TypeScript types, generic parameters, and complex type constructs into the
/// unified `OatsType` enum used throughout the compiler.
///
/// # Arguments
/// * `ty` - TypeScript AST type node to convert
///
/// # Returns
/// The corresponding `OatsType` if the conversion is supported, or `None` for
/// unsupported or unrecognized TypeScript type constructs
///
/// # Supported Types
/// - Primitive types: `number`, `string`, `boolean`, `void`, `undefined`
/// - Generic types and type parameters
/// - Union types (converted to `OatsType::Union`)
/// - Array types with element type inference
/// - Function types with parameter and return type mapping
pub fn map_ts_type(ty: &ast::TsType) -> Option<OatsType> {
    match ty {
        ast::TsType::TsKeywordType(keyword) => match keyword.kind {
            ast::TsKeywordTypeKind::TsNumberKeyword => Some(OatsType::Number),
            ast::TsKeywordTypeKind::TsVoidKeyword => Some(OatsType::Void),
            ast::TsKeywordTypeKind::TsBooleanKeyword => Some(OatsType::Boolean),
            // `undefined` behaves similarly to `void` at the ABI level; we
            // represent it as `Void` here but special-case unions like
            // `T | undefined` to map to `Option<T>` below.
            ast::TsKeywordTypeKind::TsUndefinedKeyword => Some(OatsType::Void),
            ast::TsKeywordTypeKind::TsStringKeyword => Some(OatsType::String),
            _ => None,
        },
        ast::TsType::TsTypeRef(type_ref) => {
            if let Some(ident) = type_ref.type_name.as_ident() {
                // Check for generic classes or functions
                if ident.sym.as_ref() == "Generic"
                    && let Some(type_params) = &type_ref.type_params
                {
                    let mapped_params: Vec<_> = type_params
                        .params
                        .iter()
                        .filter_map(|param| map_ts_type(param))
                        .collect();
                    return Some(OatsType::Generic(mapped_params));
                }
                // Check if this is a Promise<T> type
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
                // Otherwise, it's a nominal type like Foo -> map to NominalStruct("Foo").
                // If the identifier is a bare type parameter (e.g. `T`) with no
                // type arguments we don't have a concrete mapping at this phase.
                // Return a placeholder `Generic` value so union/optional return
                // annotations like `T | undefined` can be accepted by the
                // strictness checker. Generic specialization happens later in
                // the pipeline.
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
                // If there are no explicit type parameters, assume a nominal
                // struct; otherwise, treat unknown bare idents as a placeholder
                // Generic so nested generic signatures can be represented.
                if type_ref.type_params.is_none() {
                    return Some(OatsType::NominalStruct(ident.sym.to_string()));
                }
                // Bare generic type parameter (e.g. `T`) -> placeholder
                return Some(OatsType::Generic(vec![]));
            }
            None
        }
        ast::TsType::TsUnionOrIntersectionType(ut) => {
            // SWC wraps unions and intersections in TsUnionOrIntersectionType;
            // try to extract union types specifically.
            if let ast::TsUnionOrIntersectionType::TsUnionType(un) = ut {
                // Special-case common pattern: `T | null` -> Option<T>
                if un.types.len() == 2 {
                    // Try to detect `null` or `undefined` in one of the union
                    // arms and map the other arm to an OatsType; if
                    // successful, return Option<that_type>.
                    let mut seen_nullish = false;
                    let mut other: Option<&ast::TsType> = None;
                    for tbox in &un.types {
                        let t = &**tbox;
                        if let ast::TsType::TsKeywordType(k) = t {
                            use deno_ast::swc::ast::TsKeywordTypeKind;
                            if matches!(k.kind, TsKeywordTypeKind::TsNullKeyword)
                                || matches!(k.kind, TsKeywordTypeKind::TsUndefinedKeyword)
                            {
                                seen_nullish = true;
                                continue;
                            }
                        }
                        other = Some(t);
                    }
                    if seen_nullish
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
        // Map tuple types like `[A, B]` to an array-of-union of the element types.
        // This is a pragmatic compatibility choice: tuples are lowered to runtime
        // arrays where each slot may contain any of the tuple element types.
        // It enables expressions like `t[0]` to work with existing array helpers.
        ast::TsType::TsTupleType(tuple) => {
            // Collect element types into a Tuple variant
            let mut elems: Vec<OatsType> = Vec::new();
            for elem in &tuple.elem_types {
                if let Some(mapped) = map_ts_type(&elem.ty) {
                    elems.push(mapped);
                } else {
                    return None;
                }
            }
            if elems.is_empty() {
                return None;
            }
            Some(OatsType::Tuple(elems))
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

/// Map a TypeScript AST type to an OatsType while applying a substitution
/// map for type-parameters. The `subst` map maps type-parameter identifier
/// names (e.g. "T") to concrete `OatsType` values. When a TsTypeRef refers
/// to a name present in `subst` it will be replaced with the mapped type.
pub fn map_ts_type_with_subst(
    ty: &ast::TsType,
    subst: &std::collections::HashMap<String, OatsType>,
) -> Option<OatsType> {
    use deno_ast::swc::ast;
    match ty {
        ast::TsType::TsKeywordType(_) => map_ts_type(ty),
        ast::TsType::TsTypeRef(type_ref) => {
            if let Some(ident) = type_ref.type_name.as_ident() {
                let name = ident.sym.to_string();
                // If this ident is a substituted type-parameter, return it
                if let Some(mapped) = subst.get(&name) {
                    return Some(mapped.clone());
                }
                // Otherwise fall back to existing map rules for known generics
                // and nominal types. If there are type arguments, map them
                // recursively using the same substitution map.
                if ident.sym.as_ref() == "Promise" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst(first_param, subst)
                            .map(|inner| OatsType::Promise(Box::new(inner)));
                    }
                    return Some(OatsType::Promise(Box::new(OatsType::Void)));
                }
                if ident.sym.as_ref() == "Array" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst(first_param, subst)
                            .map(|inner| OatsType::Array(Box::new(inner)));
                    }
                    return Some(OatsType::Array(Box::new(OatsType::Number)));
                }
                if ident.sym.as_ref() == "Weak" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst(first_param, subst)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_ref() == "Option" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst(first_param, subst)
                            .map(|inner| OatsType::Option(Box::new(inner)));
                    }
                    return None;
                }

                // If there are no explicit type parameters, assume a nominal
                // struct; if there are type parameters but the ident isn't a
                // special generic we fallback to a placeholder Generic so
                // downstream phases can represent nested generics.
                if type_ref.type_params.is_none() {
                    return Some(OatsType::NominalStruct(name));
                }
                return Some(OatsType::Generic(vec![]));
            }
            None
        }
        ast::TsType::TsUnionOrIntersectionType(ut) => {
            // Handle union types like `T | undefined` by applying substitution
            // to each arm. This mirrors `map_ts_type` but uses the provided
            // `subst` map so named type parameters are replaced.
            if let ast::TsUnionOrIntersectionType::TsUnionType(un) = ut {
                // Special-case `T | null` or `T | undefined` -> Option<T>
                if un.types.len() == 2 {
                    let mut seen_nullish = false;
                    let mut other: Option<&ast::TsType> = None;
                    for tbox in &un.types {
                        let t = &**tbox;
                        if let ast::TsType::TsKeywordType(k) = t {
                            use deno_ast::swc::ast::TsKeywordTypeKind;
                            if matches!(k.kind, TsKeywordTypeKind::TsNullKeyword)
                                || matches!(k.kind, TsKeywordTypeKind::TsUndefinedKeyword)
                            {
                                seen_nullish = true;
                                continue;
                            }
                        }
                        other = Some(t);
                    }
                    if seen_nullish && let Some(o) = other {
                        if let Some(mapped) = map_ts_type_with_subst(o, subst) {
                            return Some(OatsType::Option(Box::new(mapped)));
                        }
                        return None;
                    }
                }

                // General union: map each part with substitution
                let mut parts = Vec::new();
                for t in &un.types {
                    if let Some(mapped) = map_ts_type_with_subst(t, subst) {
                        parts.push(mapped);
                    } else {
                        return None;
                    }
                }
                return Some(OatsType::Union(parts));
            }
            None
        }
        ast::TsType::TsArrayType(arr) => map_ts_type_with_subst(&arr.elem_type, subst)
            .map(|elem| OatsType::Array(Box::new(elem))),
        ast::TsType::TsTupleType(tuple) => {
            let mut elems: Vec<OatsType> = Vec::new();
            for elem in &tuple.elem_types {
                if let Some(mapped) = map_ts_type_with_subst(&elem.ty, subst) {
                    elems.push(mapped);
                } else {
                    return None;
                }
            }
            if elems.is_empty() {
                return None;
            }
            Some(OatsType::Tuple(elems))
        }
        ast::TsType::TsTypeLit(typelit) => {
            let mut fields: Vec<(String, OatsType)> = Vec::new();
            for member in &typelit.members {
                if let ast::TsTypeElement::TsPropertySignature(prop) = member
                    && let ast::Expr::Ident(id) = &*prop.key
                {
                    let fname = id.sym.to_string();
                    if let Some(type_ann) = &prop.type_ann
                        && let Some(mapped) = map_ts_type_with_subst(&type_ann.type_ann, subst)
                    {
                        fields.push((fname, mapped));
                        continue;
                    }
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
    // Return type annotation required for regular functions, but for arrows, we infer
    let ret_type = if let Some(return_type) = &func_decl.return_type {
        if let Some(mapped) = map_ts_type(&return_type.type_ann) {
            mapped
        } else {
            return Err(anyhow::anyhow!("Function return type not supported"));
        }
    } else {
        // Assume Number if no annotation (for arrows)
        crate::types::OatsType::Number
    };

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

    Ok(FunctionSig {
        params: param_types,
        ret: ret_type,
    })
}

/// Infers an Oats type from AST expression patterns and literal values.
///
/// This function provides type inference for expressions where explicit type
/// annotations are not present. It analyzes literal values, array construction
/// patterns, and other expression forms to determine the most appropriate type
/// representation in the Oats type system.
///
/// # Arguments
/// * `expr` - AST expression to analyze for type inference
///
/// # Returns
/// The inferred `OatsType` if successful, or `None` if the expression type
/// cannot be determined through static analysis
///
/// # Inference Strategy
/// - **Literals**: Direct mapping from literal type to `OatsType`
/// - **Arrays**: Element type inferred from first non-null element
/// - **Complex expressions**: Currently unsupported, returns `None`
pub fn infer_type_from_expr(expr: &ast::Expr) -> Option<OatsType> {
    match expr {
        ast::Expr::Lit(lit) => match lit {
            ast::Lit::Num(_) => Some(OatsType::Number),
            ast::Lit::Str(_) => Some(OatsType::String),
            ast::Lit::Bool(_) => Some(OatsType::Boolean),
            _ => None,
        },
        ast::Expr::Array(arr) => {
            // Infer array element type from first non-null element
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

/// Performs comprehensive type inference using multiple information sources.
///
/// This function implements a fallback hierarchy for type inference, attempting
/// to determine the most accurate type representation by consulting TypeScript
/// type annotations first, then expression-based inference, and finally falling
/// back to conservative defaults.
///
/// # Arguments
/// * `ts_type` - Optional TypeScript type annotation
/// * `expr` - Optional expression for inference
///
/// # Returns
/// The most specific `OatsType` that can be determined from available information
///
/// # Inference Priority
/// 1. **TypeScript annotations**: Explicit type information (highest priority)
/// 2. **Expression inference**: Types derived from literal values and patterns
/// 3. **Generic fallback**: Conservative `Generic` type when inference fails
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

    // Fallback: default to Number (most conservative common type)
    OatsType::Number
}

/// Applies inferred type substitutions to resolve generic placeholders.
///
/// This function performs a simple heuristic-based substitution for generic
/// types that could not be resolved through explicit type parameter mapping.
/// It serves as a last-resort mechanism during monomorphization to ensure
/// that all generic placeholders are replaced with concrete types.
///
/// # Arguments
/// * `ty` - Type that may contain generic placeholders
/// * `inferred` - Array of inferred concrete types for substitution
///
/// # Returns
/// A new `OatsType` with generic placeholders replaced by inferred types
///
/// # Substitution Strategy
/// - **Generic placeholders**: Replaced with the first inferred type
/// - **Optional generics**: Wrapped in `Option` with the inferred type
/// - **Array generics**: Element type is recursively substituted
/// - **Concrete types**: Returned unchanged
///
/// # Fallback Behavior
/// When no inferred types are available, generic placeholders default to
/// `Number` type as the most conservative choice for the Oats type system.
pub fn apply_inferred_subst(ty: &OatsType, inferred: &[OatsType]) -> OatsType {
    match ty {
        OatsType::Generic(_) => {
            if !inferred.is_empty() {
                inferred[0].clone()
            } else {
                OatsType::Number
            }
        }
        OatsType::Option(inner) => match &**inner {
            OatsType::Generic(_) => {
                if !inferred.is_empty() {
                    OatsType::Option(Box::new(inferred[0].clone()))
                } else {
                    OatsType::Option(Box::new(OatsType::Number))
                }
            }
            other => OatsType::Option(Box::new(apply_inferred_subst(other, inferred))),
        },
        OatsType::Array(inner) => OatsType::Array(Box::new(apply_inferred_subst(inner, inferred))),
        OatsType::Union(parts) => {
            let new_parts = parts
                .iter()
                .map(|p| apply_inferred_subst(p, inferred))
                .collect();
            OatsType::Union(new_parts)
        }
        OatsType::Tuple(elems) => OatsType::Tuple(
            elems
                .iter()
                .map(|e| apply_inferred_subst(e, inferred))
                .collect(),
        ),
        OatsType::Weak(inner) => OatsType::Weak(Box::new(apply_inferred_subst(inner, inferred))),
        OatsType::Promise(inner) => {
            OatsType::Promise(Box::new(apply_inferred_subst(inner, inferred)))
        }
        OatsType::StructLiteral(fields) => OatsType::StructLiteral(
            fields
                .iter()
                .map(|(n, t)| (n.clone(), apply_inferred_subst(t, inferred)))
                .collect(),
        ),
        other => other.clone(),
    }
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
