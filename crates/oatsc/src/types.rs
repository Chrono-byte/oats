//! Representation of Oats types used by the compiler.
//!
//! `OatsType` is a compact enum used during parsing and codegen to convey
//! the static type information that the compiler currently understands. The
//! code generator maps these variants to LLVM ABI types and uses this
//! information to decide on boxing/unboxing, union representation, and
//! reference-counting behavior.

use crate::diagnostics::{Diagnostic, Label, Span};
use anyhow::Result;
use oats_ast::*;
use std::collections::HashMap;

// Type alias for the locals stack used in codegen
pub type LocalsStack = Vec<HashMap<String, OatsType>>;

/// Oats type system (used by the compiler)
///
/// Key variants and their meaning:
/// - `Number` -> numeric `f64` values.
/// - `Boolean` -> boolean values (represented as i1/i64 in places).
/// - `I8`, `U8`, `I16`, `U16`, `I32`, `U32`, `I64`, `U64` -> signed/unsigned integer types.
/// - `F32`, `F64` -> floating point types.
/// - `Isize`, `Usize` -> architecture-sized signed/unsigned integer types.
/// - `Char` -> Unicode scalar value.
/// - `Union(Vec<OatsType>)` -> tagged unions. Codegen represents unions as
///   either `f64` (numeric-only unions) or `i8*` pointer slots when any arm
///   is pointer-like. Numeric arms can be boxed into runtime union objects
///   to fit pointer-like ABI slots.
/// - `Array(Box<OatsType>)` -> runtime array with element type metadata used
///   by helpers like `array_get_f64` / `array_get_ptr`.
/// - `Weak(T)` -> non-owning references (affects whether `rc_inc` or
///   `rc_weak_inc` is used when storing values).
/// - `Unowned(T)` -> non-owning, non-zeroing references that do not perform
///   any RC operations (use only when lifetimes are guaranteed).
/// - `Option(T)` -> nullable/optional values.
/// - `NominalStruct(name)` -> nominal class/struct identified by name.
/// - `StructLiteral(fields)` -> anonymous object shape inferred from an
///   object literal type; the emitter may register such shapes under a
///   generated nominal name when needed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OatsType {
    // Legacy number type (maps to f64 for backward compatibility)
    Number,
    // Boolean type
    Boolean,
    // Integer types
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    // Architecture-sized types
    Isize,
    Usize,
    // Floating point types
    F32,
    F64,
    // Character type
    Char,
    // Union of multiple possible types (e.g. number | string)
    Union(Vec<OatsType>),
    // Array of element type (e.g. number[])
    Array(Box<OatsType>),
    // Weak reference wrapper (non-owning)
    Weak(Box<OatsType>),
    // Unowned reference wrapper (non-owning, no RC operations)
    Unowned(Box<OatsType>),
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
    // Generic type instance with concrete type arguments
    GenericInstance {
        base_name: String,
        type_args: Vec<OatsType>,
    },
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Variable {
        ty: OatsType,
    },
    StdFunction {
        mangled_name: String,
        sig: FunctionSig,
    },
}

#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub type_params: Vec<String>,
    pub params: Vec<OatsType>,
    pub ret: OatsType,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
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
    /// The `Symbol` if found, or `None` if not present
    pub fn get(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    /// Retrieves the type of a variable symbol from the symbol table.
    ///
    /// This is a convenience method for getting the OatsType of a variable.
    /// For std functions, this returns None.
    ///
    /// # Arguments
    /// * `name` - Symbol name to look up
    ///
    /// # Returns
    /// The `OatsType` for the symbol if it's a variable, or `None` otherwise
    pub fn get_type(&self, name: &str) -> Option<&OatsType> {
        self.get(name).and_then(|symbol| match symbol {
            Symbol::Variable { ty } => Some(ty),
            Symbol::StdFunction { .. } => None,
        })
    }

    /// Inserts a symbol into the current (innermost) scope.
    ///
    /// This function adds a new symbol binding to the most recently created scope.
    /// If multiple scopes exist, the symbol is added to the innermost scope,
    /// allowing it to shadow symbols with the same name in outer scopes.
    ///
    /// # Arguments
    /// * `name` - Symbol name to bind
    /// * `symbol` - Symbol information
    pub fn insert(&mut self, name: String, symbol: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, symbol);
        }
    }

    /// Inserts a variable symbol into the current (innermost) scope.
    ///
    /// # Arguments
    /// * `name` - Symbol name to bind
    /// * `ty` - Type information for the variable
    pub fn insert_variable(&mut self, name: String, ty: OatsType) {
        self.insert(name, Symbol::Variable { ty });
    }

    /// Inserts a std function symbol into the current (innermost) scope.
    ///
    /// # Arguments
    /// * `name` - Symbol name to bind
    /// * `mangled_name` - Mangled external symbol name
    /// * `sig` - Function signature
    pub fn insert_std_function(&mut self, name: String, mangled_name: String, sig: FunctionSig) {
        self.insert(name, Symbol::StdFunction { mangled_name, sig });
    }

    /// Returns all symbols from all scopes as a flattened collection.
    ///
    /// This function collects symbols from all scopes in the table, which can
    /// be useful for debugging or analysis purposes. Note that symbols in inner
    /// scopes that shadow outer symbols will both be included in the result.
    ///
    /// # Returns
    /// A vector of (name, symbol) pairs for all symbols in the table
    pub fn all_symbols(&self) -> Vec<(String, Symbol)> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.iter().map(|(k, v)| (k.clone(), v.clone())))
            .collect()
    }
}

/// Maps an Oats AST type annotation to the corresponding Oats type system representation.
///
/// This function serves as the primary bridge between the Oats AST type system
/// and Oats' internal type representation. It handles the conversion of common
/// Oats types, generic parameters, and complex type constructs into the
/// unified `OatsType` enum used throughout the compiler.
///
/// # Arguments
/// * `ty` - Oats AST type node to convert
///
/// # Returns
/// The corresponding `OatsType` if the conversion is supported, or `None` for
/// unsupported or unrecognized Oats type constructs
///
/// # Supported Types
/// - Primitive types: `number`, `string`, `boolean`, `void`, `undefined`
/// - Generic types and type parameters
/// - Union types (converted to `OatsType::Union`)
/// - Array types with element type inference
/// - Function types with parameter and return type mapping
///
/// Helper to get span from a TsType
fn ts_type_span(ty: &TsType) -> oats_ast::Span {
    match ty {
        TsType::TsKeywordType(_) => 0..0, // Keyword types don't have spans in oats_ast
        TsType::TsTypeRef(tr) => tr.span.clone(),
        TsType::TsArrayType(at) => at.span.clone(),
        TsType::TsUnionType(ut) => ut.span.clone(),
        TsType::TsIntersectionType(it) => it.span.clone(),
        TsType::TsFunctionType(ft) => ft.span.clone(),
        TsType::TsTupleType(tt) => tt.span.clone(),
    }
}

pub fn map_ts_type(ty: &TsType) -> Option<OatsType> {
    match ty {
        TsType::TsKeywordType(keyword) => match keyword {
            TsKeywordType::TsNumberKeyword => Some(OatsType::Number),
            TsKeywordType::TsVoidKeyword => Some(OatsType::Void),
            TsKeywordType::TsBooleanKeyword => Some(OatsType::Boolean),
            TsKeywordType::TsStringKeyword => Some(OatsType::String),
        },
        TsType::TsTypeRef(type_ref) => {
            let TsEntityName::Ident(ident) = &type_ref.type_name;
            {
                // Check for primitive types
                match ident.sym.as_ref() {
                    "i8" => return Some(OatsType::I8),
                    "u8" => return Some(OatsType::U8),
                    "i16" => return Some(OatsType::I16),
                    "u16" => return Some(OatsType::U16),
                    "i32" => return Some(OatsType::I32),
                    "u32" => return Some(OatsType::U32),
                    "i64" => return Some(OatsType::I64),
                    "u64" => return Some(OatsType::U64),
                    "isize" => return Some(OatsType::Isize),
                    "usize" => return Some(OatsType::Usize),
                    "f32" => return Some(OatsType::F32),
                    "f64" => return Some(OatsType::F64),
                    "char" => return Some(OatsType::Char),
                    _ => {}
                }
                // Check for generic classes or functions
                if ident.sym.as_str() == "Generic"
                    && let Some(type_params) = &type_ref.type_params
                {
                    let mapped_params: Vec<_> =
                        type_params.params.iter().filter_map(map_ts_type).collect();
                    return Some(OatsType::Generic(mapped_params));
                }
                // Check if this is a Promise<T> type
                if ident.sym.as_str() == "Promise" {
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
                if ident.sym.as_str() == "Array" {
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
                if ident.sym.as_str() == "Weak" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type(first_param)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_str() == "Unowned" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type(first_param)
                            .map(|inner| OatsType::Unowned(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_str() == "Option" {
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
                Some(OatsType::Generic(vec![]))
            }
        }
        TsType::TsUnionType(ut) => {
            // Special-case common pattern: `T | null` -> Option<T>
            if ut.types.len() == 2 {
                // Try to detect `null` or `undefined` in one of the union
                // arms and map the other arm to an OatsType; if
                // successful, return Option<that_type>.
                let seen_nullish = false;
                let mut other: Option<&TsType> = None;
                for t in &ut.types {
                    if matches!(t, TsType::TsKeywordType(_)) {
                        // Note: oats_ast doesn't have null/undefined keywords yet
                        // This will need to be updated when those are added
                        // For now, skip this optimization
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
            for t in &ut.types {
                if let Some(mapped) = map_ts_type(t) {
                    parts.push(mapped);
                } else {
                    return None;
                }
            }
            Some(OatsType::Union(parts))
        }
        TsType::TsArrayType(arr) => {
            // element type
            map_ts_type(&arr.elem_type).map(|elem| OatsType::Array(Box::new(elem)))
        }
        // Map tuple types like `[A, B]` to an array-of-union of the element types.
        // This is a pragmatic compatibility choice: tuples are lowered to runtime
        // arrays where each slot may contain any of the tuple element types.
        // It enables expressions like `t[0]` to work with existing array helpers.
        TsType::TsTupleType(tuple) => {
            // Collect element types into a Tuple variant
            let mut elems: Vec<OatsType> = Vec::new();
            for elem in &tuple.elem_types {
                if let Some(mapped) = map_ts_type(elem) {
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
        // Object literal types (TsTypeLit) are not yet supported in oats_ast.
        // When TsTypeLit is added to oats_ast, implement mapping here:
        // TsType::TsTypeLit(typelit) => {
        //     // Object literal type: collect property signatures
        //     // Map to StructLiteral variant with field names and types
        //     // Example: { x: number, y: string } -> StructLiteral(vec![("x".to_string(), OatsType::Number), ("y".to_string(), OatsType::String)])
        //     None
        // }
        // Note: Object literal types can be inferred from expressions (Expr::Object)
        // and are already supported via StructLiteral in that context.
        _ => None,
    }
}

/// Map an Oats AST type to an OatsType while applying a substitution
/// map for type-parameters. The `subst` map maps type-parameter identifier
/// names (e.g. "T") to concrete `OatsType` values. When a TsTypeRef refers
/// to a name present in `subst` it will be replaced with the mapped type.
pub fn map_ts_type_with_subst(
    ty: &TsType,
    subst: &std::collections::HashMap<String, OatsType>,
) -> Option<OatsType> {
    match ty {
        TsType::TsKeywordType(_) => map_ts_type(ty),
        TsType::TsTypeRef(type_ref) => {
            let TsEntityName::Ident(ident) = &type_ref.type_name;
            {
                let name = ident.sym.clone();
                // If this ident is a substituted type-parameter, return it
                if let Some(mapped) = subst.get(&name) {
                    return Some(mapped.clone());
                }
                // Otherwise fall back to existing map rules for known generics
                // and nominal types. If there are type arguments, map them
                // recursively using the same substitution map.
                if ident.sym.as_str() == "Promise" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst(first_param, subst)
                            .map(|inner| OatsType::Promise(Box::new(inner)));
                    }
                    return Some(OatsType::Promise(Box::new(OatsType::Void)));
                }
                if ident.sym.as_str() == "Array" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst(first_param, subst)
                            .map(|inner| OatsType::Array(Box::new(inner)));
                    }
                    return Some(OatsType::Array(Box::new(OatsType::Number)));
                }
                if ident.sym.as_str() == "Weak" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst(first_param, subst)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_str() == "Option" {
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
                    Some(OatsType::NominalStruct(name))
                } else {
                    Some(OatsType::Generic(vec![]))
                }
            }
        }
        TsType::TsUnionType(un) => {
            // Handle union types like `T | undefined` by applying substitution
            // to each arm. This mirrors `map_ts_type` but uses the provided
            // `subst` map so named type parameters are replaced.
            {
                // Special-case `T | null` or `T | undefined` -> Option<T>
                if un.types.len() == 2 {
                    let mut seen_nullish = false;
                    let mut other: Option<&TsType> = None;
                    for t in &un.types {
                        if matches!(t, TsType::TsKeywordType(_)) {
                            // Note: oats_ast doesn't have TsNullKeyword or TsUndefinedKeyword yet
                            // For now, skip null/undefined handling
                            if false {
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
                Some(OatsType::Union(parts))
            }
        }
        TsType::TsArrayType(arr) => map_ts_type_with_subst(&arr.elem_type, subst)
            .map(|elem| OatsType::Array(Box::new(elem))),
        TsType::TsTupleType(tuple) => {
            let mut elems: Vec<OatsType> = Vec::new();
            for elem in &tuple.elem_types {
                if let Some(mapped) = map_ts_type_with_subst(elem, subst) {
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
        // Note: TsTypeLit doesn't exist in oats_ast, use TsTypeRef instead
        // TsTypeRef is already handled above, so this pattern is unreachable
        _ => {
            // Placeholder for other types
            None
        }
    }
}

pub fn check_function_strictness(
    func_decl: &Function,
    _symbols: &mut SymbolTable,
) -> Result<(Option<FunctionSig>, Vec<Diagnostic>)> {
    let mut diagnostics = Vec::new();

    // Return type annotation required for regular functions, but for arrows, we infer
    let ret_type = if let Some(return_type) = &func_decl.return_type {
        if let Some(mapped) = map_ts_type(return_type) {
            mapped
        } else {
            diagnostics.push(
                Diagnostic::error("Unsupported return type")
                    .with_code("E1001")
                    .with_label(Label {
                        span: {
                            let r = ts_type_span(return_type);
                            Span {
                                start: r.start,
                                end: r.end,
                            }
                        },
                        message: "This return type is not supported".into(),
                    }),
            );
            return Ok((None, diagnostics));
        }
    } else {
        diagnostics.push(
            Diagnostic::error("Missing return type annotation")
                .with_code("E1002")
                .with_label(Label {
                    span: Span {
                        start: func_decl.span.start,
                        end: func_decl.span.end,
                    },
                    message: "Function return type annotation is required".into(),
                }),
        );
        return Ok((None, diagnostics));
    };

    // Collect param types
    let mut param_types = Vec::new();
    for param in &func_decl.params {
        match &param.pat {
            Pat::Ident(_ident) => {
                if let Some(ty) = &param.ty
                    && let Some(mapped) = map_ts_type(ty)
                {
                    param_types.push(mapped);
                    continue;
                }
                // If no type annotation, default to Number for now
                param_types.push(OatsType::Number);
            } // oats_ast only supports Ident patterns for parameters
            Pat::Array(_) => {
                param_types.push(OatsType::Array(Box::new(OatsType::Number)));
            }
            Pat::Object(_) => {
                param_types.push(OatsType::StructLiteral(Vec::new()));
            }
            Pat::Rest(_) => {
                param_types.push(OatsType::Array(Box::new(OatsType::Number)));
            }
        }
    }

    Ok((
        Some(FunctionSig {
            type_params: Vec::new(),
            params: param_types,
            ret: ret_type,
        }),
        diagnostics,
    ))
}

/// - **Literals**: Direct mapping from literal type to `OatsType`
/// - **Arrays**: Element type inferred from first non-null element
/// - **Complex expressions**: Currently unsupported, returns `None`
pub fn infer_type_from_expr(expr: &Expr) -> Option<OatsType> {
    match expr {
        Expr::Lit(lit) => match lit {
            Lit::F64(_)
            | Lit::F32(_)
            | Lit::I8(_)
            | Lit::I16(_)
            | Lit::I32(_)
            | Lit::I64(_)
            | Lit::I128(_)
            | Lit::ISize(_)
            | Lit::U8(_)
            | Lit::U16(_)
            | Lit::U32(_)
            | Lit::U64(_)
            | Lit::U128(_)
            | Lit::USize(_) => Some(OatsType::Number),
            Lit::Str(_) => Some(OatsType::String),
            Lit::Bool(_) => Some(OatsType::Boolean),
            _ => None,
        },
        Expr::Array(arr) => {
            // Infer array element type from first non-null element
            if let Some(Some(first_elem)) = arr.elems.first() {
                infer_type_from_expr(first_elem)
                    .map(|elem_type| OatsType::Array(Box::new(elem_type)))
            } else {
                None
            }
        }
        Expr::Object(obj) => {
            // Infer object literal type
            let mut fields = Vec::new();
            for prop in &obj.props {
                if let PropOrSpread::Prop(prop) = prop
                    && let Prop::KeyValue(kv) = prop
                    && let PropName::Ident(ident) = &kv.key
                    && let Some(field_type) = infer_type_from_expr(&kv.value)
                {
                    fields.push((ident.sym.clone(), field_type));
                }
            }
            if !fields.is_empty() {
                Some(OatsType::StructLiteral(fields))
            } else {
                None
            }
        }
        Expr::Call(_call) => {
            // For function calls, we can't infer the return type without knowing the function
            // This would require a more sophisticated analysis
            None
        }
        Expr::Member(_member) => {
            // For property access, we can't infer the type without knowing the object
            None
        }
        Expr::Unary(unary) => {
            // Unary operations generally preserve the operand type
            infer_type_from_expr(&unary.arg)
        }
        Expr::Bin(bin) => {
            // Binary operations: most return numbers, but some might return other types
            match bin.op {
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq => {
                    // Comparison operations return boolean
                    Some(OatsType::Boolean)
                }
                _ => {
                    // Arithmetic operations return the type of the operands (usually number)
                    infer_type_from_expr(&bin.left).or_else(|| infer_type_from_expr(&bin.right))
                }
            }
        }
        Expr::Paren(paren) => {
            // Parenthesized expressions have the same type as their contents
            infer_type_from_expr(&paren.expr)
        }
        Expr::Tpl(_tpl) => {
            // Template literals are strings
            Some(OatsType::String)
        }
        Expr::New(new_expr) => {
            // For new ClassName(...), infer NominalStruct("ClassName")
            if let Expr::Ident(ident) = &*new_expr.callee {
                Some(OatsType::NominalStruct(ident.sym.to_string()))
            } else {
                None
            }
        }
        Expr::Cond(cond) => {
            // For conditional expressions, infer from cons and alt if they have the same type
            let cons_type = infer_type_from_expr(&cond.cons);
            let alt_type = infer_type_from_expr(&cond.alt);
            if cons_type == alt_type {
                cons_type
            } else {
                None
            }
        }
        Expr::Await(await_expr) => {
            // For await expressions, infer the unwrapped type if it's a promise
            if let Some(inner) = infer_type_from_expr(&await_expr.arg) {
                if let OatsType::Promise(boxed) = inner {
                    Some(*boxed)
                } else {
                    Some(inner)
                }
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
/// to determine the most accurate type representation by consulting Oats
/// type annotations first, then expression-based inference, and finally falling
/// back to conservative defaults.
///
/// # Arguments
/// * `ts_type` - Optional Oats type annotation
/// * `expr` - Optional expression for inference
///
/// # Returns
/// The most specific `OatsType` that can be determined from available information
///
/// # Inference Priority
/// 1. **Oats type annotations**: Explicit type information (highest priority)
/// 2. **Expression inference**: Types derived from literal values and patterns
/// 3. **Generic fallback**: Conservative `Generic` type when inference fails
pub fn infer_type(ts_type: Option<&TsType>, expr: Option<&Expr>) -> OatsType {
    // First priority: explicit Oats type annotation
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
        OatsType::GenericInstance {
            base_name,
            type_args,
        } => OatsType::GenericInstance {
            base_name: base_name.clone(),
            type_args: type_args
                .iter()
                .map(|t| apply_inferred_subst(t, inferred))
                .collect(),
        },
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
