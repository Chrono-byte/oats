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

// Type alias for type alias information to reduce complexity warnings
type TypeAliasMap = HashMap<String, (Option<Vec<String>>, oats_ast::TsType)>;

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
    // Process-related types
    ProcessId,  // Process identifier (u64)
    MonitorRef, // Monitor reference (u64)
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
        TsType::TsTypeLit(tl) => tl.span.clone(),
    }
}

pub fn map_ts_type(ty: &TsType) -> Option<OatsType> {
    map_ts_type_with_aliases(ty, None)
}

/// Maps an Oats AST type annotation to OatsType, with optional type alias resolution.
///
/// If `type_aliases` is provided, type references will be resolved through the alias map.
/// For generic type aliases, type arguments are substituted into the aliased type.
pub fn map_ts_type_with_aliases(
    ty: &TsType,
    type_aliases: Option<&TypeAliasMap>,
) -> Option<OatsType> {
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
                    // Process-related types
                    "ProcessId" => return Some(OatsType::ProcessId),
                    "MonitorRef" => return Some(OatsType::MonitorRef),
                    _ => {}
                }

                // Check for type aliases before falling back to built-in types
                if let Some(aliases) = type_aliases
                    && let Some((alias_type_params, aliased_type)) = aliases.get(&ident.sym)
                {
                    // This is a type alias reference
                    if let Some(type_params) = &type_ref.type_params {
                        // Generic type alias instantiation: type Alias<T> = ...; used as Alias<Concrete>
                        if let Some(alias_params) = alias_type_params {
                            // Build substitution map: alias param -> concrete type arg
                            let mut subst = std::collections::HashMap::new();
                            for (i, alias_param) in alias_params.iter().enumerate() {
                                if let Some(concrete_arg) = type_params.params.get(i)
                                    && let Some(mapped_arg) =
                                        map_ts_type_with_aliases(concrete_arg, type_aliases)
                                {
                                    subst.insert(alias_param.clone(), mapped_arg);
                                }
                            }
                            // Apply substitution to aliased type, then resolve any nested aliases
                            if let Some(substituted) =
                                crate::types::map_ts_type_with_subst(aliased_type, &subst)
                            {
                                // The substituted type might still contain other type aliases,
                                // so we need to resolve them. However, we can't easily do that
                                // from here since we're working with OatsType, not TsType.
                                // For now, return the substituted type. Nested aliases will be
                                // resolved when the type is used in contexts that call
                                // map_ts_type_with_aliases.
                                return Some(substituted);
                            }
                            // If substitution failed, try resolving the aliased type directly
                            // (for non-parameterized parts)
                            return map_ts_type_with_aliases(aliased_type, type_aliases);
                        }
                    } else {
                        // Non-generic type alias: just resolve to the aliased type
                        return map_ts_type_with_aliases(aliased_type, type_aliases);
                    }
                }
                // Check for generic classes or functions
                if ident.sym.as_str() == "Generic"
                    && let Some(type_params) = &type_ref.type_params
                {
                    let mapped_params: Vec<_> = type_params
                        .params
                        .iter()
                        .filter_map(|p| map_ts_type_with_aliases(p, type_aliases))
                        .collect();
                    return Some(OatsType::Generic(mapped_params));
                }
                // Check if this is a Promise<T> type
                if ident.sym.as_str() == "Promise" {
                    // Extract the type parameter
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases)
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
                        return map_ts_type_with_aliases(first_param, type_aliases)
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
                        return map_ts_type_with_aliases(first_param, type_aliases)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_str() == "Unowned" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases)
                            .map(|inner| OatsType::Unowned(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_str() == "Option" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases)
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
                    && let Some(mapped) = map_ts_type_with_aliases(o, type_aliases)
                {
                    return Some(OatsType::Option(Box::new(mapped)));
                }
            }

            let mut parts = Vec::new();
            for t in &ut.types {
                if let Some(mapped) = map_ts_type_with_aliases(t, type_aliases) {
                    parts.push(mapped);
                } else {
                    return None;
                }
            }
            Some(OatsType::Union(parts))
        }
        TsType::TsArrayType(arr) => {
            // element type
            map_ts_type_with_aliases(&arr.elem_type, type_aliases)
                .map(|elem| OatsType::Array(Box::new(elem)))
        }
        // Map tuple types like `[A, B]` to an array-of-union of the element types.
        // This is a pragmatic compatibility choice: tuples are lowered to runtime
        // arrays where each slot may contain any of the tuple element types.
        // It enables expressions like `t[0]` to work with existing array helpers.
        TsType::TsTupleType(tuple) => {
            // Collect element types into a Tuple variant
            let mut elems: Vec<OatsType> = Vec::new();
            for elem in &tuple.elem_types {
                if let Some(mapped) = map_ts_type_with_aliases(elem, type_aliases) {
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
        // Object literal types (TsTypeLit)
        TsType::TsTypeLit(typelit) => {
            // Object literal type: collect property signatures
            // Map to StructLiteral variant with field names and types
            // Example: { x: number, y: string } -> StructLiteral(vec![("x".to_string(), OatsType::Number), ("y".to_string(), OatsType::String)])
            let mut fields = Vec::new();
            for member in &typelit.members {
                match member {
                    TsTypeElement::Property(prop) => {
                        if let Some(mapped_ty) = map_ts_type_with_aliases(&prop.ty, type_aliases) {
                            fields.push((prop.ident.sym.clone(), mapped_ty));
                        } else {
                            return None;
                        }
                    }
                    _ => {
                        // Methods and index signatures not yet supported in StructLiteral
                        return None;
                    }
                }
            }
            if fields.is_empty() {
                return None;
            }
            Some(OatsType::StructLiteral(fields))
        }
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
    map_ts_type_with_subst_depth(ty, subst, 0)
}

fn map_ts_type_with_subst_depth(
    ty: &TsType,
    subst: &std::collections::HashMap<String, OatsType>,
    depth: usize,
) -> Option<OatsType> {
    // Prevent stack overflow from deeply nested type structures
    const MAX_TYPE_SUBST_DEPTH: usize = 1000;
    if depth > MAX_TYPE_SUBST_DEPTH {
        return None;
    }

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
                        return map_ts_type_with_subst_depth(first_param, subst, depth + 1)
                            .map(|inner| OatsType::Promise(Box::new(inner)));
                    }
                    return Some(OatsType::Promise(Box::new(OatsType::Void)));
                }
                if ident.sym.as_str() == "Array" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst_depth(first_param, subst, depth + 1)
                            .map(|inner| OatsType::Array(Box::new(inner)));
                    }
                    return Some(OatsType::Array(Box::new(OatsType::Number)));
                }
                if ident.sym.as_str() == "Weak" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst_depth(first_param, subst, depth + 1)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.sym.as_str() == "Option" {
                    if let Some(type_params) = &type_ref.type_params
                        && let Some(first_param) = type_params.params.first()
                    {
                        return map_ts_type_with_subst_depth(first_param, subst, depth + 1)
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
        TsType::TsTypeLit(typelit) => {
            // Object literal type with substitution
            let mut fields = Vec::new();
            for member in &typelit.members {
                match member {
                    TsTypeElement::Property(prop) => {
                        if let Some(mapped_ty) = map_ts_type_with_subst(&prop.ty, subst) {
                            fields.push((prop.ident.sym.clone(), mapped_ty));
                        } else {
                            return None;
                        }
                    }
                    _ => {
                        // Methods and index signatures not yet supported
                        return None;
                    }
                }
            }
            if fields.is_empty() {
                return None;
            }
            Some(OatsType::StructLiteral(fields))
        }
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

    // Collect param types with strict checking
    let mut param_types = Vec::new();
    for param in &func_decl.params {
        match &param.pat {
            Pat::Ident(ident) => {
                if let Some(ty) = &param.ty {
                    if let Some(mapped) = map_ts_type(ty) {
                        param_types.push(mapped);
                        continue;
                    } else {
                        // Type annotation present but unsupported
                        diagnostics.push(
                            Diagnostic::error("Unsupported parameter type")
                                .with_code("E1003")
                                .with_label(Label {
                                    span: {
                                        let r = ts_type_span(ty);
                                        Span {
                                            start: r.start,
                                            end: r.end,
                                        }
                                    },
                                    message: format!(
                                        "Parameter '{}' has unsupported type",
                                        ident.sym
                                    ),
                                }),
                        );
                        // Continue with default to allow compilation to proceed
                        param_types.push(OatsType::Number);
                    }
                } else {
                    // Missing type annotation - emit warning but allow compilation
                    diagnostics.push(
                        Diagnostic::error("Missing parameter type annotation")
                            .with_code("E1004")
                            .with_label(Label {
                                span: Span {
                                    start: param.span.start,
                                    end: param.span.end,
                                },
                                message: format!(
                                    "Parameter '{}' requires a type annotation",
                                    ident.sym
                                ),
                            }),
                    );
                    param_types.push(OatsType::Number);
                }
            } // oats_ast only supports Ident patterns for parameters
            Pat::Array(_) => {
                // Array destructuring - require type annotation
                if let Some(ty) = &param.ty {
                    if let Some(mapped) = map_ts_type(ty) {
                        param_types.push(mapped);
                    } else {
                        diagnostics.push(
                            Diagnostic::error("Unsupported array destructuring type")
                                .with_code("E1005")
                                .with_label(Label {
                                    span: Span {
                                        start: param.span.start,
                                        end: param.span.end,
                                    },
                                    message:
                                        "Array destructuring requires a supported type annotation"
                                            .into(),
                                }),
                        );
                        param_types.push(OatsType::Array(Box::new(OatsType::Number)));
                    }
                } else {
                    diagnostics.push(
                        Diagnostic::error("Array destructuring requires type annotation")
                            .with_code("E1006")
                            .with_label(Label {
                                span: Span {
                                    start: param.span.start,
                                    end: param.span.end,
                                },
                                message:
                                    "Array destructuring parameters must have type annotations"
                                        .into(),
                            }),
                    );
                    param_types.push(OatsType::Array(Box::new(OatsType::Number)));
                }
            }
            Pat::Object(_) => {
                // Object destructuring - require type annotation
                if let Some(ty) = &param.ty {
                    if let Some(mapped) = map_ts_type(ty) {
                        param_types.push(mapped);
                    } else {
                        diagnostics.push(
                            Diagnostic::error("Unsupported object destructuring type")
                                .with_code("E1007")
                                .with_label(Label {
                                    span: Span {
                                        start: param.span.start,
                                        end: param.span.end,
                                    },
                                    message:
                                        "Object destructuring requires a supported type annotation"
                                            .into(),
                                }),
                        );
                        param_types.push(OatsType::StructLiteral(Vec::new()));
                    }
                } else {
                    diagnostics.push(
                        Diagnostic::error("Object destructuring requires type annotation")
                            .with_code("E1008")
                            .with_label(Label {
                                span: Span {
                                    start: param.span.start,
                                    end: param.span.end,
                                },
                                message:
                                    "Object destructuring parameters must have type annotations"
                                        .into(),
                            }),
                    );
                    param_types.push(OatsType::StructLiteral(Vec::new()));
                }
            }
            Pat::Rest(_) => {
                // Rest parameters - require type annotation
                if let Some(ty) = &param.ty {
                    if let Some(mapped) = map_ts_type(ty) {
                        param_types.push(mapped);
                    } else {
                        diagnostics.push(
                            Diagnostic::error("Unsupported rest parameter type")
                                .with_code("E1009")
                                .with_label(Label {
                                    span: Span {
                                        start: param.span.start,
                                        end: param.span.end,
                                    },
                                    message: "Rest parameters require a supported type annotation"
                                        .into(),
                                }),
                        );
                        param_types.push(OatsType::Array(Box::new(OatsType::Number)));
                    }
                } else {
                    diagnostics.push(
                        Diagnostic::error("Rest parameter requires type annotation")
                            .with_code("E1010")
                            .with_label(Label {
                                span: Span {
                                    start: param.span.start,
                                    end: param.span.end,
                                },
                                message: "Rest parameters must have type annotations".into(),
                            }),
                    );
                    param_types.push(OatsType::Array(Box::new(OatsType::Number)));
                }
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
    infer_type_from_expr_with_depth(expr, 0)
}

/// Internal helper with recursion depth tracking to prevent stack overflow.
fn infer_type_from_expr_with_depth(expr: &Expr, depth: usize) -> Option<OatsType> {
    // Prevent stack overflow from deeply nested expressions
    const MAX_TYPE_INFERENCE_DEPTH: usize = 1000;
    if depth > MAX_TYPE_INFERENCE_DEPTH {
        return None;
    }

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
                infer_type_from_expr_with_depth(first_elem, depth + 1)
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
                    && let Some(field_type) = infer_type_from_expr_with_depth(&kv.value, depth + 1)
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
            infer_type_from_expr_with_depth(&unary.arg, depth + 1)
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
                    infer_type_from_expr_with_depth(&bin.left, depth + 1)
                        .or_else(|| infer_type_from_expr_with_depth(&bin.right, depth + 1))
                }
            }
        }
        Expr::Paren(paren) => {
            // Parenthesized expressions have the same type as their contents
            infer_type_from_expr_with_depth(&paren.expr, depth + 1)
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
            let cons_type = infer_type_from_expr_with_depth(&cond.cons, depth + 1);
            let alt_type = infer_type_from_expr_with_depth(&cond.alt, depth + 1);
            if cons_type == alt_type {
                cons_type
            } else {
                None
            }
        }
        Expr::Await(await_expr) => {
            // For await expressions, infer the unwrapped type if it's a promise
            if let Some(inner) = infer_type_from_expr_with_depth(&await_expr.arg, depth + 1) {
                if let OatsType::Promise(boxed) = inner {
                    Some(*boxed)
                } else {
                    Some(inner)
                }
            } else {
                None
            }
        }
        // Process model expressions
        Expr::Spawn(_) => Some(OatsType::ProcessId),
        Expr::ProcessSelf(_) => Some(OatsType::ProcessId),
        Expr::ProcessWhereis(_) => Some(OatsType::ProcessId), // Returns ProcessId or null
        Expr::ProcessMonitor(_) => Some(OatsType::MonitorRef),
        Expr::ProcessLink(_)
        | Expr::ProcessUnlink(_)
        | Expr::ProcessRegister(_)
        | Expr::ProcessUnregister(_)
        | Expr::ProcessExit(_)
        | Expr::ProcessDemonitor(_) => {
            // These return void or i32 (success/error), infer as Number
            Some(OatsType::Number)
        }
        Expr::Send(_) => Some(OatsType::Number), // Returns i32 (success/error)
        Expr::Receive(_) => None,                // Returns message payload (unknown type)
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
    apply_inferred_subst_depth(ty, inferred, 0)
}

fn apply_inferred_subst_depth(ty: &OatsType, inferred: &[OatsType], depth: usize) -> OatsType {
    // Prevent stack overflow from deeply nested type structures
    const MAX_INFERRED_SUBST_DEPTH: usize = 1000;
    if depth > MAX_INFERRED_SUBST_DEPTH {
        // Return a safe fallback type to avoid stack overflow
        return OatsType::Number;
    }

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
            other => OatsType::Option(Box::new(apply_inferred_subst_depth(
                other,
                inferred,
                depth + 1,
            ))),
        },
        OatsType::Array(inner) => OatsType::Array(Box::new(apply_inferred_subst_depth(
            inner,
            inferred,
            depth + 1,
        ))),
        OatsType::Union(parts) => {
            let new_parts = parts
                .iter()
                .map(|p| apply_inferred_subst_depth(p, inferred, depth + 1))
                .collect();
            OatsType::Union(new_parts)
        }
        OatsType::Tuple(elems) => OatsType::Tuple(
            elems
                .iter()
                .map(|e| apply_inferred_subst_depth(e, inferred, depth + 1))
                .collect(),
        ),
        OatsType::Weak(inner) => OatsType::Weak(Box::new(apply_inferred_subst_depth(
            inner,
            inferred,
            depth + 1,
        ))),
        OatsType::Promise(inner) => OatsType::Promise(Box::new(apply_inferred_subst_depth(
            inner,
            inferred,
            depth + 1,
        ))),
        OatsType::StructLiteral(fields) => OatsType::StructLiteral(
            fields
                .iter()
                .map(|(n, t)| {
                    (
                        n.clone(),
                        apply_inferred_subst_depth(t, inferred, depth + 1),
                    )
                })
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
