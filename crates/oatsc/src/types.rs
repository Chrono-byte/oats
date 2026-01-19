//! Representation of Oats types used by the compiler.
//!
//! `OatsType` is a compact enum used during parsing and codegen to convey
//! the static type information that the compiler currently understands. The
//! code generator maps these variants to LLVM ABI types and uses this
//! information to decide on boxing/unboxing, union representation, and
//! reference-counting behavior.

use crate::diagnostics::{Diagnostic, Label, Span};
use anyhow::Result;
use oxc_ast::ast::{
    BindingPatternKind, Expression, Function, PropertyKey, TSSignature, TSTupleElement, TSType,
    TSTypeName,
};
use oxc_span::Span as OxcSpan;
use std::collections::HashMap;

// Type alias for the locals stack used in codegen
pub type LocalsStack = Vec<HashMap<String, OatsType>>;

/// Oats type system (used by the compiler)
///
/// Key variants and their meaning:
/// - `Number` -> numeric `f64` values.
/// - `Boolean` -> boolean values (represented as i1/i64 in places).
/// - `Undefined` -> undefined value (JavaScript primitive).
/// - `Null` -> null value (JavaScript primitive).
/// - `BigInt` -> arbitrary precision integers (JavaScript ES2020+).
/// - `Symbol` -> unique symbol values (JavaScript ES6+).
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
    // Number type
    Number,
    // Boolean type
    Boolean,
    // Undefined type (only value is undefined)
    Undefined,
    // Null type (only value is null)
    Null,
    // BigInt type for arbitrary precision integers
    BigInt,
    // Symbol type for unique symbols
    Symbol,
    // Union of multiple possible types (e.g. number | string)
    Union(Vec<Self>),
    // Array of element type (e.g. number[])
    Array(Box<Self>),
    // Weak reference wrapper (non-owning)
    Weak(Box<Self>),
    // Unowned reference wrapper (non-owning, no RC operations)
    Unowned(Box<Self>),
    // Optional wrapper, represented as nullable/Option
    Option(Box<Self>),
    Void,
    String,
    // Fixed-size tuple type with ordered element types
    Tuple(Vec<Self>),
    NominalStruct(String),
    // Promise type wrapping a result type (e.g. Promise<number>)
    Promise(Box<Self>),
    // Anonymous struct-like type (object literal type) with named fields
    StructLiteral(Vec<(String, Self)>),
    // Generic type with type parameters
    Generic(Vec<Self>),
    // Enum type with name and variant names
    Enum(String, Vec<String>),
    // Generic type instance with concrete type arguments
    GenericInstance {
        base_name: String,
        type_args: Vec<Self>,
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
    Type {
        mangled_name: String,
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
    #[must_use]
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
    #[must_use]
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
    /// This is a convenience method for getting the `OatsType` of a variable.
    /// For std functions, this returns None.
    ///
    /// # Arguments
    /// * `name` - Symbol name to look up
    ///
    /// # Returns
    /// The `OatsType` for the symbol if it's a variable, or `None` otherwise
    #[must_use]
    pub fn get_type(&self, name: &str) -> Option<&OatsType> {
        self.get(name).and_then(|symbol| match symbol {
            Symbol::Variable { ty } => Some(ty),
            Symbol::StdFunction { .. } | Symbol::Type { .. } => None,
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

    /// Inserts a type symbol into the current (innermost) scope.
    pub fn insert_type(&mut self, name: String, mangled_name: String) {
        self.insert(name, Symbol::Type { mangled_name });
    }

    /// Returns all symbols from all scopes as a flattened collection.
    ///
    /// This function collects symbols from all scopes in the table, which can
    /// be useful for debugging or analysis purposes. Note that symbols in inner
    /// scopes that shadow outer symbols will both be included in the result.
    ///
    /// # Returns
    /// A vector of (name, symbol) pairs for all symbols in the table
    #[must_use]
    pub fn all_symbols(&self) -> Vec<(String, Symbol)> {
        self.scopes
            .iter()
            .flat_map(|scope| scope.iter().map(|(k, v)| (k.clone(), v.clone())))
            .collect()
    }
}

/// Helper to extract `TSType` from `TSTupleElement`
fn tuple_elem_to_ts_type<'a>(elem: &'a TSTupleElement<'a>) -> Option<&'a TSType<'a>> {
    match elem {
        TSTupleElement::TSOptionalType(opt) => Some(&opt.type_annotation),
        TSTupleElement::TSRestType(rest) => Some(&rest.type_annotation),
        // TSTupleElement inherits all TSType variants, but we can't directly convert
        // For now, return None for inherited variants - they should be rare in practice
        _ => None,
    }
}

/// Helper to get span from a `TSType`
fn ts_type_span(ty: &TSType<'_>) -> OxcSpan {
    match ty {
        TSType::TSNumberKeyword(kw) => kw.span,
        TSType::TSVoidKeyword(kw) => kw.span,
        TSType::TSBooleanKeyword(kw) => kw.span,
        TSType::TSStringKeyword(kw) => kw.span,
        TSType::TSTypeReference(tr) => tr.span,
        TSType::TSArrayType(at) => at.span,
        TSType::TSUnionType(ut) => ut.span,
        TSType::TSIntersectionType(it) => it.span,
        TSType::TSFunctionType(ft) => ft.span,
        TSType::TSTupleType(tt) => tt.span,
        TSType::TSTypeLiteral(tl) => tl.span,
        _ => OxcSpan::default(),
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
#[must_use]
pub fn map_ts_type(ty: &TSType<'_>) -> Option<OatsType> {
    map_ts_type_with_aliases(ty, None, None)
}

/// Maps an Oats AST type annotation to `OatsType`, with optional type alias resolution.
///
/// If `type_aliases` is provided, type references will be resolved through the alias map.
/// For generic type aliases, type arguments are substituted into the aliased type.
type TypeAliasMap<'a> = HashMap<String, (Option<Vec<String>>, &'a TSType<'a>)>;
#[must_use]
pub fn map_ts_type_with_aliases(
    ty: &TSType<'_>,
    type_aliases: Option<&TypeAliasMap<'_>>,
    symbols: Option<&SymbolTable>,
) -> Option<OatsType> {
    match ty {
        TSType::TSNumberKeyword(_) => Some(OatsType::Number),
        TSType::TSVoidKeyword(_) => Some(OatsType::Void),
        TSType::TSBooleanKeyword(_) => Some(OatsType::Boolean),
        TSType::TSStringKeyword(_) => Some(OatsType::String),
        TSType::TSUndefinedKeyword(_) | TSType::TSNullKeyword(_) => Some(OatsType::Void),
        TSType::TSTypeReference(type_ref) => {
            let TSTypeName::IdentifierReference(ident) = &type_ref.type_name else {
                // QualifiedName and ThisExpression not yet supported
                return None;
            };
            {
                // Check for primitive types
                match ident.name.as_str() {
                    "number" => return Some(OatsType::Number),
                    "boolean" => return Some(OatsType::Boolean),
                    "string" => return Some(OatsType::String),
                    "undefined" => return Some(OatsType::Undefined),
                    "null" => return Some(OatsType::Null),
                    "bigint" => return Some(OatsType::BigInt),
                    "symbol" => return Some(OatsType::Symbol),

                    // Process-related types
                    "ProcessId" => return Some(OatsType::ProcessId),
                    "MonitorRef" => return Some(OatsType::MonitorRef),
                    _ => {}
                }

                // Check symbol table for mangled types
                if let Some(syms) = symbols
                    && let Some(Symbol::Type { mangled_name }) = syms.get(ident.name.as_str())
                {
                    return Some(OatsType::NominalStruct(mangled_name.clone()));
                }

                // Check for type aliases before falling back to built-in types
                if let Some(aliases) = type_aliases
                    && let Some((alias_type_params, aliased_type)) =
                        aliases.get(ident.name.as_str())
                {
                    // This is a type alias reference
                    if let Some(type_args) = &type_ref.type_arguments {
                        // Generic type alias instantiation: type Alias<T> = ...; used as Alias<Concrete>
                        if let Some(alias_params) = alias_type_params {
                            // Build substitution map: alias param -> concrete type arg
                            let mut subst = HashMap::new();
                            for (i, alias_param) in alias_params.iter().enumerate() {
                                if let Some(concrete_arg) = type_args.params.get(i)
                                    && let Some(mapped_arg) = map_ts_type_with_aliases(
                                        concrete_arg,
                                        type_aliases,
                                        symbols,
                                    )
                                {
                                    subst.insert(alias_param.clone(), mapped_arg);
                                }
                            }
                            // Apply substitution to aliased type, then resolve any nested aliases
                            if let Some(substituted) = map_ts_type_with_subst(aliased_type, &subst)
                            {
                                // The substituted type might still contain other type aliases,
                                // so we need to resolve them. However, we can't easily do that
                                // from here since we're working with OatsType, not TSType.
                                // For now, return the substituted type. Nested aliases will be
                                // resolved when the type is used in contexts that call
                                // map_ts_type_with_aliases.
                                return Some(substituted);
                            }
                            // If substitution failed, try resolving the aliased type directly
                            // (for non-parameterized parts)
                            return map_ts_type_with_aliases(aliased_type, type_aliases, symbols);
                        }
                    } else {
                        // Non-generic type alias: just resolve to the aliased type
                        return map_ts_type_with_aliases(aliased_type, type_aliases, symbols);
                    }
                }
                // Check for generic classes or functions
                if ident.name.as_str() == "Generic"
                    && let Some(type_args) = &type_ref.type_arguments
                {
                    let mapped_params: Vec<_> = type_args
                        .params
                        .iter()
                        .filter_map(|p| map_ts_type_with_aliases(p, type_aliases, symbols))
                        .collect();
                    return Some(OatsType::Generic(mapped_params));
                }
                // Check if this is a Promise<T> type
                if ident.name.as_str() == "Promise" {
                    // Extract the type parameter
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases, symbols)
                            .map(|inner| OatsType::Promise(Box::new(inner)));
                    }
                    // Promise without type parameter defaults to Promise<void>
                    return Some(OatsType::Promise(Box::new(OatsType::Void)));
                }
                // Support generic Array<T> written as Array<T>
                if ident.name.as_str() == "Array" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases, symbols)
                            .map(|inner| OatsType::Array(Box::new(inner)));
                    }
                    // No type param -> default to Array<number>
                    return Some(OatsType::Array(Box::new(OatsType::Number)));
                }
                // Support Weak<T> and Option<T> as language-level generics
                if ident.name.as_str() == "Weak" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases, symbols)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.name.as_str() == "Unowned" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases, symbols)
                            .map(|inner| OatsType::Unowned(Box::new(inner)));
                    }
                    return None;
                }
                if ident.name.as_str() == "Option" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_aliases(first_param, type_aliases, symbols)
                            .map(|inner| OatsType::Option(Box::new(inner)));
                    }
                    return None;
                }
                // If there are no explicit type parameters, assume a nominal
                // struct; otherwise, treat unknown bare idents as a placeholder
                // Generic so nested generic signatures can be represented.
                if type_ref.type_arguments.is_none() {
                    return Some(OatsType::NominalStruct(ident.name.to_string()));
                }
                // Bare generic type parameter (e.g. `T`) -> placeholder
                Some(OatsType::Generic(vec![]))
            }
        }
        TSType::TSUnionType(ut) => {
            // Special-case common pattern: `T | null` -> Option<T>
            if ut.types.len() == 2 {
                // Try to detect `null` or `undefined` in one of the union
                // arms and map the other arm to an OatsType; if
                // successful, return Option<that_type>.
                let mut nullish_idx = None;
                for (i, t) in ut.types.iter().enumerate() {
                    if matches!(t, TSType::TSNullKeyword(_) | TSType::TSUndefinedKeyword(_)) {
                        nullish_idx = Some(i);
                        break;
                    }
                }

                if let Some(idx) = nullish_idx {
                    let other_idx = 1 - idx;
                    if let Some(mapped) =
                        map_ts_type_with_aliases(&ut.types[other_idx], type_aliases, symbols)
                    {
                        return Some(OatsType::Option(Box::new(mapped)));
                    }
                }
            }

            let mut parts = Vec::new();
            for t in &ut.types {
                if let Some(mapped) = map_ts_type_with_aliases(t, type_aliases, symbols) {
                    parts.push(mapped);
                } else {
                    return None;
                }
            }
            Some(OatsType::Union(parts))
        }
        TSType::TSArrayType(arr) => {
            // element type
            map_ts_type_with_aliases(&arr.element_type, type_aliases, symbols)
                .map(|elem| OatsType::Array(Box::new(elem)))
        }
        // Map tuple types like `[A, B]` to an array-of-union of the element types.
        // This is a pragmatic compatibility choice: tuples are lowered to runtime
        // arrays where each slot may contain any of the tuple element types.
        // It enables expressions like `t[0]` to work with existing array helpers.
        TSType::TSTupleType(tuple) => {
            eprintln!(
                "DEBUG: map_ts_type_with_aliases: Found TSTupleType with {} elements",
                tuple.element_types.len()
            );
            // Collect element types into a Tuple variant
            let mut elems: Vec<OatsType> = Vec::new();
            for (i, elem) in tuple.element_types.iter().enumerate() {
                eprintln!(
                    "DEBUG: Processing tuple element {}: {:?}",
                    i,
                    std::mem::discriminant(elem)
                );
                // TSTupleElement can be TSOptionalType, TSRestType, or any TSType
                // Extract the TSType from the element
                // TSTupleElement can be TSOptionalType, TSRestType, or directly a TSType variant
                // First try the helper function for optional/rest types
                let elem_ty = if let Some(ty) = tuple_elem_to_ts_type(elem) {
                    eprintln!(
                        "DEBUG: tuple_elem_to_ts_type returned Some for element {}",
                        i
                    );
                    ty
                } else {
                    // TSTupleElement inherits all TSType variants
                    // Most inherited TSType variants are not directly usable in Oats tuples
                    // Return None for unsupported inherited variants (rare in practice)
                    // This is safer than transmute and avoids undefined behavior
                    eprintln!(
                        "WARNING: Tuple element {} uses unsupported type variant - skipping",
                        i
                    );
                    return None;
                };
                eprintln!(
                    "DEBUG: Mapping element type {}: {:?}",
                    i,
                    std::mem::discriminant(elem_ty)
                );
                if let Some(mapped) = map_ts_type_with_aliases(elem_ty, type_aliases, symbols) {
                    eprintln!("DEBUG: Successfully mapped element {} to {:?}", i, mapped);
                    elems.push(mapped);
                } else {
                    eprintln!("DEBUG: Failed to map element type {}", i);
                    return None;
                }
            }
            if elems.is_empty() {
                eprintln!("DEBUG: Tuple has no elements after processing");
                return None;
            }
            eprintln!(
                "DEBUG: Successfully created Tuple with {} elements: {:?}",
                elems.len(),
                elems
            );
            Some(OatsType::Tuple(elems))
        }
        // Object literal types (TSTypeLiteral)
        TSType::TSTypeLiteral(typelit) => {
            // Object literal type: collect property signatures
            // Map to StructLiteral variant with field names and types
            // Example: { x: number, y: string } -> StructLiteral(vec![("x".to_string(), OatsType::Number), ("y".to_string(), OatsType::String)])
            let mut fields = Vec::new();
            for member in &typelit.members {
                match member {
                    TSSignature::TSPropertySignature(prop) => {
                        let prop_name = match &prop.key {
                            PropertyKey::StaticIdentifier(ident) => Some(ident.name.to_string()),
                            PropertyKey::Identifier(ident) => Some(ident.name.to_string()),
                            PropertyKey::StringLiteral(s) => Some(s.value.to_string()),
                            _ => None,
                        };

                        if let Some(name) = prop_name {
                            if let Some(type_ann) = &prop.type_annotation {
                                if let Some(mapped_ty) = map_ts_type_with_aliases(
                                    &type_ann.type_annotation,
                                    type_aliases,
                                    symbols,
                                ) {
                                    fields.push((name, mapped_ty));
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
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

/// Map an Oats AST type to an `OatsType` while applying both type substitution
/// and type alias resolution.
#[must_use]
pub fn map_ts_type_with_subst_and_aliases(
    ty: &TSType<'_>,
    subst: &HashMap<String, OatsType>,
    type_aliases: Option<&TypeAliasMap<'_>>,
    symbols: Option<&SymbolTable>,
) -> Option<OatsType> {
    // First apply substitution
    if let Some(substituted) = map_ts_type_with_subst(ty, subst) {
        // If substitution changed the type, return it directly
        // (substitution already handles the type structure)
        Some(substituted)
    } else {
        // If substitution didn't apply, fall back to alias resolution
        map_ts_type_with_aliases(ty, type_aliases, symbols)
    }
}

/// Map an Oats AST type to an `OatsType` while applying a substitution
/// map for type-parameters. The `subst` map maps type-parameter identifier
/// names (e.g. "T") to concrete `OatsType` values. When a `TSTypeReference` refers
/// to a name present in `subst` it will be replaced with the mapped type.
#[must_use]
pub fn map_ts_type_with_subst(
    ty: &TSType<'_>,
    subst: &HashMap<String, OatsType>,
) -> Option<OatsType> {
    map_ts_type_with_subst_depth(ty, subst, 0)
}

fn map_ts_type_with_subst_depth(
    ty: &TSType<'_>,
    subst: &HashMap<String, OatsType>,
    depth: usize,
) -> Option<OatsType> {
    // Prevent stack overflow from deeply nested type structures
    const MAX_TYPE_SUBST_DEPTH: usize = 1000;
    if depth > MAX_TYPE_SUBST_DEPTH {
        return None;
    }

    match ty {
        TSType::TSNumberKeyword(_)
        | TSType::TSVoidKeyword(_)
        | TSType::TSBooleanKeyword(_)
        | TSType::TSStringKeyword(_)
        | TSType::TSUndefinedKeyword(_)
        | TSType::TSNullKeyword(_) => map_ts_type(ty),
        TSType::TSTypeReference(type_ref) => {
            let TSTypeName::IdentifierReference(ident) = &type_ref.type_name else {
                // QualifiedName and ThisExpression not yet supported
                return None;
            };
            {
                let name = ident.name.to_string();
                // If this ident is a substituted type-parameter, return it
                if let Some(mapped) = subst.get(&name) {
                    return Some(mapped.clone());
                }
                // Otherwise fall back to existing map rules for known generics
                // and nominal types. If there are type arguments, map them
                // recursively using the same substitution map.
                if ident.name.as_str() == "Promise" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_subst_depth(first_param, subst, depth + 1)
                            .map(|inner| OatsType::Promise(Box::new(inner)));
                    }
                    return Some(OatsType::Promise(Box::new(OatsType::Void)));
                }
                if ident.name.as_str() == "Array" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_subst_depth(first_param, subst, depth + 1)
                            .map(|inner| OatsType::Array(Box::new(inner)));
                    }
                    return Some(OatsType::Array(Box::new(OatsType::Number)));
                }
                if ident.name.as_str() == "Weak" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
                    {
                        return map_ts_type_with_subst_depth(first_param, subst, depth + 1)
                            .map(|inner| OatsType::Weak(Box::new(inner)));
                    }
                    return None;
                }
                if ident.name.as_str() == "Option" {
                    if let Some(type_args) = &type_ref.type_arguments
                        && let Some(first_param) = type_args.params.first()
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
                if type_ref.type_arguments.is_none() {
                    Some(OatsType::NominalStruct(name))
                } else {
                    Some(OatsType::Generic(vec![]))
                }
            }
        }
        TSType::TSUnionType(un) => {
            // Handle union types like `T | undefined` by applying substitution
            // to each arm. This mirrors `map_ts_type` but uses the provided
            // `subst` map so named type parameters are replaced.
            {
                // Special-case `T | null` or `T | undefined` -> Option<T>
                if un.types.len() == 2 {
                    let mut nullish_idx = None;
                    for (i, t) in un.types.iter().enumerate() {
                        if matches!(t, TSType::TSNullKeyword(_) | TSType::TSUndefinedKeyword(_)) {
                            nullish_idx = Some(i);
                            break;
                        }
                    }

                    if let Some(idx) = nullish_idx {
                        let other_idx = 1 - idx;
                        if let Some(mapped) =
                            map_ts_type_with_subst_depth(&un.types[other_idx], subst, depth + 1)
                        {
                            return Some(OatsType::Option(Box::new(mapped)));
                        }
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
        TSType::TSArrayType(arr) => map_ts_type_with_subst(&arr.element_type, subst)
            .map(|elem| OatsType::Array(Box::new(elem))),
        TSType::TSTupleType(tuple) => {
            let mut elems: Vec<OatsType> = Vec::new();
            for elem in &tuple.element_types {
                // TSTupleElement can be TSOptionalType, TSRestType, or any TSType
                // Extract the TSType from the element
                let elem_ty = match tuple_elem_to_ts_type(elem) {
                    Some(ty) => ty,
                    None => {
                        // TSTupleElement inherits TSType variants, but we can't easily convert
                        // For now, skip inherited variants (they're rare in practice)
                        continue;
                    }
                };
                if let Some(mapped) = map_ts_type_with_subst(elem_ty, subst) {
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
        TSType::TSTypeLiteral(typelit) => {
            // Object literal type with substitution
            let mut fields = Vec::new();
            for member in &typelit.members {
                match member {
                    TSSignature::TSPropertySignature(prop) => {
                        let prop_name = match &prop.key {
                            PropertyKey::StaticIdentifier(ident) => Some(ident.name.to_string()),
                            PropertyKey::Identifier(ident) => Some(ident.name.to_string()),
                            PropertyKey::StringLiteral(s) => Some(s.value.to_string()),
                            _ => None,
                        };

                        if let Some(name) = prop_name {
                            if let Some(type_ann) = &prop.type_annotation {
                                if let Some(mapped_ty) = map_ts_type_with_subst_depth(
                                    &type_ann.type_annotation,
                                    subst,
                                    depth + 1,
                                ) {
                                    fields.push((name, mapped_ty));
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
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
    func_decl: &Function<'_>,
    _symbols: &mut SymbolTable,
) -> Result<(Option<FunctionSig>, Vec<Diagnostic>)> {
    let mut diagnostics = Vec::new();

    // 0. Register type parameters in symbol table first
    let mut type_params = Vec::new();
    if let Some(tp) = &func_decl.type_parameters {
        for p in &tp.params {
            let name = p.name.name.to_string();
            type_params.push(name.clone());
            // Register as a placeholder type
            _symbols.insert_type(name.clone(), name);
        }
    }

    // Return type annotation required for regular functions, but for arrows, we infer
    let ret_type = if let Some(return_type) = &func_decl.return_type {
        if let Some(mapped) =
            map_ts_type_with_aliases(&return_type.type_annotation, None, Some(_symbols))
        {
            mapped
        } else {
            diagnostics.push(
                Diagnostic::error("Unsupported return type")
                    .with_code("E1001")
                    .with_label(Label {
                        span: {
                            let r = ts_type_span(&return_type.type_annotation);
                            Span {
                                start: r.start as usize,
                                end: r.end as usize,
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
                        start: func_decl.span.start as usize,
                        end: func_decl.span.end as usize,
                    },
                    message: "Function return type annotation is required".into(),
                }),
        );
        return Ok((None, diagnostics));
    };

    // Collect param types with strict checking
    let mut param_types = Vec::new();
    for param in &func_decl.params.items {
        match &param.pattern.kind {
            BindingPatternKind::BindingIdentifier(ident) => {
                if let Some(type_ann) = &param.pattern.type_annotation {
                    if let Some(mapped) =
                        map_ts_type_with_aliases(&type_ann.type_annotation, None, Some(_symbols))
                    {
                        param_types.push(mapped);
                        continue;
                    }
                    // Type annotation present but unsupported
                    diagnostics.push(
                        Diagnostic::error("Unsupported parameter type")
                            .with_code("E1003")
                            .with_label(Label {
                                span: {
                                    let r = ts_type_span(&type_ann.type_annotation);
                                    Span {
                                        start: r.start as usize,
                                        end: r.end as usize,
                                    }
                                },
                                message: format!("Parameter '{}' has unsupported type", ident.name),
                            }),
                    );
                    // Continue with default to allow compilation to proceed
                    param_types.push(OatsType::Number);
                } else {
                    // Missing type annotation - emit warning but allow compilation
                    diagnostics.push(
                        Diagnostic::error("Missing parameter type annotation")
                            .with_code("E1004")
                            .with_label(Label {
                                span: Span {
                                    start: param.span.start as usize,
                                    end: param.span.end as usize,
                                },
                                message: format!(
                                    "Parameter '{}' requires a type annotation",
                                    ident.name
                                ),
                            }),
                    );
                    param_types.push(OatsType::Number);
                }
            }
            BindingPatternKind::ArrayPattern(_) => {
                // Array destructuring - require type annotation
                if let Some(type_ann) = &param.pattern.type_annotation {
                    if let Some(mapped) =
                        map_ts_type_with_aliases(&type_ann.type_annotation, None, Some(_symbols))
                    {
                        param_types.push(mapped);
                    } else {
                        diagnostics.push(
                            Diagnostic::error("Unsupported array destructuring type")
                                .with_code("E1005")
                                .with_label(Label {
                                    span: Span {
                                        start: param.span.start as usize,
                                        end: param.span.end as usize,
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
                                    start: param.span.start as usize,
                                    end: param.span.end as usize,
                                },
                                message:
                                    "Array destructuring parameters must have type annotations"
                                        .into(),
                            }),
                    );
                    param_types.push(OatsType::Array(Box::new(OatsType::Number)));
                }
            }
            BindingPatternKind::ObjectPattern(_) => {
                // Object destructuring - require type annotation
                if let Some(type_ann) = &param.pattern.type_annotation {
                    if let Some(mapped) =
                        map_ts_type_with_aliases(&type_ann.type_annotation, None, Some(_symbols))
                    {
                        param_types.push(mapped);
                    } else {
                        diagnostics.push(
                            Diagnostic::error("Unsupported object destructuring type")
                                .with_code("E1007")
                                .with_label(Label {
                                    span: Span {
                                        start: param.span.start as usize,
                                        end: param.span.end as usize,
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
                                    start: param.span.start as usize,
                                    end: param.span.end as usize,
                                },
                                message:
                                    "Object destructuring parameters must have type annotations"
                                        .into(),
                            }),
                    );
                    param_types.push(OatsType::StructLiteral(Vec::new()));
                }
            }
            BindingPatternKind::AssignmentPattern(_) => {
                // Rest parameters - require type annotation
                if let Some(type_ann) = &param.pattern.type_annotation {
                    if let Some(mapped) =
                        map_ts_type_with_aliases(&type_ann.type_annotation, None, Some(_symbols))
                    {
                        param_types.push(mapped);
                    } else {
                        diagnostics.push(
                            Diagnostic::error("Unsupported rest parameter type")
                                .with_code("E1009")
                                .with_label(Label {
                                    span: Span {
                                        start: param.span.start as usize,
                                        end: param.span.end as usize,
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
                                    start: param.span.start as usize,
                                    end: param.span.end as usize,
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
            type_params,
            params: param_types,
            ret: ret_type,
        }),
        diagnostics,
    ))
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
///
/// Note: Expression-based inference is currently limited. For full inference,
/// use the codegen-level inference functions.
#[must_use]
pub fn infer_type(ts_type: Option<&TSType<'_>>, _expr: Option<&Expression<'_>>) -> OatsType {
    // First priority: explicit Oats type annotation
    if let Some(ts_ty) = ts_type
        && let Some(oats_type) = map_ts_type(ts_ty)
    {
        return oats_type;
    }

    // Expression inference removed - it used old AST types and is not currently
    // compatible with oxc_ast. Use codegen-level inference for expression types.

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
#[must_use]
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
            if inferred.is_empty() {
                OatsType::Number
            } else {
                inferred[0].clone()
            }
        }
        OatsType::Option(inner) => match &**inner {
            OatsType::Generic(_) => {
                if inferred.is_empty() {
                    OatsType::Option(Box::new(OatsType::Number))
                } else {
                    OatsType::Option(Box::new(inferred[0].clone()))
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
    /// Check if this type is a Promise
    #[must_use]
    pub const fn is_promise(&self) -> bool {
        matches!(self, Self::Promise(_))
    }

    /// Unwrap the inner type of a Promise, if this is a Promise type
    #[must_use]
    pub fn unwrap_promise_inner(&self) -> Option<&Self> {
        match self {
            Self::Promise(inner) => Some(inner),
            _ => None,
        }
    }

    /// Create a Promise wrapping the given type
    #[must_use]
    pub fn wrap_in_promise(inner: Self) -> Self {
        Self::Promise(Box::new(inner))
    }
}
