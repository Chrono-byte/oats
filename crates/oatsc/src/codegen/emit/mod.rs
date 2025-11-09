//! Code generation for top-level items.
//!
//! This module implements lowering of high-level declarations (functions,
//! constructors, etc.) into LLVM IR using the shared `CodeGen` helpers.
//!
//! Notes on design and ABI decisions:
//! - Functions use a simple, handwritten ABI mapping defined by `OatsType` ->
//!   LLVM primitive types. Numbers map to `f64`, booleans to `i1`/i8 as
//!   appropriate, and any pointer-like types map to `i8*` so the runtime can
//!   uniformly operate on object pointers. Unions are mapped to either `f64`
//!   or `i8*` depending on whether any branch is pointer-like; this keeps
//!   math-heavy unions efficient while still supporting heap objects.
//! - Constructors return `i8*` (object base pointer). The runtime expects the
//!   object layout described below and the codegen relies on the runtime's
//!   helpers (rc_inc/rc_dec, box/unbox) to manage lifetimes.

pub mod async_handling;
pub mod constructors;
pub mod enums;
pub mod functions;
pub mod generator_handling;

// Type alias for the `locals_stack` used during statement lowering.
//
// This is a stack of scopes, where each scope maps local variable names to
// a tuple containing:
// - PointerValue: the alloca or pointer to the variable's storage.
// - BasicTypeEnum: the LLVM type of the variable.
// - bool: is this variable mutable (declared with `let`; `let` is immutable by default, use `let mut` for mutation)
// - bool: is this variable a function parameter
// - bool: is this variable declared as Weak<T> (for reference counting)
// - Option<String>: if the variable is of a NominalStruct type, the name of
//   that nominal type; otherwise None. This helps member lowering resolve
//   fields without fallback heuristics.
// - Option<OatsType>: the high-level OatsType, for unions
pub type LocalsStackLocal<'a> = Vec<
    std::collections::HashMap<
        String,
        (
            inkwell::values::PointerValue<'a>,
            inkwell::types::BasicTypeEnum<'a>,
            bool,
            bool,
            bool,
            Option<String>,
            Option<crate::types::OatsType>,
        ),
    >,
>;
