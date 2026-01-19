//! Common types module for code generation
//!
//! This module contains shared type definitions used across multiple codegen modules
//! to eliminate redundancy and ensure consistency.

use inkwell::types::BasicTypeEnum;
use inkwell::values::PointerValue;
use std::collections::HashMap;

/// Local entry representation used across all codegen modules
/// Represents a local variable with its storage, type, and metadata
pub type LocalEntry<'a> = (
    PointerValue<'a>,               // alloca ptr
    BasicTypeEnum<'a>,              // LLVM type
    bool,                           // is_initialized
    bool,                           // is_const
    bool,                           // is_weak
    Option<String>,                 // nominal type name
    Option<crate::types::OatsType>, // OatsType for unions
);

/// Stack of local variable scopes
pub type LocalsStackLocal<'a> = Vec<HashMap<String, LocalEntry<'a>>>;

/// Context for loop control flow (break/continue)
#[derive(Clone)]
pub struct LoopContext<'a> {
    /// Target block for `continue` statements
    pub continue_block: inkwell::basic_block::BasicBlock<'a>,
    /// Target block for `break` statements
    pub break_block: inkwell::basic_block::BasicBlock<'a>,
    /// Index into `locals_stack` marking the first scope belonging to this loop.
    /// Used to limit RC decrements when breaking/continuing.
    pub locals_start: usize,
    /// Optional label for labeled break/continue
    pub label: Option<String>,
}

/// Type maps for various language constructs
pub type EnumVariantMap = HashMap<String, Vec<(String, Vec<crate::types::OatsType>)>>;
pub type TypeAliasMap<'a> = HashMap<String, (Option<Vec<String>>, &'a oxc_ast::ast::TSType<'a>)>;
