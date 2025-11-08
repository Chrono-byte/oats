//! Oats AST definitions
//!
//! This crate defines the abstract syntax tree (AST) for the Oats language.
//! It serves as the contract between the parser and the compiler core.

use std::ops::Range;

/// Represents a source code span as a byte range.
pub type Span = Range<usize>;

/// The top-level module AST node.
#[derive(Debug, Clone)]
pub struct Module {
    pub body: Vec<Stmt>,
    pub span: Span,
}

/// Statements in the AST.
#[derive(Debug, Clone)]
pub enum Stmt {
    FnDecl(FnDecl),
    ClassDecl(ClassDecl),
    VarDecl(VarDecl),
    ExprStmt(ExprStmt),
    If(IfStmt),
    For(ForStmt),
    While(WhileStmt),
    DoWhile(DoWhileStmt),
    Switch(SwitchStmt),
    Try(TryStmt),
    Block(BlockStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Continue(ContinueStmt),
    Throw(ThrowStmt),
    Debugger(DebuggerStmt),
    Labeled(LabeledStmt),
    DeclareFn(DeclareFn),
}

/// Function declaration.
#[derive(Debug, Clone)]
pub struct FnDecl {
    pub ident: Ident,
    pub params: Vec<Param>,
    pub body: Option<BlockStmt>,
    pub return_type: Option<TsType>,
    pub span: Span,
}

/// Class declaration.
#[derive(Debug, Clone)]
pub struct ClassDecl {
    pub ident: Ident,
    pub super_class: Option<Expr>,
    pub body: Vec<ClassMember>,
    pub span: Span,
}

/// Variable declaration.
#[derive(Debug, Clone)]
pub struct VarDecl {
    pub kind: VarDeclKind,
    pub decls: Vec<VarDeclarator>,
    pub span: Span,
}

/// Variable declaration kind.
#[derive(Debug, Clone)]
pub enum VarDeclKind {
    Let { mutable: bool },
    Const,
}

/// Variable declarator.
#[derive(Debug, Clone)]
pub struct VarDeclarator {
    pub name: Pat,
    pub init: Option<Expr>,
    pub span: Span,
}

/// Expression statement.
#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: Expr,
    pub span: Span,
}

/// If statement.
#[derive(Debug, Clone)]
pub struct IfStmt {
    pub test: Expr,
    pub cons: Box<Stmt>,
    pub alt: Option<Box<Stmt>>,
    pub span: Span,
}

/// For statement.
#[derive(Debug, Clone)]
pub struct ForStmt {
    pub init: Option<ForInit>,
    pub test: Option<Expr>,
    pub update: Option<Expr>,
    pub body: Box<Stmt>,
    pub span: Span,
}

/// For-of statement.
#[derive(Debug, Clone)]
pub struct ForOfStmt {
    pub left: ForHead,
    pub right: Expr,
    pub body: Box<Stmt>,
    pub span: Span,
}

/// For head.
#[derive(Debug, Clone)]
pub enum ForHead {
    VarDecl(VarDecl),
    Pat(Pat),
}

/// While statement.
#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub test: Expr,
    pub body: Box<Stmt>,
    pub span: Span,
}

/// Do-while statement.
#[derive(Debug, Clone)]
pub struct DoWhileStmt {
    pub body: Box<Stmt>,
    pub test: Expr,
    pub span: Span,
}

/// Switch statement.
#[derive(Debug, Clone)]
pub struct SwitchStmt {
    pub discriminant: Expr,
    pub cases: Vec<SwitchCase>,
    pub span: Span,
}

/// Switch case.
#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub test: Option<Expr>,
    pub cons: Vec<Stmt>,
    pub span: Span,
}

/// Try statement.
#[derive(Debug, Clone)]
pub struct TryStmt {
    pub block: BlockStmt,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStmt>,
    pub span: Span,
}

/// Catch clause.
#[derive(Debug, Clone)]
pub struct CatchClause {
    pub param: Option<Pat>,
    pub body: BlockStmt,
    pub span: Span,
}

/// Block statement.
#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/// Return statement.
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub arg: Option<Expr>,
    pub span: Span,
}

/// Break statement.
#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub label: Option<Ident>,
    pub span: Span,
}

/// Continue statement.
#[derive(Debug, Clone)]
pub struct ContinueStmt {
    pub label: Option<Ident>,
    pub span: Span,
}

/// Throw statement.
#[derive(Debug, Clone)]
pub struct ThrowStmt {
    pub arg: Expr,
    pub span: Span,
}

/// Debugger statement.
#[derive(Debug, Clone)]
pub struct DebuggerStmt {
    pub span: Span,
}

/// Labeled statement.
#[derive(Debug, Clone)]
pub struct LabeledStmt {
    pub label: Ident,
    pub body: Box<Stmt>,
    pub span: Span,
}

/// Declare function statement.
#[derive(Debug, Clone, PartialEq)]
pub struct DeclareFn {
    pub ident: Ident,
    pub params: Vec<Param>,
    pub return_type: TsType,
    pub span: Span,
}

/// Class member.
#[derive(Debug, Clone)]
pub enum ClassMember {
    Field(FieldDecl),
    Method(MethodDecl),
    Constructor(ConstructorDecl),
}

/// Field declaration.
#[derive(Debug, Clone)]
pub struct FieldDecl {
    pub ident: Ident,
    pub ty: Option<TsType>,
    pub span: Span,
}

/// Method declaration.
#[derive(Debug, Clone)]
pub struct MethodDecl {
    pub ident: Ident,
    pub params: Vec<Param>,
    pub body: Option<BlockStmt>,
    pub return_type: Option<TsType>,
    pub span: Span,
}

/// Constructor declaration.
#[derive(Debug, Clone)]
pub struct ConstructorDecl {
    pub params: Vec<Param>,
    pub body: Option<BlockStmt>,
    pub span: Span,
}

/// Function.
#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Param>,
    pub body: Option<BlockStmt>,
    pub span: Span,
}

/// Function parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub pat: Pat,
    pub ty: Option<TsType>,
    pub span: Span,
}

/// Pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Ident(Ident),
}

/// Identifier.
#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub sym: String,
    pub span: Span,
}

/// Property name.
#[derive(Debug, Clone)]
pub enum PropName {
    Ident(Ident),
    Str(String),
    Num(f64),
    // Add more as needed
}

/// Private name.
#[derive(Debug, Clone)]
pub struct PrivateName {
    pub id: Ident,
    pub span: Span,
}

/// Expressions.
#[derive(Debug, Clone)]
pub enum Expr {
    This(ThisExpr),
    Ident(Ident),
    Lit(Lit),
    Unary(UnaryExpr),
    Bin(BinExpr),
    Cond(CondExpr),
    Call(CallExpr),
    Member(MemberExpr),
    Array(ArrayLit),
    Object(ObjectLit),
    Fn(FnExpr),
    Arrow(ArrowExpr),
    Assign(AssignExpr),
    Seq(SeqExpr),
    Paren(ParenExpr),
    // Add more as needed
}

/// This expression.
#[derive(Debug, Clone)]
pub struct ThisExpr {
    pub span: Span,
}

/// Literal.
#[derive(Debug, Clone)]
pub enum Lit {
    Str(String),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),
    F32(f32),
    F64(f64),
    Bool(bool),
    Null,
}

/// Unary expression.
#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub arg: Box<Expr>,
    pub span: Span,
}

/// Unary operator.
#[derive(Debug, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    TypeOf,
    Void,
    Delete,
    // Add more
}

/// Binary expression.
#[derive(Debug, Clone)]
pub struct BinExpr {
    pub op: BinaryOp,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
    pub span: Span,
}

/// Binary operator.
#[derive(Debug, Clone)]
pub enum BinaryOp {
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Plus,
    Minus,
    Mul,
    Div,
    // Add more
}

/// Conditional expression.
#[derive(Debug, Clone)]
pub struct CondExpr {
    pub test: Box<Expr>,
    pub cons: Box<Expr>,
    pub alt: Box<Expr>,
    pub span: Span,
}

/// Call expression.
#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Callee,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Callee.
#[derive(Debug, Clone)]
pub enum Callee {
    Expr(Box<Expr>),
    Super(Super),
}

/// Super.
#[derive(Debug, Clone)]
pub struct Super {
    pub span: Span,
}

/// Member expression.
#[derive(Debug, Clone)]
pub struct MemberExpr {
    pub obj: Box<Expr>,
    pub prop: MemberProp,
    pub span: Span,
}

/// Member property.
#[derive(Debug, Clone)]
pub enum MemberProp {
    Ident(Ident),
    PrivateName(PrivateName),
    Computed(Box<Expr>),
}

/// Array literal.
#[derive(Debug, Clone)]
pub struct ArrayLit {
    pub elems: Vec<Option<Expr>>,
    pub span: Span,
}

/// Object literal.
#[derive(Debug, Clone)]
pub struct ObjectLit {
    pub props: Vec<PropOrSpread>,
    pub span: Span,
}

/// Property or spread.
#[derive(Debug, Clone)]
pub enum PropOrSpread {
    Prop(Prop),
    Spread(SpreadElement),
}

/// Property.
#[derive(Debug, Clone)]
pub enum Prop {
    Shorthand(Ident),
    KeyValue(KeyValueProp),
    // Add more
}

/// Key-value property.
#[derive(Debug, Clone)]
pub struct KeyValueProp {
    pub key: PropName,
    pub value: Expr,
    pub span: Span,
}

/// Spread element.
#[derive(Debug, Clone)]
pub struct SpreadElement {
    pub expr: Expr,
    pub span: Span,
}

/// Function expression.
#[derive(Debug, Clone)]
pub struct FnExpr {
    pub ident: Option<Ident>,
    pub function: Function,
    pub span: Span,
}

/// Arrow expression.
#[derive(Debug, Clone)]
pub struct ArrowExpr {
    pub params: Vec<Pat>,
    pub body: ArrowBody,
    pub span: Span,
}

/// Arrow body.
#[derive(Debug, Clone)]
pub enum ArrowBody {
    Expr(Box<Expr>),
    Block(BlockStmt),
}

/// Assignment expression.
#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub op: AssignOp,
    pub left: AssignTarget,
    pub right: Box<Expr>,
    pub span: Span,
}

/// Assignment operator.
#[derive(Debug, Clone)]
pub enum AssignOp {
    Eq,
    // Add more
}

/// Assignment target.
#[derive(Debug, Clone)]
pub enum AssignTarget {
    Pat(Pat),
    // Add more
}

/// Sequence expression.
#[derive(Debug, Clone)]
pub struct SeqExpr {
    pub exprs: Vec<Expr>,
    pub span: Span,
}

/// Parenthesized expression.
#[derive(Debug, Clone)]
pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub span: Span,
}

/// TypeScript type annotations.
#[derive(Debug, Clone, PartialEq)]
pub enum TsType {
    TsKeywordType(TsKeywordType),
    TsTypeRef(TsTypeRef),
    TsArrayType(TsArrayType),
    // Add more as needed
}

/// TypeScript keyword type.
#[derive(Debug, Clone, PartialEq)]
pub enum TsKeywordType {
    TsNumberKeyword,
    TsStringKeyword,
    TsBooleanKeyword,
    TsVoidKeyword,
    // Add more
}

/// TypeScript type reference.
#[derive(Debug, Clone, PartialEq)]
pub struct TsTypeRef {
    pub type_name: TsEntityName,
    pub type_params: Option<TsTypeParamInstantiation>,
    pub span: Span,
}

/// TypeScript entity name.
#[derive(Debug, Clone, PartialEq)]
pub enum TsEntityName {
    Ident(Ident),
    // Add more
}

/// TypeScript type parameter instantiation.
#[derive(Debug, Clone, PartialEq)]
pub struct TsTypeParamInstantiation {
    pub params: Vec<TsType>,
    pub span: Span,
}

/// TypeScript array type.
#[derive(Debug, Clone, PartialEq)]
pub struct TsArrayType {
    pub elem_type: Box<TsType>,
    pub span: Span,
}

/// For initialization.
#[derive(Debug, Clone)]
pub enum ForInit {
    VarDecl(VarDecl),
    Expr(Expr),
}
