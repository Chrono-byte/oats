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
    For(Box<ForStmt>),
    ForIn(ForInStmt),
    ForOf(ForOfStmt),
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
    Import(ImportStmt),
    TypeAlias(TypeAlias),
    InterfaceDecl(InterfaceDecl),
    EnumDecl(EnumDecl),
    NamespaceDecl(NamespaceDecl),
}

/// Function declaration.
#[derive(Debug, Clone)]
pub struct FnDecl {
    pub ident: Ident,
    pub params: Vec<Param>,
    pub body: Option<BlockStmt>,
    pub return_type: Option<TsType>,
    pub is_async: bool,
    pub is_generator: bool,
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
    pub ty: Option<TsType>,
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

/// For-in statement.
#[derive(Debug, Clone)]
pub struct ForInStmt {
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

/// Import statement.
#[derive(Debug, Clone)]
pub struct ImportStmt {
    pub specifiers: Vec<ImportSpecifier>,
    pub source: String,
    pub span: Span,
}

/// Import specifier.
#[derive(Debug, Clone)]
pub enum ImportSpecifier {
    Named {
        local: Ident,
        imported: Option<Ident>, // None means same as local
        span: Span,
    },
    Namespace {
        local: Ident,
        span: Span,
    },
    Default {
        local: Ident,
        span: Span,
    },
}

/// Type alias declaration.
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub ident: Ident,
    pub type_params: Option<Vec<TsTypeParam>>,
    pub ty: TsType,
    pub span: Span,
}

/// Type parameter.
#[derive(Debug, Clone, PartialEq)]
pub struct TsTypeParam {
    pub ident: Ident,
    pub constraint: Option<TsType>,
    pub default: Option<TsType>,
    pub span: Span,
}

/// Interface declaration.
#[derive(Debug, Clone)]
pub struct InterfaceDecl {
    pub ident: Ident,
    pub type_params: Option<Vec<TsTypeParam>>,
    pub extends: Vec<TsTypeRef>,
    pub body: Vec<InterfaceMember>,
    pub span: Span,
}

/// Interface member.
#[derive(Debug, Clone)]
pub enum InterfaceMember {
    Property(InterfaceProperty),
    Method(InterfaceMethod),
    IndexSignature(IndexSignature),
}

/// Interface property.
#[derive(Debug, Clone)]
pub struct InterfaceProperty {
    pub ident: Ident,
    pub ty: TsType,
    pub optional: bool,
    pub readonly: bool,
    pub span: Span,
}

/// Interface method.
#[derive(Debug, Clone)]
pub struct InterfaceMethod {
    pub ident: Ident,
    pub params: Vec<Param>,
    pub return_type: TsType,
    pub optional: bool,
    pub span: Span,
}

/// Index signature.
#[derive(Debug, Clone, PartialEq)]
pub struct IndexSignature {
    pub key_name: Ident,
    pub key_type: TsType,
    pub value_type: TsType,
    pub readonly: bool,
    pub span: Span,
}

/// Enum declaration.
#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub ident: Ident,
    pub members: Vec<EnumMember>,
    pub span: Span,
}

/// Enum member (variant).
#[derive(Debug, Clone)]
pub struct EnumMember {
    pub ident: Ident,
    /// Optional fields for this variant. If None, variant has no data.
    /// If Some, contains the field types (tuple-like variant).
    /// For struct-like variants, this would be a tuple of types in order.
    pub fields: Option<Vec<TsType>>,
    pub span: Span,
}

/// Namespace declaration.
#[derive(Debug, Clone)]
pub struct NamespaceDecl {
    pub ident: Ident,
    pub body: Vec<Stmt>,
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
    pub return_type: Option<TsType>,
    pub span: Span,
    pub is_async: bool,
    pub is_generator: bool,
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
    Array(ArrayPat),
    Object(ObjectPat),
    Rest(RestPat),
}

/// Array pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct ArrayPat {
    pub elems: Vec<Option<Pat>>,
    pub span: Span,
}

/// Object pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct ObjectPat {
    pub props: Vec<ObjectPatProp>,
    pub span: Span,
}

/// Object pattern property.
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectPatProp {
    KeyValue {
        key: PropName,
        value: Pat,
        span: Span,
    },
    Rest {
        arg: Ident,
        span: Span,
    },
}

/// Rest pattern.
#[derive(Debug, Clone, PartialEq)]
pub struct RestPat {
    pub arg: Box<Pat>,
    pub span: Span,
}

/// Identifier.
#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub sym: String,
    pub span: Span,
}

/// Property name.
#[derive(Debug, Clone, PartialEq)]
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
    OptionalMember(OptionalMemberExpr),
    Array(ArrayLit),
    Object(ObjectLit),
    Fn(FnExpr),
    Arrow(ArrowExpr),
    Assign(AssignExpr),
    Seq(SeqExpr),
    Paren(ParenExpr),
    New(NewExpr),
    Update(UpdateExpr),
    Await(AwaitExpr),
    Yield(YieldExpr),
    Super(SuperExpr),
    Tpl(TplExpr),
    // Process model expressions
    Spawn(SpawnExpr),
    Send(SendExpr),
    Receive(ReceiveExpr),
    ProcessSelf(ProcessSelfExpr),
    ProcessExit(ProcessExitExpr),
    ProcessLink(ProcessLinkExpr),
    ProcessUnlink(ProcessUnlinkExpr),
    ProcessMonitor(ProcessMonitorExpr),
    ProcessDemonitor(ProcessDemonitorExpr),
    ProcessWhereis(ProcessWhereisExpr),
    ProcessRegister(ProcessRegisterExpr),
    ProcessUnregister(ProcessUnregisterExpr),
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
    BitwiseNot, // ~
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
    Mod,             // %
    And,             // &&
    Or,              // ||
    BitwiseAnd,      // &
    BitwiseOr,       // |
    BitwiseXor,      // ^
    LShift,          // <<
    RShift,          // >>
    URShift,         // >>>
    Exp,             // **
    NullishCoalesce, // ??
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

/// Optional member expression (optional chaining).
#[derive(Debug, Clone)]
pub struct OptionalMemberExpr {
    pub obj: Box<Expr>,
    pub prop: MemberProp,
    pub span: Span,
}

/// Super expression.
#[derive(Debug, Clone)]
pub struct SuperExpr {
    pub span: Span,
}

/// Yield expression.
#[derive(Debug, Clone)]
pub struct YieldExpr {
    pub arg: Option<Box<Expr>>,
    pub delegate: bool, // yield* vs yield
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
    Spread(Box<SpreadElement>),
}

/// Property.
#[derive(Debug, Clone)]
pub enum Prop {
    Shorthand(Ident),
    KeyValue(Box<KeyValueProp>),
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
    pub return_type: Option<TsType>,
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
    Eq,           // =
    PlusEq,       // +=
    MinusEq,      // -=
    MulEq,        // *=
    DivEq,        // /=
    ModEq,        // %=
    LShiftEq,     // <<=
    RShiftEq,     // >>=
    URShiftEq,    // >>>=
    BitwiseAndEq, // &=
    BitwiseOrEq,  // |=
    BitwiseXorEq, // ^=
    ExpEq,        // **=
}

/// Assignment target.
#[derive(Debug, Clone)]
pub enum AssignTarget {
    Pat(Pat),
    Member(MemberExpr),
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

/// New expression.
#[derive(Debug, Clone)]
pub struct NewExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// Update expression (++x, x++, --x, x--).
#[derive(Debug, Clone)]
pub struct UpdateExpr {
    pub op: UpdateOp,
    pub prefix: bool,
    pub arg: Box<Expr>,
    pub span: Span,
}

/// Update operator.
#[derive(Debug, Clone)]
pub enum UpdateOp {
    Inc, // ++
    Dec, // --
}

/// Await expression.
#[derive(Debug, Clone)]
pub struct AwaitExpr {
    pub arg: Box<Expr>,
    pub span: Span,
}

/// Template literal expression.
#[derive(Debug, Clone)]
pub struct TplExpr {
    pub quasis: Vec<TplElement>,
    pub exprs: Vec<Expr>,
    pub span: Span,
}

/// Template literal element (quasi or expression).
#[derive(Debug, Clone)]
pub struct TplElement {
    pub raw: String,
    pub cooked: Option<String>,
    pub span: Span,
}

/// Spawn expression: spawn a new process
#[derive(Debug, Clone)]
pub struct SpawnExpr {
    pub name: Option<Box<Expr>>,      // Optional process name (string)
    pub priority: Option<Box<Expr>>,  // Optional priority (0=Normal, 1=High)
    pub span: Span,
}

/// Send expression: send a message to a process
#[derive(Debug, Clone)]
pub struct SendExpr {
    pub to: Box<Expr>,                // Target process (ProcessId or string name)
    pub message: Box<Expr>,            // Message payload
    pub span: Span,
}

/// Receive expression: receive a message
#[derive(Debug, Clone)]
pub struct ReceiveExpr {
    pub type_id: Option<Box<Expr>>,   // Optional type filter (i64)
    pub span: Span,
}

/// Process self expression: get current process ID
#[derive(Debug, Clone)]
pub struct ProcessSelfExpr {
    pub span: Span,
}

/// Process exit expression: exit a process
#[derive(Debug, Clone)]
pub struct ProcessExitExpr {
    pub pid: Option<Box<Expr>>,       // Optional process ID (defaults to self)
    pub reason: Option<Box<Expr>>,    // Optional exit reason (string or number)
    pub span: Span,
}

/// Process link expression: link two processes
#[derive(Debug, Clone)]
pub struct ProcessLinkExpr {
    pub pid1: Box<Expr>,              // First process ID
    pub pid2: Box<Expr>,               // Second process ID
    pub span: Span,
}

/// Process unlink expression: unlink two processes
#[derive(Debug, Clone)]
pub struct ProcessUnlinkExpr {
    pub pid1: Box<Expr>,               // First process ID
    pub pid2: Box<Expr>,               // Second process ID
    pub span: Span,
}

/// Process monitor expression: monitor a process
#[derive(Debug, Clone)]
pub struct ProcessMonitorExpr {
    pub target: Box<Expr>,            // Target process ID
    pub span: Span,
}

/// Process demonitor expression: stop monitoring a process
#[derive(Debug, Clone)]
pub struct ProcessDemonitorExpr {
    pub monitor_ref: Box<Expr>,       // Monitor reference
    pub span: Span,
}

/// Process whereis expression: lookup process by name
#[derive(Debug, Clone)]
pub struct ProcessWhereisExpr {
    pub name: Box<Expr>,               // Process name (string)
    pub span: Span,
}

/// Process register expression: register a process name
#[derive(Debug, Clone)]
pub struct ProcessRegisterExpr {
    pub pid: Box<Expr>,                // Process ID
    pub name: Box<Expr>,               // Process name (string)
    pub span: Span,
}

/// Process unregister expression: unregister a process name
#[derive(Debug, Clone)]
pub struct ProcessUnregisterExpr {
    pub name: Box<Expr>,               // Process name (string)
    pub span: Span,
}

/// Oats type annotations.
#[derive(Debug, Clone, PartialEq)]
pub enum TsType {
    TsKeywordType(TsKeywordType),
    TsTypeRef(TsTypeRef),
    TsArrayType(TsArrayType),
    TsUnionType(TsUnionType),
    TsIntersectionType(TsIntersectionType),
    TsFunctionType(TsFunctionType),
    TsTupleType(TsTupleType),
    TsTypeLit(TsTypeLit),
    // Add more as needed
}

/// Type literal (object type).
#[derive(Debug, Clone, PartialEq)]
pub struct TsTypeLit {
    pub members: Vec<TsTypeElement>,
    pub span: Span,
}

/// Type element in a type literal.
#[derive(Debug, Clone, PartialEq)]
pub enum TsTypeElement {
    Property(TsPropertySignature),
    Method(TsMethodSignature),
    IndexSignature(IndexSignature),
}

/// Property signature in a type literal.
#[derive(Debug, Clone, PartialEq)]
pub struct TsPropertySignature {
    pub ident: Ident,
    pub ty: TsType,
    pub optional: bool,
    pub readonly: bool,
    pub span: Span,
}

/// Method signature in a type literal.
#[derive(Debug, Clone, PartialEq)]
pub struct TsMethodSignature {
    pub ident: Ident,
    pub params: Vec<Param>,
    pub return_type: TsType,
    pub optional: bool,
    pub span: Span,
}

/// Oats keyword type.
#[derive(Debug, Clone, PartialEq)]
pub enum TsKeywordType {
    TsNumberKeyword,
    TsStringKeyword,
    TsBooleanKeyword,
    TsVoidKeyword,
    // Add more
}

/// Oats type reference.
#[derive(Debug, Clone, PartialEq)]
pub struct TsTypeRef {
    pub type_name: TsEntityName,
    pub type_params: Option<TsTypeParamInstantiation>,
    pub span: Span,
}

/// Oats entity name.
#[derive(Debug, Clone, PartialEq)]
pub enum TsEntityName {
    Ident(Ident),
    // Add more
}

/// Oats type parameter instantiation.
#[derive(Debug, Clone, PartialEq)]
pub struct TsTypeParamInstantiation {
    pub params: Vec<TsType>,
    pub span: Span,
}

/// Oats array type.
#[derive(Debug, Clone, PartialEq)]
pub struct TsArrayType {
    pub elem_type: Box<TsType>,
    pub span: Span,
}

/// Oats union type.
#[derive(Debug, Clone, PartialEq)]
pub struct TsUnionType {
    pub types: Vec<TsType>,
    pub span: Span,
}

/// Oats intersection type.
#[derive(Debug, Clone, PartialEq)]
pub struct TsIntersectionType {
    pub types: Vec<TsType>,
    pub span: Span,
}

/// Oats function type.
#[derive(Debug, Clone, PartialEq)]
pub struct TsFunctionType {
    pub params: Vec<Param>,
    pub return_type: Box<TsType>,
    pub span: Span,
}

/// Oats tuple type.
#[derive(Debug, Clone, PartialEq)]
pub struct TsTupleType {
    pub elem_types: Vec<TsType>,
    pub span: Span,
}

/// For initialization.
#[derive(Debug, Clone)]
pub enum ForInit {
    VarDecl(VarDecl),
    Expr(Expr),
}
