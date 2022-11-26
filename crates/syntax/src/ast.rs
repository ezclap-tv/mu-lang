//! This module contains all the type definitions that constitute Mu's
//! [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
//! (AST).
//!
//! The entrypoint for the AST is [Module][`crate::ast::Module`].

// ### Developer notes for working with spans in this module:
//
// Use `T` wrapped in [Spanned][`crate::span::Spanned`] instead of `T` when
// the full span of the syntax node cannot be reconstructed from its fields
// alone. The Span inside of `Spanned` should always encapsulate the entire
// syntax node.
//
// If you need to add a span to an enum, add a `Kind` suffix to its name, and
// create a type alias for the original name which wraps the enum in
// [Spanned][`crate::span::Spanned`]. For example:
//
// ```text
// enum Stmt<'a> { ... }
// // the above becomes
// enum StmtKind<'a> { ... }
// type Stmt<'a> = Spanned<StmtKind<'a>>;
// ```

#![allow(clippy::needless_lifetimes)]

use std::borrow::Cow;

use indexmap::IndexMap;
use span::Spanned;
use syntax_derive::DebugInner;

/// Any kind of name containing alphanumeric characters, underscores, and
/// numbers: `a`, `_b`, `c_0`, etc.
/// It is invalid for an identifier to start with a digit.
pub type Ident<'a> = Spanned<Cow<'a, str>>;

/// Order-preserving hash map
pub type Map<'a, T> = IndexMap<Cow<'a, str>, T>;

/// A module is the basic building block of Mu programs.
///
/// Each module contains a list of imports and exported symbols.
#[derive(Clone, Debug, Default)]
pub struct Module<'a> {
  pub imports: Vec<Import<'a>>,
  pub symbols: Symbols<'a>,
  pub top_level: Vec<Stmt<'a>>,
}

/// A normalized import, which may optionally be renamed.
///
/// e.g.:
///
/// ```text
/// use {
///   a.{b, c as d},
///   e as f
/// }
/// ```
///
/// Will normalize to the following imports:
///
/// ```text
/// use a.b;
/// use a.c as d;
/// use e as f;
/// ```
#[derive(Clone, Debug, Default)]
pub struct Import<'a> {
  /// Normalized path to the symbol or module.
  pub path: Vec<Ident<'a>>,
  /// Imports may be renamed.
  pub alias: Option<Ident<'a>>,
}

#[derive(Clone, Debug, Default)]
pub struct Symbols<'a> {
  pub fns: Map<'a, (symbol::Vis, symbol::Fn<'a>)>,
  pub classes: Map<'a, (symbol::Vis, symbol::Class<'a>)>,
  pub traits: Map<'a, (symbol::Vis, symbol::Trait<'a>)>,
  pub aliases: Map<'a, (symbol::Vis, symbol::Alias<'a>)>,
}

pub mod symbol {
  use super::{expr, Expr, Ident, Map, Type};

  /// A visibility modifier.
  #[derive(Clone, Copy, Debug, PartialEq)]
  pub enum Vis {
    /// `pub`
    Public,
    /// There is no syntax for this, symbols are private by default.
    Private,
  }

  /// Functions are bundles of code.
  ///
  /// ```text
  /// fn name[T, A, B, C](a: A, b: B, c: C) -> T
  /// where
  ///   A: Foo,
  ///   B: Bar[C],
  ///   C: Baz,
  /// {
  ///   // ...
  /// }
  /// ```
  #[derive(Clone, Debug)]
  pub struct Fn<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub params: Vec<Param<'a>>,
    pub ret: Option<Type<'a>>,
    pub bounds: Vec<Bound<'a>>,
    pub body: Option<expr::Block<'a>>,
  }

  /// Classes encapsulate data, and operations on that data.
  ///
  /// ```text
  /// class Foo[T] {
  ///   type Bar = T;
  ///   fn new() -> Self { /* ... */ }
  ///   impl Trait { /* ... */ }
  /// }
  /// ```
  #[derive(Clone, Debug)]
  pub struct Class<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub bounds: Vec<Bound<'a>>,
    pub members: ClassMembers<'a>,
  }

  #[derive(Clone, Debug, Default)]
  pub struct ClassMembers<'a> {
    pub fields: Map<'a, Field<'a>>,
    pub fns: Map<'a, Fn<'a>>,
    pub aliases: Map<'a, Alias<'a>>,
    pub impls: Vec<Impl<'a>>,
  }

  /// Traits encapsulate behavior.
  /// Anything that is generic can be bound to traits.
  ///
  /// ```text
  /// trait Foo[T] {
  ///   type Bar = T;
  ///   fn bar() -> Bar;
  /// }
  ///
  /// fn f[T](v: Foo[T]) -> Foo[T].Bar {
  ///   v.bar()
  /// }
  /// ```
  #[derive(Clone, Debug)]
  pub struct Trait<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub bounds: Vec<Bound<'a>>,
    pub members: TraitMembers<'a>,
  }

  #[derive(Clone, Debug, Default)]
  pub struct TraitMembers<'a> {
    pub fns: Map<'a, Fn<'a>>,
    pub aliases: Map<'a, Alias<'a>>,
  }

  /// A type alias allows renaming types and partially instantiating generic
  /// types.
  ///
  /// ```text
  /// class Foo[T];
  ///
  /// type Bar = Foo[Baz];
  /// ```
  #[derive(Clone, Debug)]
  pub struct Alias<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub bounds: Vec<Bound<'a>>,
    pub ty: Type<'a>,
  }

  #[derive(Clone, Debug)]
  pub struct Impl<'a> {
    pub tparams: Vec<TParam<'a>>,
    pub ty: Type<'a>,
    pub members: TraitMembers<'a>,
  }

  #[derive(Clone, Debug)]
  pub struct TParam<'a> {
    pub name: Ident<'a>,
    pub default: Option<Type<'a>>,
  }

  #[derive(Clone, Debug)]
  pub struct Param<'a> {
    pub name: Ident<'a>,
    pub ty: Option<Type<'a>>,
    pub default: Option<Expr<'a>>,
  }

  #[derive(Clone, Debug)]
  pub struct Bound<'a> {
    pub ty: Type<'a>,
    pub constraint: Vec<Type<'a>>,
  }

  #[derive(Clone, Debug)]
  pub struct Field<'a> {
    pub name: Ident<'a>,
    pub ty: Type<'a>,
  }
}

#[derive(Clone, DebugInner)]
pub enum StmtKind<'a> {
  Let(Box<stmt::Let<'a>>),
  Loop(Box<stmt::Loop<'a>>),
  Expr(Box<Expr<'a>>),
}

pub type Stmt<'a> = Spanned<StmtKind<'a>>;

pub mod stmt {
  use super::expr::Block;
  use super::{DebugInner, Expr, Ident, StmtKind, Type};

  #[derive(Clone, Debug)]
  pub struct Let<'a> {
    pub name: Ident<'a>,
    pub ty: Option<Type<'a>>,
    pub value: Expr<'a>,
  }

  #[inline]
  pub fn let_<'a>(name: Ident<'a>, ty: Option<Type<'a>>, value: Expr<'a>) -> StmtKind<'a> {
    StmtKind::Let(Box::new(Let { name, ty, value }))
  }

  #[derive(Clone, DebugInner)]
  pub enum Loop<'a> {
    For(LoopFor<'a>),
    While(LoopWhile<'a>),
    Inf(LoopInf<'a>),
  }

  #[derive(Clone, Debug)]
  pub struct LoopFor<'a> {
    pub item: Ident<'a>,
    pub iter: Expr<'a>,
    pub body: Block<'a>,
  }

  #[inline]
  pub fn for_<'a>(item: Ident<'a>, iter: Expr<'a>, body: Block<'a>) -> StmtKind<'a> {
    StmtKind::Loop(Box::new(Loop::For(LoopFor { item, iter, body })))
  }

  #[derive(Clone, Debug)]
  pub struct LoopWhile<'a> {
    pub cond: Expr<'a>,
    pub body: Block<'a>,
  }

  #[inline]
  pub fn while_<'a>(cond: Expr<'a>, body: Block<'a>) -> StmtKind<'a> {
    StmtKind::Loop(Box::new(Loop::While(LoopWhile { cond, body })))
  }

  #[derive(Clone, Debug)]
  pub struct LoopInf<'a> {
    pub body: Block<'a>,
  }

  #[inline]
  pub fn loop_<'a>(body: Block<'a>) -> StmtKind<'a> {
    StmtKind::Loop(Box::new(Loop::Inf(LoopInf { body })))
  }

  pub fn expr<'a>(expr: Expr<'a>) -> StmtKind<'a> {
    StmtKind::Expr(Box::new(expr))
  }
}

#[derive(Clone, DebugInner)]
pub enum TypeKind<'a> {
  Opt(Box<ty::Opt<'a>>),
  Path(Box<Path<'a>>),
  Fn(Box<ty::Fn<'a>>),
  Array(Box<ty::Array<'a>>),
  Tuple(Box<ty::Tuple<'a>>),
  Inst(Box<ty::Inst<'a>>),
  Field(Box<ty::Field<'a>>),
}

pub type Type<'a> = Spanned<TypeKind<'a>>;

pub mod ty {
  use super::{Ident, Path, Type, TypeKind};

  #[derive(Clone, Debug)]
  pub struct Opt<'a> {
    pub inner: Type<'a>,
  }

  #[inline]
  pub fn option<'a>(inner: Type<'a>) -> TypeKind<'a> {
    TypeKind::Opt(Box::new(Opt { inner }))
  }

  #[inline]
  pub fn path<'a>(path: Path<'a>) -> TypeKind<'a> {
    TypeKind::Path(Box::new(path))
  }

  #[derive(Clone, Debug)]
  pub struct Fn<'a> {
    pub params: Vec<Type<'a>>,
    pub ret: Option<Type<'a>>,
  }

  #[inline]
  pub fn fn_<'a>(params: Vec<Type<'a>>, ret: Option<Type<'a>>) -> TypeKind<'a> {
    TypeKind::Fn(Box::new(Fn { params, ret }))
  }

  #[derive(Clone, Debug)]
  pub struct Array<'a> {
    pub item: Type<'a>,
  }

  #[inline]
  pub fn array<'a>(item: Type<'a>) -> TypeKind<'a> {
    TypeKind::Array(Box::new(Array { item }))
  }

  #[derive(Clone, Debug)]
  pub struct Tuple<'a> {
    pub items: Vec<Type<'a>>,
  }

  #[inline]
  pub fn tuple<'a>(items: Vec<Type<'a>>) -> TypeKind<'a> {
    TypeKind::Tuple(Box::new(Tuple { items }))
  }

  #[derive(Clone, Debug)]
  pub struct Inst<'a> {
    pub cons: Type<'a>,
    pub args: Vec<Type<'a>>,
  }

  #[inline]
  pub fn inst<'a>(cons: Type<'a>, args: Vec<Type<'a>>) -> TypeKind<'a> {
    TypeKind::Inst(Box::new(Inst { cons, args }))
  }

  #[derive(Clone, Debug)]
  pub struct Field<'a> {
    pub ty: Type<'a>,
    pub name: Ident<'a>,
  }

  #[inline]
  pub fn field<'a>(ty: Type<'a>, name: Ident<'a>) -> TypeKind<'a> {
    TypeKind::Field(Box::new(Field { ty, name }))
  }
}

#[derive(Clone, Debug)]
pub struct Path<'a> {
  pub segments: Vec<Segment<'a>>,
}

#[derive(Clone, Debug)]
pub struct Segment<'a> {
  pub ident: Ident<'a>,
  pub generic_args: Vec<Type<'a>>,
}

#[derive(Clone, DebugInner)]
pub enum ExprKind<'a> {
  Ctrl(Box<expr::Ctrl<'a>>),
  Block(Box<expr::Block<'a>>),
  If(Box<expr::If<'a>>),
  Try(Box<expr::Try<'a>>),
  Spawn(Box<expr::Spawn<'a>>),
  Lambda(Box<expr::Lambda<'a>>),
  Range(Box<expr::Range<'a>>),
  Binary(Box<expr::Binary<'a>>),
  Unary(Box<expr::Unary<'a>>),
  Call(Box<expr::Call<'a>>),
  Cast(Box<expr::Cast<'a>>),
  Inst(Box<expr::Inst<'a>>),
  Class(Box<expr::Class<'a>>),
  GetVar(Box<expr::GetVar<'a>>),
  GetField(Box<expr::GetField<'a>>),
  GetIndex(Box<expr::GetIndex<'a>>),
  SetVar(Box<expr::SetVar<'a>>),
  SetField(Box<expr::SetField<'a>>),
  SetIndex(Box<expr::SetIndex<'a>>),
  Literal(Box<expr::Literal<'a>>),
}

pub type Expr<'a> = Spanned<ExprKind<'a>>;

pub mod expr {
  use super::{DebugInner, Expr, ExprKind, Ident, Path, Stmt, Type};

  /// A list of statements.
  ///
  /// e.g.:
  ///
  /// ```text
  /// {
  ///   a := 0;
  ///   b := 1;
  /// }
  /// ```
  #[derive(Clone, Debug, Default)]
  pub struct Block<'a> {
    pub items: Vec<Stmt<'a>>,
    pub last: Option<Expr<'a>>,
  }

  #[derive(Clone, Debug)]
  pub enum Ctrl<'a> {
    Return(Option<Expr<'a>>),
    Throw(Option<Expr<'a>>),
    Break,
    Continue,
  }

  #[inline]
  pub fn return_<'a>(value: Option<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Return(value)))
  }

  #[inline]
  pub fn throw_<'a>(value: Option<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Throw(value)))
  }

  #[inline]
  pub fn break_<'a>() -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Break))
  }

  #[inline]
  pub fn continue_<'a>() -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Continue))
  }

  #[inline]
  pub fn block<'a>(block: Block<'a>) -> ExprKind<'a> {
    ExprKind::Block(Box::new(block))
  }

  #[derive(Clone, Debug)]
  pub struct If<'a> {
    pub branches: Vec<(Expr<'a>, Block<'a>)>,
    pub else_: Option<Block<'a>>,
  }

  #[inline]
  pub fn if_<'a>(branches: Vec<(Expr<'a>, Block<'a>)>, else_: Option<Block<'a>>) -> ExprKind<'a> {
    ExprKind::If(Box::new(If { branches, else_ }))
  }

  #[derive(Clone, Debug)]
  pub struct Try<'a> {
    pub body: Block<'a>,
    pub catch: (Ident<'a>, Block<'a>),
  }

  #[inline]
  pub fn try_<'a>(body: Block<'a>, catch: (Ident<'a>, Block<'a>)) -> ExprKind<'a> {
    ExprKind::Try(Box::new(Try { body, catch }))
  }

  #[derive(Clone, DebugInner)]
  pub enum Spawn<'a> {
    Call(SpawnCall<'a>),
    Block(SpawnBlock<'a>),
  }

  #[derive(Clone, Debug)]
  pub struct SpawnCall<'a>(pub Call<'a>);

  #[inline]
  pub fn spawn_call<'a>(body: Call<'a>) -> ExprKind<'a> {
    ExprKind::Spawn(Box::new(Spawn::Call(SpawnCall(body))))
  }

  #[derive(Clone, Debug)]
  pub struct SpawnBlock<'a>(pub Block<'a>);

  #[inline]
  pub fn spawn_block<'a>(body: Block<'a>) -> ExprKind<'a> {
    ExprKind::Spawn(Box::new(Spawn::Block(SpawnBlock(body))))
  }

  #[derive(Clone, Debug)]
  pub struct Lambda<'a> {
    pub params: Vec<Ident<'a>>,
    pub body: Block<'a>,
  }

  #[inline]
  pub fn lambda<'a>(params: Vec<Ident<'a>>, body: Block<'a>) -> ExprKind<'a> {
    ExprKind::Lambda(Box::new(Lambda { params, body }))
  }

  #[derive(Clone, Debug)]
  pub enum Range<'a> {
    /// `..`
    RangeFull,
    /// `<expr> ..`
    /// `.. <expr>`
    /// `<expr> .. <expr>`
    RangeEx(Option<Expr<'a>>, Option<Expr<'a>>),
    /// `..= <expr>`
    /// `<expr> ..= <expr>`
    RangeInc(Option<Expr<'a>>, Expr<'a>),
  }

  #[inline]
  pub fn range_full<'a>() -> ExprKind<'a> {
    ExprKind::Range(Box::new(Range::RangeFull))
  }
  #[inline]
  pub fn range_exclusive<'a>(start: Option<Expr<'a>>, end: Option<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Range(Box::new(Range::RangeEx(start, end)))
  }

  #[inline]
  pub fn range_inclusive<'a>(start: Option<Expr<'a>>, end: Expr<'a>) -> ExprKind<'a> {
    ExprKind::Range(Box::new(Range::RangeInc(start, end)))
  }

  #[derive(Clone, Debug)]
  pub struct Binary<'a> {
    pub op: BinaryOp,
    pub left: Expr<'a>,
    pub right: Expr<'a>,
  }

  #[derive(Clone, Copy, Debug)]
  pub enum BinaryOp {
    NullOr,
    BoolOr,
    BoolAnd,
    Equals,
    NotEquals,
    CompareLess,
    CompareLessEq,
    CompareMore,
    CompareMoreEq,
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Power,
  }

  #[inline]
  pub fn binary<'a>(op: BinaryOp, left: Expr<'a>, right: Expr<'a>) -> ExprKind<'a> {
    ExprKind::Binary(Box::new(Binary { op, left, right }))
  }

  #[derive(Clone, Debug)]
  pub struct Unary<'a> {
    pub op: UnaryOp,
    pub inner: Expr<'a>,
  }

  #[derive(Clone, Copy, Debug)]
  pub enum UnaryOp {
    Invert,
    Negate,
  }

  #[inline]
  pub fn unary<'a>(op: UnaryOp, inner: Expr<'a>) -> ExprKind<'a> {
    ExprKind::Unary(Box::new(Unary { op, inner }))
  }

  #[derive(Clone, Debug)]
  pub struct Call<'a> {
    pub opt: bool,
    pub target: Expr<'a>,
    pub args: Vec<Expr<'a>>,
  }

  #[inline]
  pub fn call<'a>(opt: bool, target: Expr<'a>, args: Vec<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Call(Box::new(Call { opt, target, args }))
  }

  #[derive(Clone, Debug)]
  pub struct Cast<'a> {
    pub opt: bool,
    pub target: Expr<'a>,
    pub ty: Type<'a>,
  }

  #[inline]
  pub fn cast<'a>(opt: bool, target: Expr<'a>, ty: Type<'a>) -> ExprKind<'a> {
    ExprKind::Cast(Box::new(Cast { opt, target, ty }))
  }

  #[derive(Clone, Debug)]
  pub struct GetVar<'a> {
    pub name: Ident<'a>,
  }

  #[inline]
  pub fn get_var<'a>(name: Ident<'a>) -> ExprKind<'a> {
    ExprKind::GetVar(Box::new(GetVar { name }))
  }

  #[derive(Clone, Debug)]
  pub struct GetField<'a> {
    pub opt: bool,
    pub target: Expr<'a>,
    pub key: Ident<'a>,
  }

  #[inline]
  pub fn get_field<'a>(opt: bool, target: Expr<'a>, key: Ident<'a>) -> ExprKind<'a> {
    ExprKind::GetField(Box::new(GetField { opt, target, key }))
  }

  #[derive(Clone, Debug)]
  pub struct GetIndex<'a> {
    pub opt: bool,
    pub target: Expr<'a>,
    pub key: Expr<'a>,
  }

  #[inline]
  pub fn get_index<'a>(opt: bool, target: Expr<'a>, key: Expr<'a>) -> ExprKind<'a> {
    ExprKind::GetIndex(Box::new(GetIndex { opt, target, key }))
  }

  #[derive(Clone, Debug)]
  pub struct SetVar<'a> {
    pub name: Ident<'a>,
    pub value: Expr<'a>,
    pub op: Option<AssignOp>,
  }

  #[inline]
  pub fn set_var<'a>(name: Ident<'a>, value: Expr<'a>, op: Option<AssignOp>) -> ExprKind<'a> {
    ExprKind::SetVar(Box::new(SetVar { name, value, op }))
  }

  #[derive(Clone, Debug)]
  pub struct SetField<'a> {
    pub target: Expr<'a>,
    pub name: Ident<'a>,
    pub value: Expr<'a>,
    pub op: Option<AssignOp>,
  }

  #[inline]
  pub fn set_field<'a>(
    target: Expr<'a>,
    name: Ident<'a>,
    value: Expr<'a>,
    op: Option<AssignOp>,
  ) -> ExprKind<'a> {
    ExprKind::SetField(Box::new(SetField {
      target,
      name,
      value,
      op,
    }))
  }

  #[derive(Clone, Debug)]
  pub struct SetIndex<'a> {
    pub target: Expr<'a>,
    pub key: Expr<'a>,
    pub value: Expr<'a>,
    pub op: Option<AssignOp>,
  }

  #[inline]
  pub fn set_index<'a>(
    target: Expr<'a>,
    key: Expr<'a>,
    value: Expr<'a>,
    op: Option<AssignOp>,
  ) -> ExprKind<'a> {
    ExprKind::SetIndex(Box::new(SetIndex {
      target,
      key,
      value,
      op,
    }))
  }

  #[derive(Clone, Copy, Debug)]
  pub enum AssignOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Opt,
  }

  #[derive(Clone, Debug)]
  pub struct Inst<'a> {
    pub target: Expr<'a>,
    pub args: Vec<Type<'a>>,
  }

  pub fn inst<'a>(target: Expr<'a>, args: Vec<Type<'a>>) -> ExprKind<'a> {
    ExprKind::Inst(Box::new(Inst { target, args }))
  }

  #[derive(Clone, Debug)]
  pub struct Class<'a> {
    pub target: Path<'a>,
    pub fields: Vec<(Ident<'a>, Expr<'a>)>,
  }

  pub fn class<'a>(target: Path<'a>, fields: Vec<(Ident<'a>, Expr<'a>)>) -> ExprKind<'a> {
    ExprKind::Class(Box::new(Class { target, fields }))
  }

  #[derive(Clone, DebugInner)]
  pub enum Literal<'a> {
    Null(Null),
    Bool(Bool),
    Int(Int),
    Float(Float),
    Str(Str),
    Array(Array<'a>),
    Tuple(Tuple<'a>),
  }

  #[derive(Clone, Debug)]
  pub struct Null;

  #[derive(Clone, Copy, Debug)]
  pub struct Bool {
    pub value: bool,
  }

  #[derive(Clone, Copy, Debug)]
  pub struct Int {
    pub value: i32,
  }

  #[derive(Clone, Copy, Debug)]
  pub struct Float {
    pub value: f32,
  }

  #[derive(Clone, Debug)]
  pub struct Str {
    pub value: String,
  }

  #[derive(Clone, Debug)]
  pub enum Array<'a> {
    List(Vec<Expr<'a>>),
    Copy(Expr<'a>, Expr<'a>),
  }

  #[derive(Clone, Debug)]
  pub struct Tuple<'a> {
    pub fields: Vec<Expr<'a>>,
  }

  pub fn null<'a>() -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Null(Null)))
  }

  pub fn bool<'a>(value: bool) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Bool(Bool { value })))
  }

  pub fn int<'a>(value: i32) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Int(Int { value })))
  }

  pub fn float<'a>(value: f32) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Float(Float { value })))
  }

  pub fn string<'a>(value: impl Into<String>) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Str(Str {
      value: value.into(),
    })))
  }

  pub fn array_list<'a>(items: Vec<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Array(Array::List(items))))
  }

  pub fn array_copy<'a>(value: Expr<'a>, n: Expr<'a>) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Array(Array::Copy(value, n))))
  }

  pub fn tuple<'a>(fields: Vec<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Tuple(Tuple { fields })))
  }
}

// display impls

use std::fmt;

impl<'a> fmt::Display for TypeKind<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      TypeKind::Opt(ty) => write!(f, "{ty}"),
      TypeKind::Path(p) => write!(f, "{p}"),
      TypeKind::Fn(ty) => write!(f, "{ty}"),
      TypeKind::Array(ty) => write!(f, "{ty}"),
      TypeKind::Tuple(ty) => write!(f, "{ty}"),
      TypeKind::Inst(ty) => write!(f, "{ty}"),
      TypeKind::Field(ty) => write!(f, "{ty}"),
    }
  }
}

impl<'a> fmt::Display for ty::Opt<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "({})?", self.inner)
  }
}

impl<'a> fmt::Display for Path<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut iter = self.segments.iter().peekable();
    while let Some(segment) = iter.next() {
      write!(f, "{}", segment.ident)?;
      if !segment.generic_args.is_empty() {
        write!(f, "<")?;
        fmt_type_list(&segment.generic_args, f)?;
        write!(f, ">")?;
      }
      if iter.peek().is_some() {
        write!(f, ".")?;
      }
    }

    Ok(())
  }
}

impl<'a> fmt::Display for ty::Fn<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "fn")?;
    write!(f, "(")?;
    fmt_type_list(&self.params, f)?;
    write!(f, ")")?;
    if let Some(ret) = &self.ret {
      write!(f, "-> {}", ret)?;
    }

    Ok(())
  }
}

impl<'a> fmt::Display for ty::Array<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "[{}]", self.item)
  }
}

impl<'a> fmt::Display for ty::Tuple<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "(")?;
    fmt_type_list(&self.items, f)?;
    write!(f, ")")?;

    Ok(())
  }
}

impl<'a> fmt::Display for ty::Inst<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}<", self.cons)?;
    fmt_type_list(&self.args, f)?;
    write!(f, ">")?;

    Ok(())
  }
}

fn fmt_type_list(list: &[Type], f: &mut fmt::Formatter<'_>) -> fmt::Result {
  let mut iter = list.iter().peekable();
  while let Some(arg) = iter.next() {
    write!(f, "{arg}")?;
    if iter.peek().is_some() {
      write!(f, ", ")?;
    }
  }
  Ok(())
}

impl<'a> fmt::Display for ty::Field<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}.{}", self.ty, self.name)
  }
}
