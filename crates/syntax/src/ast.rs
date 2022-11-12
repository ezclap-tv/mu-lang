//! This module contains all the type definitions that constitute Mu's
//! [abstract syntax tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree)
//! (AST).
//!
//! The entrypoint for the AST is [Module][`crate::ast::Module`].
//!
//! Developer notes for working with spans in this module:
//! - Use `T` wrapped in [Spanned][`crate::span::Spanned`] instead of `T` when
//!   the full span of the syntax node cannot be reconstructed from its fields
//!   alone. The Span inside of `Spanned` should always encapsulate the entire
//!   syntax node.
//! - Add a `span` ([Span][`crate::span::Span`]) field when it doesn't make
//!   sense for the span to encapsulate the entire syntax node. One such case is
//!   imports, which can be nested, so the resulting span would look quite
//!   awkward, taking up multiple lines, but none of them quite fully.
//! - If you need to add a span to an enum, add a `Kind` suffix to its name, and
//!   create a type alias for the original name which wraps the enum in
//!   [Spanned][`crate::span::Spanned`]. For example:
//!
//! ```text
//! enum Stmt<'a> { ... }
//! // the above becomes
//! enum StmtKind<'a> { ... }
//! type Stmt<'a> = Spanned<StmtKind<'a>>;
//! ```
#![allow(clippy::needless_lifetimes)]

use std::borrow::Cow;
use std::ops::Deref;

use indexmap::IndexMap;

use crate::lexer::Token;
use crate::span::{Span, Spanned};

// TODO: spans
// - Type
// - Stmt
// - Expr

/// Any kind of name containing alphanumeric characters, underscores, and
/// numbers: `a`, `_b`, `c_0`, etc. It is invalid for an identifier to start
/// with a digit, but internally, they may be made up of arbitrary UTF8 text.
pub type Ident<'a> = Spanned<Cow<'a, str>>;

/// Converts a `Token` to an identifier. Only use this if `token.kind ==
/// TokenKind::Ident`.
pub fn ident<'a>(token: &Token<'a>) -> Ident<'a> {
  Spanned::new(token.span, token.lexeme.clone())
}

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
  pub fns: Map<'a, symbol::Fn<'a>>,
  pub classes: Map<'a, symbol::Class<'a>>,
  pub traits: Map<'a, symbol::Trait<'a>>,
  pub aliases: Map<'a, symbol::Alias<'a>>,
}

impl<'a> Symbols<'a> {
  // Helper function to simplify adding `Fn` symbols
  #[allow(clippy::too_many_arguments)]
  #[inline]
  pub fn insert_fn(
    &mut self,
    name: Ident<'a>,
    tparams: Vec<symbol::TParam<'a>>,
    params: Vec<symbol::Param<'a>>,
    ret: Option<Type<'a>>,
    bounds: Vec<symbol::Bound<'a>>,
    body: Option<expr::Block<'a>>,
    span: Span,
  ) {
    self.fns.insert(
      name.deref().clone(),
      symbol::Fn {
        name,
        tparams,
        params,
        ret,
        bounds,
        body,
        span,
      },
    );
  }

  // Helper function to simplify adding `Class` symbols
  #[inline]
  pub fn insert_class(
    &mut self,
    name: Ident<'a>,
    tparams: Vec<symbol::TParam<'a>>,
    bounds: Vec<symbol::Bound<'a>>,
    members: Vec<symbol::ClassMember<'a>>,
    span: Span,
  ) {
    self.classes.insert(
      name.deref().clone(),
      symbol::Class {
        name,
        tparams,
        bounds,
        members,
        span,
      },
    );
  }

  // Helper function to simplify adding `Trait` symbols
  #[inline]
  pub fn insert_trait(
    &mut self,
    name: Ident<'a>,
    tparams: Vec<symbol::TParam<'a>>,
    members: Vec<symbol::TraitMember<'a>>,
    span: Span,
  ) {
    self.traits.insert(
      name.deref().clone(),
      symbol::Trait {
        name,
        tparams,
        members,
        span,
      },
    );
  }

  // Helper function to simplify adding `Alias` symbols
  #[inline]
  pub fn insert_alias(
    &mut self,
    name: Ident<'a>,
    tparams: Vec<symbol::TParam<'a>>,
    bounds: Vec<symbol::Bound<'a>>,
    ty: Type<'a>,
    span: Span,
  ) {
    self.aliases.insert(
      name.deref().clone(),
      symbol::Alias {
        name,
        tparams,
        bounds,
        ty,
        span,
      },
    );
  }
}

pub mod symbol {
  use super::{expr, Expr, Ident, Span, Type};

  /// A visibility modifier.
  #[derive(Clone, Copy, Debug)]
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
    pub span: Span,
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
    pub members: Vec<ClassMember<'a>>,
    pub span: Span,
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
    pub members: Vec<TraitMember<'a>>,
    pub span: Span,
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
    pub span: Span,
  }

  #[derive(Clone, Debug)]
  pub struct Impl<'a> {
    pub tparams: Vec<TParam<'a>>,
    pub trait_: Type<'a>,
    pub members: Vec<TraitMember<'a>>,
    pub span: Span,
  }

  #[derive(Clone, Debug)]
  pub struct TParam<'a> {
    pub name: Ident<'a>,
    pub default: Option<Type<'a>>,
  }

  #[derive(Clone, Debug)]
  pub struct Param<'a> {
    pub name: Ident<'a>,
    pub ty: Type<'a>,
    pub default: Option<Expr<'a>>,
  }

  #[derive(Clone, Debug)]
  pub struct Bound<'a> {
    pub ty: Type<'a>,
    pub constraint: Vec<Type<'a>>,
  }

  #[derive(Clone, Debug)]
  pub enum ClassMember<'a> {
    Field(Ident<'a>, Type<'a>),
    Fn(Fn<'a>),
    Alias(Alias<'a>),
    Impl(Impl<'a>),
  }

  #[derive(Clone, Debug)]
  pub enum TraitMember<'a> {
    Fn(Fn<'a>),
    Alias(Alias<'a>),
  }
}

#[derive(Clone, Debug)]
pub enum StmtKind<'a> {
  Let(Box<stmt::Let<'a>>),
  Loop(Box<stmt::Loop<'a>>),
  Expr(Box<Expr<'a>>),
}

pub type Stmt<'a> = Spanned<StmtKind<'a>>;

pub mod stmt {
  use super::expr::Block;
  use super::{Expr, Ident, StmtKind, Type};

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

  #[derive(Clone, Debug)]
  pub enum Loop<'a> {
    For(For<'a>),
    While(While<'a>),
    Inf(Inf<'a>),
  }

  #[derive(Clone, Debug)]
  pub struct For<'a> {
    pub item: Ident<'a>,
    pub iter: Expr<'a>,
    pub body: Block<'a>,
  }

  #[inline]
  pub fn for_<'a>(item: Ident<'a>, iter: Expr<'a>, body: Block<'a>) -> StmtKind<'a> {
    StmtKind::Loop(Box::new(Loop::For(For { item, iter, body })))
  }

  #[derive(Clone, Debug)]
  pub struct While<'a> {
    pub cond: Expr<'a>,
    pub body: Block<'a>,
  }

  #[inline]
  pub fn while_<'a>(cond: Expr<'a>, body: Block<'a>) -> StmtKind<'a> {
    StmtKind::Loop(Box::new(Loop::While(While { cond, body })))
  }

  #[derive(Clone, Debug)]
  pub struct Inf<'a> {
    pub body: Block<'a>,
  }

  #[inline]
  pub fn loop_<'a>(body: Block<'a>) -> StmtKind<'a> {
    StmtKind::Loop(Box::new(Loop::Inf(Inf { body })))
  }

  pub fn expr<'a>(expr: Expr<'a>) -> StmtKind<'a> {
    StmtKind::Expr(Box::new(expr))
  }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum ExprKind<'a> {
  Ctrl(Box<expr::Ctrl<'a>>),
  Do(Box<expr::Block<'a>>),
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
  use super::{Expr, ExprKind, Ident, Path, Stmt, Type};

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
  pub fn do_<'a>(block: Block<'a>) -> ExprKind<'a> {
    ExprKind::Do(Box::new(block))
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

  #[derive(Clone, Debug)]
  pub enum Spawn<'a> {
    Call(Call<'a>),
    Block(Block<'a>),
  }

  #[inline]
  pub fn spawn_call<'a>(body: Call<'a>) -> ExprKind<'a> {
    ExprKind::Spawn(Box::new(Spawn::Call(body)))
  }

  #[inline]
  pub fn spawn_block<'a>(body: Block<'a>) -> ExprKind<'a> {
    ExprKind::Spawn(Box::new(Spawn::Block(body)))
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
  pub struct Range<'a> {
    pub start: Expr<'a>,
    pub end: Expr<'a>,
  }

  #[inline]
  pub fn range<'a>(start: Expr<'a>, end: Expr<'a>) -> ExprKind<'a> {
    ExprKind::Range(Box::new(Range { start, end }))
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

  #[derive(Clone, Debug)]
  pub enum Literal<'a> {
    Null,
    Bool(Bool),
    Int(Int),
    Float(Float),
    Str(Str),
    Array(Array<'a>),
    Tuple(Tuple<'a>),
  }

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
    ExprKind::Literal(Box::new(Literal::Null))
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
