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
  Spanned::new(token.lexeme.clone(), token.span)
}

/// Order-preserving hash map
pub type Map<'a, T> = IndexMap<Cow<'a, str>, T>;

/// A period-separated list of identifiers: `a.b.c`
///
/// Used to specify where a symbol comes from if it isn't directly imported,
/// e.g.:
///
/// ```text
/// use module;
/// type Bar = module.Foo;
/// ```
pub type Path<'a> = Spanned<Vec<Ident<'a>>>;

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
pub type Block<'a> = Spanned<Vec<StmtKind<'a>>>;

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
#[derive(Clone, Debug)]
pub struct Import<'a> {
  /// Normalized path to the symbol or module.
  pub path: Vec<Cow<'a, str>>,
  /// Imports may be renamed.
  pub alias: Option<Ident<'a>>,
  /// Span of the last identifier in the import path pre-normalization.
  ///
  /// ```text
  /// use a.b.thing as other;
  ///      // ^^^^^
  ///      // span
  ///
  /// use task.{
  ///   join,
  /// //^^^^ span
  ///   race,
  /// //^^^^ span
  /// };
  /// ```
  pub span: Span,
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
    body: Option<expr::Do<'a>>,
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
    pub body: Option<expr::Do<'a>>,
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
  use super::{Block, Expr, Ident, StmtKind, Type};

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
}

#[derive(Clone, Debug)]
pub enum TypeKind<'a> {
  Opt(Box<ty::Opt<'a>>),
  Fn(Box<ty::Fn<'a>>),
  Array(Box<ty::Array<'a>>),
  Tuple(Box<ty::Tuple<'a>>),
  Inst(Box<ty::Inst<'a>>),
  Field(Box<ty::Field<'a>>),
  Var(Box<ty::Var<'a>>),
}

pub type Type<'a> = Spanned<TypeKind<'a>>;

pub mod ty {
  use super::{Ident, Type, TypeKind};

  #[derive(Clone, Debug)]
  pub struct Opt<'a> {
    pub inner: Type<'a>,
  }

  #[inline]
  pub fn option<'a>(inner: Type<'a>) -> TypeKind<'a> {
    TypeKind::Opt(Box::new(Opt { inner }))
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

  #[derive(Clone, Debug)]
  pub struct Var<'a> {
    pub name: Ident<'a>,
  }

  #[inline]
  pub fn var<'a>(name: Ident<'a>) -> TypeKind<'a> {
    TypeKind::Var(Box::new(Var { name }))
  }
}

#[derive(Clone, Debug)]
pub enum ExprKind<'a> {
  Ctrl(Box<expr::Ctrl<'a>>),
  Do(Box<expr::Do<'a>>),
  If(Box<expr::If<'a>>),
  Try(Box<expr::Try<'a>>),
  Spawn(Box<expr::Spawn<'a>>),
  Lambda(Box<expr::Lambda<'a>>),
  Range(Box<expr::Range<'a>>),
  Binary(Box<expr::Binary<'a>>),
  Unary(Box<expr::Unary<'a>>),
  Call(Box<expr::Call<'a>>),
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
  use super::{Block, Expr, ExprKind, Ident, Type};

  #[derive(Clone, Debug)]
  pub enum Ctrl<'a> {
    Return(Expr<'a>),
    Throw(Expr<'a>),
    Break,
    Continue,
  }

  #[inline]
  pub fn return_<'a>(inner: Expr<'a>) -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Return(inner)))
  }

  #[inline]
  pub fn throw_<'a>(inner: Expr<'a>) -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Throw(inner)))
  }

  #[inline]
  pub fn break_<'a>() -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Break))
  }

  #[inline]
  pub fn continue_<'a>() -> ExprKind<'a> {
    ExprKind::Ctrl(Box::new(Ctrl::Continue))
  }

  #[derive(Clone, Debug)]
  pub struct Do<'a> {
    pub body: Block<'a>,
    pub value: Option<Expr<'a>>,
  }

  #[inline]
  pub fn do_<'a>(body: Block<'a>, value: Option<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Do(Box::new(Do { body, value }))
  }

  #[derive(Clone, Debug)]
  pub struct If<'a> {
    pub branches: Vec<(Expr<'a>, Do<'a>)>,
    pub else_: Option<Do<'a>>,
  }

  #[inline]
  pub fn if_<'a>(branches: Vec<(Expr<'a>, Do<'a>)>, else_: Option<Do<'a>>) -> ExprKind<'a> {
    ExprKind::If(Box::new(If { branches, else_ }))
  }

  #[derive(Clone, Debug)]
  pub struct Try<'a> {
    pub body: TryKind<'a>,
    pub branches: Vec<(Type<'a>, Do<'a>)>,
  }

  #[derive(Clone, Debug)]
  pub enum TryKind<'a> {
    Expr(Expr<'a>),
    Block(Do<'a>),
  }

  #[inline]
  pub fn try_expr<'a>(body: Expr<'a>, branches: Vec<(Type<'a>, Do<'a>)>) -> ExprKind<'a> {
    ExprKind::Try(Box::new(Try {
      body: TryKind::Expr(body),
      branches,
    }))
  }

  #[inline]
  pub fn try_block<'a>(body: Do<'a>, branches: Vec<(Type<'a>, Do<'a>)>) -> ExprKind<'a> {
    ExprKind::Try(Box::new(Try {
      body: TryKind::Block(body),
      branches,
    }))
  }

  #[derive(Clone, Debug)]
  pub enum Spawn<'a> {
    Call(Call<'a>),
    Block(Do<'a>),
  }

  #[inline]
  pub fn spawn_call<'a>(body: Call<'a>) -> ExprKind<'a> {
    ExprKind::Spawn(Box::new(Spawn::Call(body)))
  }

  #[inline]
  pub fn spawn_block<'a>(body: Do<'a>) -> ExprKind<'a> {
    ExprKind::Spawn(Box::new(Spawn::Block(body)))
  }

  #[derive(Clone, Debug)]
  pub struct Lambda<'a> {
    pub params: Vec<(Ident<'a>, Type<'a>)>,
    pub body: Do<'a>,
  }

  #[inline]
  pub fn lambda<'a>(params: Vec<(Ident<'a>, Type<'a>)>, body: Do<'a>) -> ExprKind<'a> {
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
    Opt,
    Or,
    And,
    Eq,
    Neq,
    More,
    Less,
    MoreEq,
    LessEq,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
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
    Neg,
    Not,
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
    pub name: Ident<'a>,
  }

  #[inline]
  pub fn get_field<'a>(opt: bool, target: Expr<'a>, name: Ident<'a>) -> ExprKind<'a> {
    ExprKind::GetField(Box::new(GetField { opt, target, name }))
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
  pub enum Literal<'a> {
    Null,
    Bool(Bool),
    Int(Int),
    Float(Float),
    String(String<'a>),
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
  pub struct String<'a> {
    pub fragments: Vec<Frag<'a>>,
  }

  #[derive(Clone, Debug)]
  pub enum Frag<'a> {
    Str(Ident<'a>),
    Expr(Expr<'a>),
  }

  #[derive(Clone, Debug)]
  pub enum Array<'a> {
    List(Vec<Expr<'a>>),
    Copy(Expr<'a>, usize),
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

  pub fn string<'a>(fragments: Vec<Frag<'a>>) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::String(String { fragments })))
  }

  pub fn array_list<'a>(items: Vec<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Array(Array::List(items))))
  }

  pub fn array_copy<'a>(value: Expr<'a>, n: usize) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Array(Array::Copy(value, n))))
  }

  pub fn tuple<'a>(fields: Vec<Expr<'a>>) -> ExprKind<'a> {
    ExprKind::Literal(Box::new(Literal::Tuple(Tuple { fields })))
  }
}
