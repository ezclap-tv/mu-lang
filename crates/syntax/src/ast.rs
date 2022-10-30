#![allow(clippy::needless_lifetimes)]

use indexmap::IndexMap;

use crate::lexer::Token;

// TODO: spans
// - Type
// - Stmt
// - Expr

/// Order-preserving hash map
pub type Map<'a, T> = IndexMap<Ident<'a>, T>;

/// Any kind of name containing alphanumeric characters, underscores, and
/// numbers: `a`, `_b`, `c_0`, etc.
///
/// Used to represent names.
///
/// May not start with a number.
pub type Ident<'a> = Token<'a>;

/// A period-separated list of identifiers: `a.b.c`
///
/// Used to specify where a symbol comes from if it isn't directly imported,
/// e.g.:
///
/// ```no_run
/// use module;
/// type Bar = module.Foo;
/// ```
pub type Path<'a> = Vec<Ident<'a>>;

/// A list of statements.
///
/// e.g.:
///
/// ```no_run
/// {
///   a := 0;
///   b := 1;
/// }
/// ```
pub type Block<'a> = Vec<Stmt<'a>>;

/// A module is the basic building block of Mu programs.
///
/// Each module contains a list of imports and exported symbols.
pub struct Module<'a> {
  pub imports: Vec<Import<'a>>,
  pub symbols: Map<'a, (Vis, Symbol<'a>)>,
}

/// A normalized import, which may optionally be renamed.
///
/// e.g.:
///
/// ```no_run
/// use {
///   a.{b, c as d},
///   e as f
/// }
/// ```
///
/// Will normalize to the following imports:
///
/// ```no_run
/// use a.b;
/// use a.c as d;
/// use e as f;
/// ```
pub struct Import<'a> {
  pub path: Path<'a>,
  pub as_: Option<Ident<'a>>,
}

/// A visibility modifier.
pub enum Vis {
  /// `pub`
  Public,
  /// There is no syntax for this, symbols are private by default.
  Private,
}

/// Symbols are static (lexically scoped) module items.
///
/// For more information about a specific symbol kind, see its inner type.
pub enum Symbol<'a> {
  Fn(Box<symbol::Fn<'a>>),
  Class(Box<symbol::Class<'a>>),
  Trait(Box<symbol::Trait<'a>>),
  Alias(Box<symbol::Alias<'a>>),
}

pub mod symbol {
  use super::{expr, Expr, Ident, Symbol, Type};

  /// Functions are bundles of code.
  ///
  /// ```no_run
  /// fn name[T, A, B, C](a: A, b: B, c: C) -> T
  /// where
  ///   A: Foo,
  ///   B: Bar[C],
  ///   C: Baz,
  /// {
  ///   // ...
  /// }
  /// ```
  pub struct Fn<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub params: Vec<Param<'a>>,
    pub ret: Type<'a>,
    pub bounds: Vec<Bound<'a>>,
    pub body: Option<expr::Do<'a>>,
  }

  /// Helper function to construct a boxed and wrapped `Fn`.
  #[inline]
  pub fn fn_<'a>(
    name: Ident<'a>,
    tparams: Vec<TParam<'a>>,
    params: Vec<Param<'a>>,
    ret: Type<'a>,
    bounds: Vec<Bound<'a>>,
    body: Option<expr::Do<'a>>,
  ) -> Symbol<'a> {
    Symbol::Fn(Box::new(Fn {
      name,
      tparams,
      params,
      ret,
      bounds,
      body,
    }))
  }

  /// Classes encapsulate data, and operations on that data.
  ///
  /// ```no_run
  /// class Foo[T] {
  ///   type Bar = T;
  ///   fn new() -> Self { /* ... */ }
  ///   impl Trait { /* ... */ }
  /// }
  /// ```
  pub struct Class<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub bounds: Vec<Bound<'a>>,
    pub members: Vec<ClassMember<'a>>,
  }

  /// Helper function to construct a boxed and wrapped `Class`.
  #[inline]
  pub fn class<'a>(
    name: Ident<'a>,
    tparams: Vec<TParam<'a>>,
    bounds: Vec<Bound<'a>>,
    members: Vec<ClassMember<'a>>,
  ) -> Symbol<'a> {
    Symbol::Class(Box::new(Class {
      name,
      tparams,
      bounds,
      members,
    }))
  }

  /// Traits encapsulate behavior.
  /// Anything that is generic can be bound to traits.
  ///
  /// ```no_run
  /// trait Foo[T] {
  ///   type Bar = T;
  ///   fn bar() -> Bar;
  /// }
  ///
  /// fn f[T](v: Foo[T]) -> Foo[T].Bar {
  ///   v.bar()
  /// }
  /// ```
  pub struct Trait<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub members: Vec<TraitMember<'a>>,
  }

  /// Helper function to construct a boxed and wrapped `Trait`.
  #[inline]
  pub fn trait_<'a>(
    name: Ident<'a>,
    tparams: Vec<TParam<'a>>,
    members: Vec<TraitMember<'a>>,
  ) -> Symbol<'a> {
    Symbol::Trait(Box::new(Trait {
      name,
      tparams,
      members,
    }))
  }

  /// A type alias allows renaming types and partially instantiating generic
  /// types.
  ///
  /// ```no_run
  /// class Foo[T];
  ///
  /// type Bar = Foo[Baz];
  /// ```
  pub struct Alias<'a> {
    pub name: Ident<'a>,
    pub tparams: Vec<TParam<'a>>,
    pub bounds: Vec<Bound<'a>>,
    pub ty: Type<'a>,
  }

  /// Helper function to construct a boxed and wrapped `Alias`.
  #[inline]
  pub fn alias<'a>(
    name: Ident<'a>,
    tparams: Vec<TParam<'a>>,
    bounds: Vec<Bound<'a>>,
    ty: Type<'a>,
  ) -> Symbol<'a> {
    Symbol::Alias(Box::new(Alias {
      name,
      tparams,
      bounds,
      ty,
    }))
  }

  pub struct Impl<'a> {
    pub tparams: Vec<TParam<'a>>,
    pub trait_: Type<'a>,
    pub members: Vec<TraitMember<'a>>,
  }

  pub struct TParam<'a> {
    pub name: Ident<'a>,
    pub default: Type<'a>,
  }

  pub struct Param<'a> {
    pub name: Ident<'a>,
    pub ty: Type<'a>,
    pub default: Option<Expr<'a>>,
  }

  pub struct Bound<'a> {
    pub ty: Type<'a>,
    pub constraint: Vec<Type<'a>>,
  }

  pub enum ClassMember<'a> {
    Field(Ident<'a>, Type<'a>),
    Fn(Fn<'a>),
    Alias(Alias<'a>),
    Impl(Impl<'a>),
  }

  pub enum TraitMember<'a> {
    Fn(Fn<'a>),
    Alias(Alias<'a>),
  }
}

pub enum Stmt<'a> {
  Var(Box<stmt::Var<'a>>),
  Loop(Box<stmt::Loop<'a>>),
  Expr(Box<Expr<'a>>),
}

pub mod stmt {
  use super::{Block, Expr, Ident, Stmt, Type};

  pub struct Var<'a> {
    pub name: Ident<'a>,
    pub ty: Option<Type<'a>>,
    pub value: Expr<'a>,
  }

  #[inline]
  pub fn var<'a>(name: Ident<'a>, ty: Option<Type<'a>>, value: Expr<'a>) -> Stmt<'a> {
    Stmt::Var(Box::new(Var { name, ty, value }))
  }

  pub enum Loop<'a> {
    For(For<'a>),
    While(While<'a>),
    Inf(Inf<'a>),
  }

  pub struct For<'a> {
    pub item: Ident<'a>,
    pub iter: Expr<'a>,
    pub body: Block<'a>,
  }

  #[inline]
  pub fn for_<'a>(item: Ident<'a>, iter: Expr<'a>, body: Block<'a>) -> Stmt<'a> {
    Stmt::Loop(Box::new(Loop::For(For { item, iter, body })))
  }

  pub struct While<'a> {
    pub cond: Expr<'a>,
    pub body: Block<'a>,
  }

  #[inline]
  pub fn while_<'a>(cond: Expr<'a>, body: Block<'a>) -> Stmt<'a> {
    Stmt::Loop(Box::new(Loop::While(While { cond, body })))
  }

  pub struct Inf<'a> {
    pub body: Block<'a>,
  }

  #[inline]
  pub fn loop_<'a>(body: Block<'a>) -> Stmt<'a> {
    Stmt::Loop(Box::new(Loop::Inf(Inf { body })))
  }
}

pub enum Type<'a> {
  Opt(Box<ty::Opt<'a>>),
  Fn(Box<ty::Fn<'a>>),
  Array(Box<ty::Array<'a>>),
  Tuple(Box<ty::Tuple<'a>>),
  Inst(Box<ty::Inst<'a>>),
  Var(Box<ty::Var<'a>>),
}

pub mod ty {
  use super::{Path, Type};

  pub struct Opt<'a> {
    pub inner: Type<'a>,
  }

  #[inline]
  pub fn option<'a>(inner: Type<'a>) -> Type<'a> {
    Type::Opt(Box::new(Opt { inner }))
  }

  pub struct Fn<'a> {
    pub params: Vec<Type<'a>>,
    pub ret: Type<'a>,
  }

  #[inline]
  pub fn fn_<'a>(params: Vec<Type<'a>>, ret: Type<'a>) -> Type<'a> {
    Type::Fn(Box::new(Fn { params, ret }))
  }

  pub struct Array<'a> {
    pub item: Type<'a>,
  }

  #[inline]
  pub fn array<'a>(item: Type<'a>) -> Type<'a> {
    Type::Array(Box::new(Array { item }))
  }

  pub struct Tuple<'a> {
    pub items: Vec<Type<'a>>,
  }

  #[inline]
  pub fn tuple<'a>(items: Vec<Type<'a>>) -> Type<'a> {
    Type::Tuple(Box::new(Tuple { items }))
  }

  pub struct Inst<'a> {
    pub cons: Type<'a>,
    pub args: Vec<Type<'a>>,
  }

  #[inline]
  pub fn inst<'a>(cons: Type<'a>, args: Vec<Type<'a>>) -> Type<'a> {
    Type::Inst(Box::new(Inst { cons, args }))
  }

  pub struct Var<'a> {
    pub path: Path<'a>,
  }

  #[inline]
  pub fn var<'a>(path: Path<'a>) -> Type<'a> {
    Type::Var(Box::new(Var { path }))
  }
}

pub enum Expr<'a> {
  Ctrl(Box<expr::Ctrl<'a>>),
  Do(Box<expr::Do<'a>>),
  If(Box<expr::If<'a>>),
  Try(Box<expr::Try<'a>>),
  Spawn(Box<expr::Spawn<'a>>),
  Lambda(Box<expr::Lambda<'a>>),
  Assign(Box<expr::Assign<'a>>),
  Range(Box<expr::Range<'a>>),
  Binary(Box<expr::Binary<'a>>),
  Unary(Box<expr::Unary<'a>>),
  Call(Box<expr::Call<'a>>),
  Field(Box<expr::Field<'a>>),
  Index(Box<expr::Index<'a>>),
  Var(Box<expr::Var<'a>>),
  Literal(Box<expr::Literal<'a>>),
}

pub mod expr {
  use super::{Block, Expr, Ident, Type};

  pub enum Ctrl<'a> {
    Return(Expr<'a>),
    Throw(Expr<'a>),
    Break,
    Continue,
  }

  #[inline]
  pub fn return_<'a>(inner: Expr<'a>) -> Expr<'a> {
    Expr::Ctrl(Box::new(Ctrl::Return(inner)))
  }

  #[inline]
  pub fn throw_<'a>(inner: Expr<'a>) -> Expr<'a> {
    Expr::Ctrl(Box::new(Ctrl::Throw(inner)))
  }

  #[inline]
  pub fn break_<'a>() -> Expr<'a> {
    Expr::Ctrl(Box::new(Ctrl::Break))
  }

  #[inline]
  pub fn continue_<'a>() -> Expr<'a> {
    Expr::Ctrl(Box::new(Ctrl::Continue))
  }

  pub struct Do<'a> {
    pub body: Block<'a>,
    pub value: Option<Expr<'a>>,
  }

  #[inline]
  pub fn do_<'a>(body: Block<'a>, value: Option<Expr<'a>>) -> Expr<'a> {
    Expr::Do(Box::new(Do { body, value }))
  }

  pub struct If<'a> {
    pub branches: Vec<(Expr<'a>, Do<'a>)>,
    pub else_: Option<Do<'a>>,
  }

  #[inline]
  pub fn if_<'a>(branches: Vec<(Expr<'a>, Do<'a>)>, else_: Option<Do<'a>>) -> Expr<'a> {
    Expr::If(Box::new(If { branches, else_ }))
  }

  pub struct Try<'a> {
    pub body: TryKind<'a>,
    pub branches: Vec<(Type<'a>, Do<'a>)>,
  }

  pub enum TryKind<'a> {
    Expr(Expr<'a>),
    Block(Do<'a>),
  }

  #[inline]
  pub fn try_expr<'a>(body: Expr<'a>, branches: Vec<(Type<'a>, Do<'a>)>) -> Expr<'a> {
    Expr::Try(Box::new(Try {
      body: TryKind::Expr(body),
      branches,
    }))
  }

  #[inline]
  pub fn try_block<'a>(body: Do<'a>, branches: Vec<(Type<'a>, Do<'a>)>) -> Expr<'a> {
    Expr::Try(Box::new(Try {
      body: TryKind::Block(body),
      branches,
    }))
  }

  pub enum Spawn<'a> {
    Call(Call<'a>),
    Block(Do<'a>),
  }

  #[inline]
  pub fn spawn_call<'a>(body: Call<'a>) -> Expr<'a> {
    Expr::Spawn(Box::new(Spawn::Call(body)))
  }

  #[inline]
  pub fn spawn_block<'a>(body: Do<'a>) -> Expr<'a> {
    Expr::Spawn(Box::new(Spawn::Block(body)))
  }

  pub struct Lambda<'a> {
    pub params: Vec<(Ident<'a>, Type<'a>)>,
    pub body: Do<'a>,
  }

  #[inline]
  pub fn lambda<'a>(params: Vec<(Ident<'a>, Type<'a>)>, body: Do<'a>) -> Expr<'a> {
    Expr::Lambda(Box::new(Lambda { params, body }))
  }

  pub struct Assign<'a> {
    pub target: Expr<'a>,
    pub value: Expr<'a>,
  }

  #[inline]
  pub fn assign<'a>(target: Expr<'a>, value: Expr<'a>) -> Expr<'a> {
    Expr::Assign(Box::new(Assign { target, value }))
  }

  pub struct Range<'a> {
    pub start: Expr<'a>,
    pub end: Expr<'a>,
  }

  #[inline]
  pub fn range<'a>(start: Expr<'a>, end: Expr<'a>) -> Expr<'a> {
    Expr::Range(Box::new(Range { start, end }))
  }

  pub struct Binary<'a> {
    pub op: BinaryOp,
    pub left: Expr<'a>,
    pub right: Expr<'a>,
  }

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
    Mult,
    Div,
    Rem,
    Pow,
  }

  #[inline]
  pub fn binary<'a>(op: BinaryOp, left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
    Expr::Binary(Box::new(Binary { op, left, right }))
  }

  pub struct Unary<'a> {
    pub op: UnaryOp,
    pub inner: Expr<'a>,
  }

  pub enum UnaryOp {
    Neg,
    Not,
  }

  #[inline]
  pub fn unary<'a>(op: UnaryOp, inner: Expr<'a>) -> Expr<'a> {
    Expr::Unary(Box::new(Unary { op, inner }))
  }

  pub struct Call<'a> {
    pub opt: bool,
    pub target: Expr<'a>,
    pub args: Vec<Expr<'a>>,
  }

  #[inline]
  pub fn call<'a>(opt: bool, target: Expr<'a>, args: Vec<Expr<'a>>) -> Expr<'a> {
    Expr::Call(Box::new(Call { opt, target, args }))
  }

  pub struct Field<'a> {
    pub opt: bool,
    pub target: Expr<'a>,
    pub name: Ident<'a>,
  }

  #[inline]
  pub fn field<'a>(opt: bool, target: Expr<'a>, name: Ident<'a>) -> Expr<'a> {
    Expr::Field(Box::new(Field { opt, target, name }))
  }

  pub struct Index<'a> {
    pub opt: bool,
    pub target: Expr<'a>,
    pub key: Expr<'a>,
  }

  #[inline]
  pub fn index<'a>(opt: bool, target: Expr<'a>, key: Expr<'a>) -> Expr<'a> {
    Expr::Index(Box::new(Index { opt, target, key }))
  }

  pub struct Var<'a> {
    pub name: Ident<'a>,
  }

  pub enum Literal<'a> {
    Null,
    Bool(Bool),
    Int(Int),
    Float(Float),
    String(String<'a>),
    Array(Array<'a>),
    Tuple(Tuple<'a>),
  }

  pub struct Bool {
    pub value: bool,
  }

  pub struct Int {
    pub value: i32,
  }

  pub struct Float {
    pub value: f32,
  }

  pub struct String<'a> {
    pub fragments: Vec<Frag<'a>>,
  }

  pub enum Frag<'a> {
    Str(Ident<'a>),
    Expr(Expr<'a>),
  }

  pub enum Array<'a> {
    List(Vec<Expr<'a>>),
    Copy(Expr<'a>, usize),
  }

  pub struct Tuple<'a> {
    pub fields: Vec<Expr<'a>>,
  }

  pub fn null<'a>() -> Expr<'a> {
    Expr::Literal(Box::new(Literal::Null))
  }

  pub fn bool<'a>(value: bool) -> Expr<'a> {
    Expr::Literal(Box::new(Literal::Bool(Bool { value })))
  }

  pub fn int<'a>(value: i32) -> Expr<'a> {
    Expr::Literal(Box::new(Literal::Int(Int { value })))
  }

  pub fn float<'a>(value: f32) -> Expr<'a> {
    Expr::Literal(Box::new(Literal::Float(Float { value })))
  }

  pub fn string<'a>(fragments: Vec<Frag<'a>>) -> Expr<'a> {
    Expr::Literal(Box::new(Literal::String(String { fragments })))
  }

  pub fn array_list<'a>(items: Vec<Expr<'a>>) -> Expr<'a> {
    Expr::Literal(Box::new(Literal::Array(Array::List(items))))
  }

  pub fn array_copy<'a>(value: Expr<'a>, n: usize) -> Expr<'a> {
    Expr::Literal(Box::new(Literal::Array(Array::Copy(value, n))))
  }

  pub fn tuple<'a>(fields: Vec<Expr<'a>>) -> Expr<'a> {
    Expr::Literal(Box::new(Literal::Tuple(Tuple { fields })))
  }
}
