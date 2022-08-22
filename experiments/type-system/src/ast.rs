use std::borrow::Cow;

use ast2str::AstToStr;

use crate::lexer::Token;

#[derive(Clone, Debug, AstToStr)]
pub enum Expr<'a> {
  Lit(#[forward] Lit<'a>),
  Let(#[forward] Let<'a>),
  Call(#[forward] Call<'a>),
  Use(#[forward] Use<'a>),
  If(#[forward] If<'a>),
  /* Assign(Assign<'a>), */
  Access(#[forward] Access<'a>),
  Binary(#[forward] Binary<'a>),
  Unary(#[forward] Unary<'a>),
}

#[derive(Clone, Debug, AstToStr)]
pub enum Lit<'a> {
  Number(#[rename = "value"] i64),
  Bool(#[rename = "value"] bool),
  String(#[rename = "value"] Cow<'a, str>),
  Record(#[rename = "fields"] Vec<Field<'a>>),
}

#[derive(Clone, Debug, AstToStr)]
pub struct Field<'a> {
  pub ident: Token<'a>,
  pub value: Box<Expr<'a>>,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Let<'a> {
  pub kind: LetKind<'a>,
  #[rename = "in"]
  pub in_: Option<Box<Expr<'a>>>,
}

#[derive(Clone, Debug, AstToStr)]
pub enum LetKind<'a> {
  Var(#[forward] Var<'a>),
  Func(#[forward] Func<'a>),
}

#[derive(Clone, Debug, AstToStr)]
pub struct Call<'a> {
  pub func: Box<Expr<'a>>,
  pub arg: Box<Expr<'a>>,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Use<'a> {
  pub ident: Token<'a>,
}

#[derive(Clone, Debug, AstToStr)]
pub struct If<'a> {
  pub cond: Box<Expr<'a>>,
  pub then: Box<Expr<'a>>,
  #[rename = "else"]
  pub else_: Box<Expr<'a>>,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Assign<'a> {
  pub lhs: Box<Expr<'a>>,
  pub rhs: Box<Expr<'a>>,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Access<'a> {
  pub target: Box<Expr<'a>>,
  pub field: Token<'a>,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Binary<'a> {
  pub op: BinaryOp,
  pub lhs: Box<Expr<'a>>,
  pub rhs: Box<Expr<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, AstToStr)]
pub enum BinaryOp {
  Add,
  Sub,
  Mult,
  Div,
  Rem,
  Power,
  And,
  Or,
  Equal,
  NotEqual,
  LessThan,
  LessEqual,
  GreaterEqual,
  GreaterThan,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Unary<'a> {
  pub op: UnaryOp,
  pub rhs: Box<Expr<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, AstToStr)]
pub enum UnaryOp {
  Not,
  Negate,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Var<'a> {
  pub ident: Token<'a>,
  pub value: Box<Expr<'a>>,
  #[rename = "type"]
  pub type_: Option<Type<'a>>,
}

#[derive(Clone, Debug, AstToStr)]
pub struct Func<'a> {
  pub ident: Token<'a>,
  pub param: (Token<'a>, Type<'a>),
  pub ret: Type<'a>,
  pub body: Box<Expr<'a>>,
}

#[derive(Clone, Debug, AstToStr)]
pub enum Type<'a> {
  Ident(#[rename = "ident"] Token<'a>),
  Record(#[rename = "fields"] Vec<(Token<'a>, Box<Type<'a>>)>),
}
