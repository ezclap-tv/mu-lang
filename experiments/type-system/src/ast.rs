use std::borrow::Cow;

use serde::{Deserialize, Serialize};

use crate::lexer::Token;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Expr<'a> {
  Lit(Lit<'a>),
  Let(Let<'a>),
  Call(Call<'a>),
  Use(Use<'a>),
  If(If<'a>),
  /* Assign(Assign<'a>), */
  Access(Access<'a>),
  Binary(Binary<'a>),
  Unary(Unary<'a>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Lit<'a> {
  Number(#[serde(rename = "value")] i64),
  Bool(#[serde(rename = "value")] bool),
  String(#[serde(rename = "value")] Cow<'a, str>),
  Record(#[serde(rename = "fields")] Vec<Field<'a>>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Field<'a> {
  pub ident: Token<'a>,
  pub value: Box<Expr<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Let<'a> {
  pub kind: LetKind<'a>,
  #[serde(rename = "in")]
  pub in_: Option<Box<Expr<'a>>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LetKind<'a> {
  Var(Var<'a>),
  Func(Func<'a>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Call<'a> {
  pub func: Box<Expr<'a>>,
  pub arg: Box<Expr<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Use<'a> {
  pub ident: Token<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct If<'a> {
  pub cond: Box<Expr<'a>>,
  pub then: Box<Expr<'a>>,
  #[serde(rename = "else")]
  pub else_: Option<Box<Expr<'a>>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Assign<'a> {
  pub lhs: Box<Expr<'a>>,
  pub rhs: Box<Expr<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Access<'a> {
  pub target: Box<Expr<'a>>,
  pub field: Token<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Binary<'a> {
  pub op: BinaryOp,
  pub lhs: Box<Expr<'a>>,
  pub rhs: Box<Expr<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Unary<'a> {
  pub op: UnaryOp,
  pub rhs: Box<Expr<'a>>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
  Not,
  Negate,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Var<'a> {
  pub ident: Token<'a>,
  pub value: Box<Expr<'a>>,
  #[serde(rename = "type")]
  pub type_: Option<Type<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Func<'a> {
  pub ident: Token<'a>,
  pub param: (Token<'a>, Type<'a>),
  pub ret: Type<'a>,
  pub body: Box<Expr<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Type<'a> {
  Ident(#[serde(rename = "ident")] Token<'a>),
  Record(#[serde(rename = "fields")] Vec<(Token<'a>, Box<Type<'a>>)>),
}
