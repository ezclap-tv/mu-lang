use std::borrow::Cow;

use serde::{Deserialize, Serialize};

use crate::lexer::Token;
use crate::span::Spanned;

pub type Program<'a> = Vec<Expr<'a>>;

pub type Expr<'a> = Spanned<ExprKind<'a>>;
pub type Type<'a> = Spanned<TypeKind<'a>>;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ExprKind<'a> {
  Lit(Lit<'a>),
  Let(Box<Let<'a>>),
  Call(Box<Call<'a>>),
  Use(Use<'a>),
  If(Box<If<'a>>),
  Access(Box<Access<'a>>),
  Binary(Box<Binary<'a>>),
  Unary(Box<Unary<'a>>),
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
  pub value: Expr<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Let<'a> {
  pub inner: LetKind<'a>,
  #[serde(rename = "in")]
  pub in_: Option<Expr<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LetKind<'a> {
  Var(Var<'a>),
  Func(Func<'a>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Call<'a> {
  pub func: Expr<'a>,
  pub arg: Expr<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Use<'a> {
  pub ident: Token<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct If<'a> {
  pub cond: Expr<'a>,
  pub then: Expr<'a>,
  #[serde(rename = "else")]
  pub else_: Expr<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Access<'a> {
  pub target: Expr<'a>,
  pub field: Token<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Binary<'a> {
  pub op: BinaryOp,
  pub lhs: Expr<'a>,
  pub rhs: Expr<'a>,
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
  pub rhs: Expr<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum UnaryOp {
  Negate,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Var<'a> {
  pub ident: Token<'a>,
  pub value: Expr<'a>,
  #[serde(rename = "type")]
  pub type_: Option<Type<'a>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Func<'a> {
  pub ident: Token<'a>,
  pub param: (Token<'a>, Type<'a>),
  pub ret: Type<'a>,
  pub body: Expr<'a>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TypeKind<'a> {
  Ident(#[serde(rename = "ident")] Token<'a>),
  Record(#[serde(rename = "fields")] Vec<(Token<'a>, Type<'a>)>),
}
