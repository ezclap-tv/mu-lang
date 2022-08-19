#![feature(allocator_api)]

pub mod ast_arena;
pub mod dynamic_arena;

pub use ast2str::{self, AstToStr};
pub use ast_arena::{Arena, AstPtr, AstVec};
pub use mu_lexer::{Span, Token, TokenKind};

#[macro_export]
macro_rules! Node {
  ($name:ident) => {
    $name<'arena, 't>
  };
  ($name:ty) => {
    $name
  };
}

#[macro_export]
macro_rules! HeapNode {
  ($name:ident) => {
    AstPtr<'arena, Node![$name]>
  };
  ($name:ty) => {
    AstPtr<'arena, Node![$name]>
  };
}

#[macro_export]
macro_rules! Vec {
  ($name:ident) => {
    AstVec<'arena, Node![$name]>
  };
  ($name:ty) => {
    AstVec<'arena, Node![$name]>
  };
}

pub type ExprPtr<'arena, 'text> = AstPtr<'arena, Expr<'arena, 'text>>;

#[derive(Debug, Clone)]
pub struct Ast<'arena, 't> {
  pub arena: Arena<'arena>,
  pub statements: Vec![Stmt],
}

impl<'arena, 't> AstToStr for Ast<'arena, 't> {
  fn ast_to_str_impl(&self, s: &dyn ast2str::Symbols) -> String {
    self
      .statements
      .iter()
      .map(|stmt| stmt.ast_to_str_impl(s))
      .collect::<Vec<_>>()
      .join("\n")
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AstToStr)]
pub enum BinOpKind {
  Add,
  Sub,
  Mul,
  Div,
  Pow,
  Eq,
  Neq,
  BitOr,
  BitAnd,
  BitXor,
  ShiftLeft,
  ShiftRight,
  Or,
  And,
}

#[derive(Debug, Clone, AstToStr)]
pub struct BinOp<'arena, 't> {
  #[skip]
  pub token: Token<'t>,
  pub kind: BinOpKind,
  pub left: Expr<'arena, 't>,
  pub right: Expr<'arena, 't>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AstToStr)]
pub enum UnOpKind {
  Negate,
  Not,
  BitNot,
}

#[derive(Debug, Clone, AstToStr)]
pub struct UnOp<'arena, 't> {
  #[skip]
  pub token: Token<'t>,
  pub kind: UnOpKind,
  pub operand: Expr<'arena, 't>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AstToStr)]
pub enum TryKind {
  Try,
  TryUnwrap,
}

#[derive(Debug, Clone, AstToStr)]
pub struct TryExpr<'arena, 't> {
  #[skip]
  pub token: Token<'t>,
  pub kind: TryKind,
  pub is_postfix: bool,
  pub operand: Expr<'arena, 't>,
}

impl<'arena, 't> BinOp<'arena, 't> {
  pub fn new(token: Token<'t>, left: Expr<'arena, 't>, right: Expr<'arena, 't>) -> Option<Self> {
    Some(Self {
      kind: Self::kind(&token)?,
      token,
      left,
      right,
    })
  }

  pub fn kind(token: &Token<'_>) -> Option<BinOpKind> {
    Some(match token.kind {
      TokenKind::Plus => BinOpKind::Add,
      TokenKind::Minus => BinOpKind::Sub,
      TokenKind::Star => BinOpKind::Mul,
      TokenKind::Slash => BinOpKind::Div,
      TokenKind::Power => BinOpKind::Pow,
      TokenKind::EqualEqual => BinOpKind::Eq,
      TokenKind::BangEqual => BinOpKind::Neq,
      TokenKind::BitOr => BinOpKind::BitOr,
      TokenKind::BitAnd => BinOpKind::BitAnd,
      TokenKind::BitXor => BinOpKind::BitXor,
      TokenKind::ShiftLeft => BinOpKind::ShiftLeft,
      TokenKind::ShiftRight => BinOpKind::ShiftRight,
      TokenKind::Or => BinOpKind::Or,
      TokenKind::And => BinOpKind::And,
      _ => return None,
    })
  }
}

#[derive(Debug, Clone, AstToStr)]
pub struct Pipeline<'arena, 't> {
  pub token: Token<'t>,
  pub input: Node![Expr],
  pub output: Node![Expr],
}

#[derive(Debug, Clone, AstToStr)]
pub enum LiteralValue<'t> {
  Null,
  Bool(bool),
  Int(i64),
  Float(f64),
  String(std::borrow::Cow<'t, str>),
}

#[derive(Debug, Clone, AstToStr)]
pub struct PrimitiveLiteral<'t> {
  #[skip]
  pub token: Token<'t>,
  #[debug]
  pub value: LiteralValue<'t>,
}

#[derive(Debug, Clone, AstToStr)]
pub struct InitializedArray<'arena, 't> {
  pub value: Node![Expr],
  pub length: Node![Expr],
}

#[derive(Debug, Clone, AstToStr)]
pub struct ArrayItem<'arena, 't> {
  pub spread: Option<Token<'t>>,
  pub value: Node![Expr],
}

#[derive(Debug, Clone, AstToStr)]
pub enum ArrayLiteral<'arena, 't> {
  Plain(#[rename = "items"] Vec![Node![ArrayItem]]),
  Initialized(#[forward] HeapNode![InitializedArray]),
}

#[derive(Debug, Clone, AstToStr)]
pub enum Tuple<'arena, 't> {
  Unit,
  Tuple(#[rename = "elements"] Vec![Node![Expr]]),
}

#[derive(Debug, Clone, AstToStr)]
pub enum ExprKind<'arena, 't> {
  Identifier(#[rename = "name"] Token<'t>),
  Pipeline(#[forward] HeapNode![Pipeline]),
  BinOp(#[forward] HeapNode![BinOp]),
  UnOp(#[forward] HeapNode![UnOp]),
  Try(#[forward] HeapNode![TryExpr]),
  PrimitiveLiteral(#[forward] AstPtr<'arena, PrimitiveLiteral<'t>>),
  ArrayLiteral(#[forward] Node![ArrayLiteral]),
  Tuple(#[forward] Node![Tuple]),
  Grouping(#[rename = "expr"] HeapNode![Expr]),
}

#[derive(Debug, Clone, AstToStr)]
pub struct ImportFragment<'arena, 't> {
  pub root_fragment: Token<'t>,
  pub path: Vec![Token<'t>],
  pub end: HeapNode![ImportEnd],
}

#[derive(AstToStr, Debug, Clone)]
pub enum ImportEnd<'arena, 't> {
  Block(AstVec<'arena, Stmt<'arena, 't>>),
  Alias(Token<'t>),
}

#[derive(Debug, Clone, AstToStr)]
pub enum StmtKind<'arena, 't> {
  Import(Node![ImportFragment]),
  ExprStmt(#[forward] HeapNode![Expr]),
  Block(#[rename = "statements"] Vec![Stmt]),
  // NilStmts are produced by free newlines
  NilStmt,
}

#[derive(Debug, Clone, AstToStr)]
pub struct Expr<'arena, 't> {
  pub kind: ExprKind<'arena, 't>,
  #[skip]
  pub span: Span,
  #[skip_if = "Option::is_none"]
  pub comments: Option<Vec![Token<'t>]>,
}

impl<'arena, 't> Expr<'arena, 't> {
  pub fn new(kind: ExprKind<'arena, 't>, span: Span) -> Self {
    Self {
      kind,
      span,
      comments: None,
    }
  }
}

#[derive(Debug, Clone, AstToStr)]
pub struct Stmt<'arena, 't> {
  pub kind: StmtKind<'arena, 't>,
  #[skip]
  pub span: Span,
  #[skip_if = "Option::is_none"]
  pub comments: Option<Vec![Token<'t>]>,
}

impl<'arena, 't> Stmt<'arena, 't> {
  pub fn new(kind: StmtKind<'arena, 't>, span: Span) -> Self {
    Self {
      kind,
      span,
      comments: None,
    }
  }
}
