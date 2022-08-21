#![feature(allocator_api)]

pub mod ast_arena;
pub mod dynamic_arena;

pub use ast2str::{self, AstToStr};
pub use mu_lexer::{Span, Token, TokenKind};

#[macro_export]
macro_rules! Node {
  ($name:ident) => {
    $name<'t>
  };
  ($name:ty) => {
    $name
  };
}

#[derive(Debug, Clone)]
pub struct Ast<'t> {
  pub statements: Vec<Stmt<'t>>,
}

impl<'t> AstToStr for Ast<'t> {
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
pub struct BinOp<'t> {
  #[skip]
  pub token: Token<'t>,
  pub kind: BinOpKind,
  pub left: Expr<'t>,
  pub right: Expr<'t>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AstToStr)]
pub enum UnOpKind {
  Negate,
  Not,
  BitNot,
}

#[derive(Debug, Clone, AstToStr)]
pub struct UnOp<'t> {
  #[skip]
  pub token: Token<'t>,
  pub kind: UnOpKind,
  pub operand: Expr<'t>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, AstToStr)]
pub enum TryKind {
  Try,
  TryUnwrap,
}

#[derive(Debug, Clone, AstToStr)]
pub struct TryExpr<'t> {
  #[skip]
  pub token: Token<'t>,
  pub kind: TryKind,
  pub is_postfix: bool,
  pub operand: Expr<'t>,
}

impl<'t> BinOp<'t> {
  pub fn new(token: Token<'t>, left: Expr<'t>, right: Expr<'t>) -> Option<Self> {
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
pub struct Pipeline<'t> {
  pub token: Token<'t>,
  pub input: Expr<'t>,
  pub output: Expr<'t>,
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
pub struct InitializedArray<'t> {
  pub value: Expr<'t>,
  pub length: Expr<'t>,
}

#[derive(Debug, Clone, AstToStr)]
pub struct ArrayItem<'t> {
  pub spread: Option<Token<'t>>,
  pub value: Expr<'t>,
}

#[derive(Debug, Clone, AstToStr)]
pub enum ArrayLiteral<'t> {
  Plain(#[rename = "items"] Vec<ArrayItem<'t>>),
  Initialized(#[forward] Box<InitializedArray<'t>>),
}

#[derive(Debug, Clone, AstToStr)]
pub enum Tuple<'t> {
  Unit,
  Tuple(#[rename = "elements"] Vec<Expr<'t>>),
}

#[derive(Debug, Clone, AstToStr)]
pub enum ExprKind<'t> {
  Identifier(#[rename = "name"] Token<'t>),
  Pipeline(#[forward] Box<Pipeline<'t>>),
  BinOp(#[forward] Box<BinOp<'t>>),
  UnOp(#[forward] Box<UnOp<'t>>),
  Try(#[forward] Box<TryExpr<'t>>),
  PrimitiveLiteral(#[forward] Box<PrimitiveLiteral<'t>>),
  ArrayLiteral(#[forward] ArrayLiteral<'t>),
  Tuple(#[forward] Tuple<'t>),
  Grouping(#[rename = "expr"] Box<Expr<'t>>),
}

#[derive(Debug, Clone, AstToStr)]
pub struct ImportFragment<'t> {
  pub root_fragment: Token<'t>,
  pub path: Vec<Token<'t>>,
  pub end: ImportEnd<'t>,
}

#[derive(AstToStr, Debug, Clone)]
pub enum ImportEnd<'t> {
  Block(Vec<Stmt<'t>>),
  Alias(Token<'t>),
}

#[derive(Debug, Clone, AstToStr)]
pub enum StmtKind<'t> {
  Import(ImportFragment<'t>),
  ExprStmt(#[forward] Expr<'t>),
  Block(#[rename = "statements"] Vec<Stmt<'t>>),
  // NilStmts are produced by free newlines
  NilStmt,
}

#[derive(Debug, Clone, AstToStr)]
pub struct Expr<'t> {
  pub kind: ExprKind<'t>,
  #[skip]
  pub span: Span,
  #[skip_if = "Option::is_none"]
  pub comments: Option<Vec<Token<'t>>>,
}

impl<'t> Expr<'t> {
  pub fn new(kind: ExprKind<'t>, span: Span) -> Self {
    Self {
      kind,
      span,
      comments: None,
    }
  }
}

#[derive(Debug, Clone, AstToStr)]
pub struct Stmt<'t> {
  pub kind: StmtKind<'t>,
  #[skip]
  pub span: Span,
  #[skip_if = "Option::is_none"]
  pub comments: Option<Vec<Token<'t>>>,
}

impl<'t> Stmt<'t> {
  pub fn new(kind: StmtKind<'t>, span: Span) -> Self {
    Self {
      kind,
      span,
      comments: None,
    }
  }
}
