use std::borrow::Cow;

use logos::Logos;
pub use logos::Span;
use serde::{Deserialize, Serialize};

pub struct Lexer<'a> {
  inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str) -> Lexer<'a> {
    Lexer {
      inner: logos::Lexer::new(source),
    }
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    self
      .inner
      .next()
      .map(|next| Token::new(self.inner.slice(), self.inner.span(), next))
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Token<'a> {
  pub lexeme: Cow<'a, str>,
  // Note on the `skip`:
  // If we're deserializing, we don't have access to the original source anymore
  // so the spans are useless. They also create a lot of noise in snapshot tests.
  #[serde(skip)]
  pub span: Span,
  pub kind: TokenKind,
}

impl<'a> Token<'a> {
  pub fn new<S: Into<Cow<'a, str>>>(lexeme: S, span: Span, kind: TokenKind) -> Token<'a> {
    Token {
      lexeme: lexeme.into(),
      span,
      kind,
    }
  }

  pub fn into_static(self) -> Token<'static> {
    Token {
      lexeme: Cow::from(self.lexeme.into_owned()),
      span: self.span.clone(),
      kind: self.kind,
    }
  }

  pub fn eof(end: usize) -> Token<'a> {
    Token::new("", end..end, TokenKind::Eof)
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Logos, Serialize, Deserialize)]
pub enum TokenKind {
  #[token("{")]
  LBrace,
  #[token("}")]
  RBrace,

  #[token("(")]
  LParen,
  #[token(")")]
  RParen,

  #[token(":")]
  Colon,
  #[token(";")]
  Semicolon,
  #[token("->")]
  Arrow,

  #[token("=")]
  Equal,
  #[token(".")]
  Dot,
  #[token(",")]
  Comma,

  #[token("+")]
  Plus,
  #[token("-")]
  Minus,
  #[token("*")]
  Star,
  #[token("/")]
  Slash,
  #[token("%")]
  Percent,
  #[token("!")]
  Bang,
  #[token("~")]
  Tilde,
  #[token("<")]
  Less,
  #[token(">")]
  More,
  #[token("**")]
  Power,
  #[token("&&")]
  And,
  #[token("||")]
  Or,
  #[token("==")]
  EqualEqual,
  #[token("!=")]
  BangEqual,
  #[token("<=")]
  LessEqual,
  #[token(">=")]
  MoreEqual,

  // Literals
  #[regex("[0-9]([0-9_]*[0-9])?")]
  Integer,
  #[regex("\"([^\"\\\\]|\\\\.)*\"")]
  String,
  #[token("true")]
  #[token("false")]
  Bool,

  // Keywords
  #[token("let")]
  Let,
  #[token("in")]
  In,
  #[token("if")]
  If,
  #[token("then")]
  Then,
  #[token("else")]
  Else,

  // Miscellaneous
  #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
  Ident,
  #[regex("[ \t\n\r]+", logos::skip)]
  Whitespace,
  #[regex("//[^\n]*", logos::skip)]
  Comment,
  #[error]
  Error,

  Eof,
}
