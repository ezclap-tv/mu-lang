use std::borrow::Cow;

use ast2str::AstToStr;
use logos::Logos;
pub use logos::Span;

pub struct Lexer<'a> {
  inner: logos::Lexer<'a, TokenKind>,
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str) -> Lexer<'a> {
    Lexer {
      inner: logos::Lexer::new(source),
    }
  }

  pub fn next(&mut self) -> Option<Token<'a>> {
    self
      .inner
      .next()
      .map(|next| Token::new(self.inner.slice(), self.inner.span(), next))
  }
}

#[derive(Clone, Debug, PartialEq, Eq, AstToStr)]
pub struct Token<'a> {
  #[forward]
  pub lexeme: Cow<'a, str>,
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Logos)]
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

#[cfg(test)]
mod tests {
  use super::*;

  fn tokenize(source: &str) -> Vec<Token<'_>> {
    let mut lex = Lexer::new(source);
    let mut tokens = vec![];
    while let Some(token) = lex.next() {
      tokens.push(token);
    }
    tokens
  }

  #[test]
  fn example() {
    let source = r#"
// variables
let v = 10;

let v: int = 10; // int
let v: bool = true; /// bool
let v: str = "test"; /// str
let v: {x:int, y:int} = {x: 10, y: 10}; /// record

// functions
// single argument
// parameter and return type annotations are required
let fib n: int -> int =
  if n < 2 then n
  else n * fib (n - 1)
  ;

// function calls
fib (v.x);

let print_square n: int -> int =
  let r = intToStr (n * n) in
  print n + " * " + n + " = " + r
  ;
"#;
    println!(
      "{:#?}",
      tokenize(source)
        .into_iter()
        .map(|t| t.kind)
        .collect::<Vec<_>>()
    );
    //assert_eq!(tokenize(source), vec![]);
  }
}
