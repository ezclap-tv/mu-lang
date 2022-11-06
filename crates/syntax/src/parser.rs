//! This module contains the implementation of Mu's parser.
//! The parser performs [syntactic analysis](https://en.wikipedia.org/wiki/Parsing),
//! transforming a stream of [tokens][`crate::lexer::Token`] into an
//! [AST][`crate::ast`].
//!
//! The parser is implemented using a technique called [recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser).
//! It is implemented according to the [grammar][`crate::grammar`]. Both must be
//! kept in sync with eachother.

use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::ast::Module;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::span::Span;

// https://github.com/ves-lang/ves/blob/master/ves-parser/src/parser.rs

pub fn parse(source: &str) -> Result<Module<'_>, Vec<Error>> {
  let end = source.len().saturating_sub(1);
  let eof = Token {
    lexeme: "".into(),
    span: (end..end).into(),
    kind: TokenKind::Eof,
  };

  State {
    lexer: Lexer::new(source),
    previous: eof.clone(),
    current: eof.clone(),
    eof,
    module: Module::default(),
    errors: vec![],
  }
  .parse()
}

struct State<'a> {
  lexer: Lexer<'a>,
  previous: Token<'a>,
  current: Token<'a>,
  eof: Token<'a>,
  module: Module<'a>,
  errors: Vec<Error>,
}

impl<'a> State<'a> {
  fn parse(self) -> Result<Module<'a>, Vec<Error>> {
    todo!()
  }

  fn parse_top_level_stmt(&mut self) -> Result<(), Error> {
    todo!()
  }

  // Skip tokens until we find one that may begin a new statement.
  fn synchronize(&mut self) {
    self.advance();
    todo!()
  }

  #[inline]
  fn advance(&mut self) -> Token<'a> {
    std::mem::swap(&mut self.previous, &mut self.current);
    self.current = self.lexer.next().unwrap_or_else(|| self.eof.clone());
    if self.previous.kind == TokenKind::Error {
      self.errors.push(Error::lexer(self.previous.clone()));
      self.advance();
    }
    self.previous.clone()
  }
}

#[derive(Clone, Debug, Serialize, Deserialize, Error)]
pub enum Error {
  #[error("Invalid character sequence `{0}` at `{1}`")]
  Lexer(String, Span),
}

impl Error {
  fn lexer(token: Token<'_>) -> Self {
    Error::Lexer(token.lexeme.to_string(), token.span)
  }
}
