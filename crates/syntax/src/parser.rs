//! This module contains the implementation of Mu's parser.
//! The parser performs [syntactic analysis](https://en.wikipedia.org/wiki/Parsing),
//! transforming a stream of [tokens][`crate::lexer::Token`] into an
//! [AST][`crate::ast`].
//!
//! The parser is implemented using a technique called [recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser).
//! It is implemented according to the [grammar][`crate::grammar`]. Both must be
//! kept in sync with eachother.

use crate::ast;
use crate::lexer::{Lexer, Token, TokenKind};

pub enum Error {}

pub struct Parser<'a> {
  lex: Lexer<'a>,
  previous: Token<'a>,
  current: Token<'a>,
  eof: Token<'a>,
}

impl<'a> Parser<'a> {
  pub fn new(source: &'a str) -> Self {
    let end = source.len().saturating_sub(1);
    let eof = Token {
      lexeme: "".into(),
      span: (end..end).into(),
      kind: TokenKind::Eof,
    };

    Self {
      lex: Lexer::new(source),
      previous: eof.clone(),
      current: eof.clone(),
      eof,
    }
  }
}
