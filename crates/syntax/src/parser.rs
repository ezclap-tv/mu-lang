//! This module contains the implementation of Mu's parser.
//! The parser performs [syntactic analysis](https://en.wikipedia.org/wiki/Parsing),
//! transforming a stream of [tokens][`crate::lexer::Token`] into an
//! [AST][`crate::ast`].
//!
//! The parser is implemented using a technique called [recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser).
//! It is implemented according to the [grammar][`crate::grammar`]. Both must be
//! kept in sync with eachother.
#![deny(unused_must_use)]

use thiserror::Error;

use crate::ast::{stmt, ExprKind, Ident, Module, StmtKind, TypeKind};
use crate::lexer::{Lexer, Token, TokenKind};
use crate::span::{Span, Spanned};

// https://github.com/ves-lang/ves/blob/master/ves-parser/src/parser.rs

pub fn parse(source: &str) -> Result<Module<'_>, Vec<Error>> {
  let lexer = Lexer::new(source);
  Parser {
    lexer,
    module: Module::default(),
    errors: vec![],
  }
  .parse()
}

struct Parser<'a> {
  lexer: Lexer<'a>,
  module: Module<'a>,
  errors: Vec<Error>,
}

impl<'a> Parser<'a> {
  fn parse(mut self) -> Result<Module<'a>, Vec<Error>> {
    use TokenKind::Eof;

    self.bump();

    while !self.current().is(Eof) {
      if let Err(e) = self.parse_top_level() {
        self.errors.push(e);
        self.sync();
      }
    }

    if !self.errors.is_empty() {
      return Err(self.errors);
    }

    Ok(self.module)
  }

  fn parse_top_level(&mut self) -> Result<(), Error> {
    use TokenKind::{
      Class, Fn, For, Let, Loop, Pub, RightBrace, Semicolon, Trait, Type, Use, While,
    };

    self.bump_if(Pub);

    if self.bump_if(Fn) {
      self.parse_decl_fn(self.previous().is(Pub))
    } else if self.bump_if(Type) {
      self.parse_decl_type(self.previous().is(Pub))
    } else if self.bump_if(Class) {
      self.parse_decl_class(self.previous().is(Pub))
    } else if self.bump_if(Trait) {
      self.parse_decl_trait(self.previous().is(Pub))
    } else if self.bump_if(Use) {
      self.parse_import()
    } else if !self.previous().is(Pub) {
      let stmt = self.span(|p| {
        if p.bump_if(For) {
          p.parse_stmt_loop_for()
        } else if p.bump_if(While) {
          p.parse_stmt_loop_while()
        } else if p.bump_if(Loop) {
          p.parse_stmt_loop_inf()
        } else if p.bump_if(Let) {
          let stmt = p.parse_stmt_let();
          p.expect(Semicolon)?;
          stmt
        } else {
          let stmt = p.parse_stmt_expr();
          // semicolon after expr stmt is only required if the expr does not end with `}`,
          // for example: if true { "a" } else { "b" }
          if !p.previous().is(RightBrace) {
            p.expect(Semicolon)?;
          }
          stmt
        }
      })?;
      self.module.top_level.push(stmt);
      Ok(())
    } else {
      Err(Error::unexpected_vis(self.previous().span))
    }
  }

  fn parse_decl_fn(&mut self, is_pub: bool) -> Result<(), Error> {
    todo!()
  }

  fn parse_decl_type(&mut self, is_pub: bool) -> Result<(), Error> {
    todo!()
  }

  fn parse_decl_class(&mut self, is_pub: bool) -> Result<(), Error> {
    todo!()
  }

  fn parse_decl_trait(&mut self, is_pub: bool) -> Result<(), Error> {
    todo!()
  }

  fn parse_import(&mut self) -> Result<(), Error> {
    todo!()
  }

  fn parse_stmt_loop_for(&mut self) -> Result<StmtKind<'a>, Error> {
    todo!()
  }

  fn parse_stmt_loop_while(&mut self) -> Result<StmtKind<'a>, Error> {
    todo!()
  }

  fn parse_stmt_loop_inf(&mut self) -> Result<StmtKind<'a>, Error> {
    todo!()
  }

  fn parse_stmt_let(&mut self) -> Result<StmtKind<'a>, Error> {
    use TokenKind::{Colon, Equal};

    let name = self.parse_ident()?;
    let ty = self.bump_then(Colon, |p| p.span(Self::parse_type))?;
    self.expect(Equal)?;
    let value = self.span(Self::parse_expr)?;

    Ok(stmt::let_(name, ty, value))
  }

  fn parse_stmt_expr(&mut self) -> Result<StmtKind<'a>, Error> {
    todo!()
  }

  fn parse_expr(&mut self) -> Result<ExprKind<'a>, Error> {
    todo!()
  }

  fn parse_type(&mut self) -> Result<TypeKind<'a>, Error> {
    todo!()
  }

  fn parse_ident(&mut self) -> Result<Ident<'a>, Error> {
    use TokenKind::Ident;
    self
      .expect(Ident)
      .map(|token| Spanned::new(token.lexeme.clone(), token.span))
  }
}

// Helper functions
impl<'a> Parser<'a> {
  #[inline]
  fn maybe(&mut self, which: TokenKind) -> Option<&Token<'a>> {
    if self.current().is(which) {
      Some(self.bump())
    } else {
      None
    }
  }

  /// If the current token matches `which`, returns it and calls `bump`,
  /// otherwise returns an error.
  #[inline]
  fn expect(&mut self, which: TokenKind) -> Result<&Token<'a>, Error> {
    if self.current().is(which) {
      Ok(self.bump())
    } else {
      Err(Error::expected(which, self.current().span))
    }
  }

  /// If the current token matches `which`, then it will return the result of
  /// `cons` wrapped in `Some`, and `None` otherwise. `cons` may fail so this
  /// also returns a `Result` with any potential errors.
  #[inline]
  fn bump_then<T>(
    &mut self,
    which: TokenKind,
    cons: impl FnOnce(&mut Self) -> Result<T, Error>,
  ) -> Result<Option<T>, Error> {
    if self.current().is(which) {
      self.bump();
      cons(self).map(Some)
    } else {
      Ok(None)
    }
  }

  /// Returns whether or not the current token matches `which`. If true, this
  /// also calls `bump`.
  #[inline]
  fn bump_if(&mut self, which: TokenKind) -> bool {
    if self.current().is(which) {
      self.bump();
      true
    } else {
      false
    }
  }

  /// Skip tokens until we find one that may begin a new statement.
  fn sync(&mut self) {
    self.bump();
    while self.previous().kind != TokenKind::Eof {
      // semicolons terminate statement, so it's safe to start parsing again after
      // we encounter one.
      if self.previous().kind == TokenKind::Semicolon {
        break;
      }

      // all of these tokens begin a statement that cannot also appear as an
      // expression, so they are good candidates for synchronization.
      match self.current().kind {
        TokenKind::Pub
        | TokenKind::Use
        | TokenKind::Fn
        | TokenKind::Type
        | TokenKind::Class
        | TokenKind::Trait
        | TokenKind::Impl
        | TokenKind::For
        | TokenKind::While
        | TokenKind::Loop => break,
        _ => {}
      };

      self.bump();
    }
  }

  /// Calls `cons` and wraps the returned value in a span that encompasses the
  /// entire sequence of tokens parsed within `cons`.
  #[inline]
  fn span<T>(
    &mut self,
    cons: impl FnOnce(&mut Self) -> Result<T, Error>,
  ) -> Result<Spanned<T>, Error> {
    let start = self.current().span.start;
    cons(self).map(|value| {
      let end = self.previous().span.end;
      Spanned::new(value, start..end)
    })
  }

  #[inline]
  fn previous(&self) -> &Token<'a> {
    self.lexer.previous()
  }

  #[inline]
  fn current(&self) -> &Token<'a> {
    self.lexer.current()
  }

  /// Move forward by one token, returning the previous one.
  #[inline]
  fn bump(&mut self) -> &Token<'a> {
    self.lexer.bump()
  }
}

#[derive(Clone, Debug, Error)]
pub enum Error {
  #[error("invalid character sequence `{0}` at {1}")]
  Lexer(String, Span),
  #[error("unexpected visibility modifier at {0}")]
  UnexpectedVis(Span),
  #[error("expected {0} at {1}")]
  Expected(TokenKind, Span),
}

impl Error {
  fn lexer(token: Token<'_>) -> Self {
    Error::Lexer(token.lexeme.to_string(), token.span)
  }

  fn unexpected_vis(span: Span) -> Self {
    Error::UnexpectedVis(span)
  }

  fn expected(which: TokenKind, span: Span) -> Self {
    Error::Expected(which, span)
  }
}

#[cfg(test)]
mod tests;
