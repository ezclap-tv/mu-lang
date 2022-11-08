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

use crate::ast;
use crate::ast::{stmt, ExprKind, Ident, Module, StmtKind, Type, TypeKind};
use crate::lexer::{Lexer, Token, TokenKind, BRACKETS, PARENS};
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
    use TokenKind::{BraceR, Class, Fn, For, Let, Loop, Pub, Semicolon, Trait, Type, Use, While};

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
          if !p.previous().is(BraceR) {
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
    let ty = self.maybe(Colon, |p, _| p.span(Self::parse_type))?;
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
    self.parse_type_option()
  }

  fn parse_type_option(&mut self) -> Result<TypeKind<'a>, Error> {
    use TokenKind::Question;

    let ty = self.span(Self::parse_type_postfix)?;
    let ty = if self.bump_if(Question) {
      // type_optional
      ast::ty::option(ty)
    } else {
      ty.into_inner()
    };

    Ok(ty)
  }

  fn parse_type_postfix(&mut self) -> Result<TypeKind<'a>, Error> {
    use TokenKind::{BracketL, Comma, Dot};

    let mut ty = self.span(Self::parse_type_primary)?;
    loop {
      if self.current().is(Dot) {
        // type_field
        ty = self.span(|p| {
          p.bump(); // bump Dot
          p.parse_ident().map(|ident| ast::ty::field(ty, ident))
        })?;
        continue;
      }

      if self.current().is(BracketL) {
        // type_inst
        ty = self.span(|p| {
          p.wrap_list(BRACKETS, Comma, |p| p.span(Self::parse_type))
            .map(|args| ast::ty::inst(ty, args))
        })?;
        continue;
      }

      break;
    }

    Ok(ty.into_inner())
  }

  fn parse_type_primary(&mut self) -> Result<TypeKind<'a>, Error> {
    use TokenKind::{ArrowThin, BracketL, Comma, Fn, Ident, ParenL, ParenR};

    // type_var
    if self.bump_if(Ident) {
      let ty = ast::ty::var(ast::ident(self.previous()));
      return Ok(ty);
    }

    // type_fn
    if self.bump_if(Fn) {
      let args = self.wrap_list(PARENS, Comma, |p| p.span(Self::parse_type))?;
      let ret = self.maybe(ArrowThin, |p, _| p.span(Self::parse_type))?;
      let ty = ast::ty::fn_(args, ret);
      return Ok(ty);
    }

    // type_array
    if self.current().is(BracketL) {
      let item = self.wrap(BRACKETS, |p| p.span(Self::parse_type))?;
      let ty = ast::ty::array(item);
      return Ok(ty);
    }

    // type_tuple or type_group
    if self.current().is(ParenL) {
      let ty = self.wrap(PARENS, |p| {
        let first = p.span(Self::parse_type)?;

        if p.current().is(Comma) {
          // type_tuple
          let mut items = vec![first];
          while p.bump_if(Comma) && !p.current().is(ParenR) {
            items.push(p.span(Self::parse_type)?);
          }
          return Ok(ast::ty::tuple(items));
        }

        // type_group
        Ok(first.into_inner())
      })?;
      return Ok(ty);
    }

    Err(Error::unexpected(self.current()))
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
  /// Parse a wrapped token sequence, e.g.:
  ///
  /// ```text
  /// parser.wrap((LeftParen, RightParen), Parser::parse_expr)
  /// ```
  /// would parse:
  ///
  /// ```text
  /// ( 1 + 1 )
  /// ```
  ///
  /// Note: Do not bump the opening token!
  ///
  /// For wrapped and separated lists (e.g. arrays), use `wrap_list` instead, as
  /// that also handles trailing commas.
  #[inline]
  fn wrap<T>(
    &mut self,
    (l, r): (TokenKind, TokenKind),
    cons: impl FnOnce(&mut Self) -> Result<T, Error>,
  ) -> Result<T, Error> {
    self.expect(l)?;
    let inner = cons(self)?;
    self.expect(r)?;
    Ok(inner)
  }

  /* #[inline]
  fn list<T>(
    &mut self,
    separator: TokenKind,
    mut cons: impl FnMut(&mut Self) -> Result<T, Error>,
  ) -> Result<Vec<T>, Error> {
    let mut items = vec![cons(self)?];
    while self.bump_if(separator) {
      items.push(cons(self)?);
    }
    Ok(items)
  } */

  /// Helper function for parsing bracketed lists, e.g. `[a, b, c,]`,
  /// with support for trailing separators.
  #[inline]
  fn wrap_list<T>(
    &mut self,
    (l, r): (TokenKind, TokenKind),
    sep: TokenKind,
    mut cons: impl FnMut(&mut Self) -> Result<T, Error>,
  ) -> Result<Vec<T>, Error> {
    self.expect(l)?;
    let mut items = vec![];
    if !self.current().is(r) {
      items.push(cons(self)?);
      while self.bump_if(sep) && !self.current().is(r) {
        items.push(cons(self)?);
      }
    }
    self.expect(r)?;

    Ok(items)
  }

  #[inline]
  fn maybe<T>(
    &mut self,
    which: TokenKind,
    cons: impl FnOnce(&mut Self, &Token<'a>) -> Result<T, Error>,
  ) -> Result<Option<T>, Error> {
    if self.current().is(which) {
      let token = self.bump().clone();
      cons(self, &token).map(Some)
    } else {
      Ok(None)
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
    self.lexer.bump();
    while self.current().is(TokenKind::Error) {
      self.errors.push(Error::lexer(self.current()));
      self.lexer.bump();
    }
    self.previous()
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
  #[error("unexpected {0} at {1}")]
  Unexpected(TokenKind, Span),
}

impl Error {
  fn lexer(token: &Token<'_>) -> Self {
    Error::Lexer(token.lexeme.to_string(), token.span)
  }

  fn unexpected_vis(span: Span) -> Self {
    Error::UnexpectedVis(span)
  }

  fn unexpected(token: &Token<'_>) -> Self {
    Error::Unexpected(token.kind, token.span)
  }

  fn expected(which: TokenKind, span: Span) -> Self {
    Error::Expected(which, span)
  }
}

#[cfg(test)]
mod tests;
