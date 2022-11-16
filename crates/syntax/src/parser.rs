//! This module contains the implementation of Mu's parser.
//! The parser performs [syntactic analysis](https://en.wikipedia.org/wiki/Parsing),
//! transforming a stream of [tokens][`crate::lexer::Token`] into an
//! [AST][`crate::ast`].
//!
//! The parser is implemented using a technique called [recursive descent](https://en.wikipedia.org/wiki/Recursive_descent_parser).
//! It is implemented according to the [grammar][`crate::grammar`]. Both must be
//! kept in sync with eachother.
#![deny(unused_must_use)]
#![allow(clippy::needless_lifetimes)]

use std::ops::Deref;

use thiserror::Error;

use crate::ast::expr::{AssignOp, BinaryOp, Block, UnaryOp};
use crate::ast::{
  expr, stmt, ty, Expr, ExprKind, Ident, Import, Module, Path, Segment, StmtKind, TypeKind,
};
use crate::lexer::{Lexer, Token, TokenKind, ANGLES, BRACES, BRACKETS, PARENS};
use crate::span::{Span, Spanned};

// https://github.com/ves-lang/ves/blob/master/ves-parser/src/parser.rs

pub fn parse(source: &str) -> Result<Module<'_>, Vec<Error>> {
  Parser {
    lexer: Lexer::new(source),
    module: Module::default(),
    errors: Vec::default(),
    ctx: Context::default(),
  }
  .parse()
}

struct Parser<'a> {
  lexer: Lexer<'a>,
  module: Module<'a>,
  errors: Vec<Error>,
  ctx: Context,
}

#[derive(Clone, Copy, Default)]
struct Context {
  /// This is used to determine if we should parse an `expr_class` or not, due
  /// to the following ambiguity:
  ///
  /// ```ignore
  /// // Parser sees this as `"if" <expr_class>` instead of `"if" <expr> <block>`
  /// if x { 0 }
  /// ```
  expr_before_block: bool,
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
    use TokenKind::{Class, Fn, Pub, Trait, Type, Use};

    self.bump_if(Pub);

    if self.bump_if(Fn) {
      self.parse_decl_fn(self.previous().is(Pub))?;
    } else if self.bump_if(Type) {
      self.parse_decl_type(self.previous().is(Pub))?;
    } else if self.bump_if(Class) {
      self.parse_decl_class(self.previous().is(Pub))?;
    } else if self.bump_if(Trait) {
      self.parse_decl_trait(self.previous().is(Pub))?;
    } else if self.bump_if(Use) {
      self.parse_import()?;
    } else if !self.previous().is(Pub) {
      let stmt = self.span(Self::parse_top_level_stmt)?;
      self.module.top_level.push(stmt);
    } else {
      return Err(Error::unexpected_vis(self.previous().span))?;
    }

    Ok(())
  }

  fn parse_decl_fn(&mut self, _is_pub: bool) -> Result<(), Error> {
    Err(Error::NotImplemented("function declarations"))
  }

  fn parse_decl_type(&mut self, _is_pub: bool) -> Result<(), Error> {
    Err(Error::NotImplemented("type declarations"))
  }

  fn parse_decl_class(&mut self, _is_pub: bool) -> Result<(), Error> {
    Err(Error::NotImplemented("class declarations"))
  }

  fn parse_decl_trait(&mut self, _is_pub: bool) -> Result<(), Error> {
    Err(Error::NotImplemented("trait declarations"))
  }

  fn parse_import(&mut self) -> Result<(), Error> {
    use TokenKind::{BraceL, Comma, Semicolon};

    let path = vec![];
    if self.current().is(BraceL) {
      self.wrap_list(BRACES, Comma, |p| p.parse_import_inner(&path))?;
    } else {
      self.parse_import_inner(&path)?;
    }

    self.expect(Semicolon)?;

    Ok(())
  }

  fn parse_import_inner(&mut self, path: &Vec<Ident<'a>>) -> Result<(), Error> {
    use TokenKind::{As, BraceL, Comma, Dot};

    let path = path.with_elem(self.parse_ident()?);
    if self.bump_if(As) {
      let alias = Some(self.parse_ident()?);
      self.module.imports.push(Import { path, alias });
      return Ok(());
    }

    if self.bump_if(Dot) {
      if self.current().is(BraceL) {
        self.wrap_list(BRACES, Comma, |p| p.parse_import_inner(&path))?;
        return Ok(());
      }

      self.parse_import_inner(&path)?;
      return Ok(());
    }

    self.module.imports.push(Import { path, alias: None });
    Ok(())
  }

  fn parse_top_level_stmt(&mut self) -> Result<StmtKind<'a>, Error> {
    use TokenKind::{BraceR, For, Let, Loop, Semicolon, While};

    // this code is duplicated in `parse_block`,
    // because semicolons work differently there.
    if self.bump_if(For) {
      self.parse_stmt_loop_for()
    } else if self.bump_if(While) {
      self.parse_stmt_loop_while()
    } else if self.bump_if(Loop) {
      self.parse_stmt_loop_inf()
    } else if self.bump_if(Let) {
      let stmt = self.parse_stmt_let();
      self.expect(Semicolon)?;
      stmt
    } else {
      let stmt = self.parse_stmt_expr()?;
      // semicolon after expr stmt is only required if the expr does not end with `}`,
      // for example:
      // if true { "a" } else { "b" } // not required
      // "b" // required
      if !self.previous().is(BraceR) {
        self.expect(Semicolon)?;
      } else {
        self.bump_if(Semicolon);
      }
      Ok(stmt)
    }
  }

  fn parse_stmt_loop_for(&mut self) -> Result<StmtKind<'a>, Error> {
    use TokenKind::In;

    let item = self.parse_ident()?;
    self.expect(In)?;
    let iter = self.span(Self::parse_expr_before_block)?;
    let body = self.parse_block()?;
    Ok(stmt::for_(item, iter, body))
  }

  fn parse_stmt_loop_while(&mut self) -> Result<StmtKind<'a>, Error> {
    let cond = self.span(Self::parse_expr_before_block)?;
    let body = self.parse_block()?;
    Ok(stmt::while_(cond, body))
  }

  fn parse_stmt_loop_inf(&mut self) -> Result<StmtKind<'a>, Error> {
    let body = self.parse_block()?;
    Ok(stmt::loop_(body))
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
    let expr = self.span(Self::parse_expr)?;
    Ok(stmt::expr(expr))
  }

  #[inline]
  fn parse_expr_before_block(&mut self) -> Result<ExprKind<'a>, Error> {
    let ctx = Context {
      expr_before_block: true,
    };
    self.with_ctx(ctx, Self::parse_expr_assign)
  }

  #[inline]
  fn parse_expr(&mut self) -> Result<ExprKind<'a>, Error> {
    let ctx = Context::default();
    self.with_ctx(ctx, Self::parse_expr_assign)
  }

  fn parse_expr_assign(&mut self) -> Result<ExprKind<'a>, Error> {
    use ExprKind::{GetField, GetIndex, GetVar};
    use TokenKind::{
      Equal, MinusEqual, NullishEqual, PercentEqual, PlusEqual, SlashEqual, StarEqual,
      StarStarEqual,
    };

    let left = self.span(Self::parse_expr_range)?;
    let op = match self.current().kind {
      Equal => None,
      PlusEqual => Some(AssignOp::Add),
      MinusEqual => Some(AssignOp::Sub),
      StarEqual => Some(AssignOp::Mul),
      SlashEqual => Some(AssignOp::Div),
      PercentEqual => Some(AssignOp::Rem),
      StarStarEqual => Some(AssignOp::Pow),
      NullishEqual => Some(AssignOp::Opt),
      _ => return Ok(left.into_inner()),
    };
    self.bump(); // bump assignment operator
    check_assign_target(&left)?;
    let value = self.span(Self::parse_expr)?;
    let target_span = left.span;
    let expr = match left.into_inner() {
      GetVar(v) => expr::set_var(v.name, value, op),
      GetField(v) => expr::set_field(v.target, v.key, value, op),
      GetIndex(v) => expr::set_index(v.target, v.key, value, op),
      _ => return Err(Error::invalid_assign(target_span)),
    };
    Ok(expr)
  }

  fn parse_expr_range(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::Range;

    let left = self.span(Self::parse_expr_binary)?;
    if self.bump_if(Range) {
      let right = self.span(Self::parse_expr_binary)?;
      return Ok(expr::range(left, right));
    }

    Ok(left.into_inner())
  }

  fn parse_expr_binary(&mut self) -> Result<ExprKind<'a>, Error> {
    self.parse_expr_opt()
  }

  fn parse_expr_opt(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::Nullish;

    let mut left = self.span(Self::parse_expr_or)?;
    while self.bump_if(Nullish) {
      let right = self.span(Self::parse_expr_or)?;
      left = Spanned::new(
        left.span.join(right.span),
        expr::binary(BinaryOp::NullOr, left, right),
      );
    }
    Ok(left.into_inner())
  }

  fn parse_expr_or(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::PipePipe;

    let mut left = self.span(Self::parse_expr_and)?;
    while self.bump_if(PipePipe) {
      let right = self.span(Self::parse_expr_and)?;
      left = Spanned::new(
        left.span.join(right.span),
        expr::binary(BinaryOp::BoolOr, left, right),
      );
    }
    Ok(left.into_inner())
  }

  fn parse_expr_and(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::AndAnd;

    let mut left = self.span(Self::parse_expr_eq)?;
    while self.bump_if(AndAnd) {
      let right = self.span(Self::parse_expr_eq)?;
      left = Spanned::new(
        left.span.join(right.span),
        expr::binary(BinaryOp::BoolAnd, left, right),
      );
    }
    Ok(left.into_inner())
  }

  fn parse_expr_eq(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::{BangEqual, EqualEqual};

    let mut left = self.span(Self::parse_expr_comp)?;
    loop {
      let op = match self.current().kind {
        EqualEqual => BinaryOp::Equals,
        BangEqual => BinaryOp::NotEquals,
        _ => break,
      };
      self.bump(); // bump operator
      let right = self.span(Self::parse_expr_comp)?;
      left = Spanned::new(left.span.join(right.span), expr::binary(op, left, right));
    }
    Ok(left.into_inner())
  }

  fn parse_expr_comp(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::{Less, LessEqual, More, MoreEqual};

    let mut left = self.span(Self::parse_expr_add)?;
    loop {
      let op = match self.current().kind {
        Less => BinaryOp::CompareLess,
        LessEqual => BinaryOp::CompareLessEq,
        More => BinaryOp::CompareMore,
        MoreEqual => BinaryOp::CompareMoreEq,
        _ => break,
      };
      self.bump(); // bump operator
      let right = self.span(Self::parse_expr_add)?;
      left = Spanned::new(left.span.join(right.span), expr::binary(op, left, right));
    }
    Ok(left.into_inner())
  }

  fn parse_expr_add(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::{Minus, Plus};

    let mut left = self.span(Self::parse_expr_mul)?;
    loop {
      let op = match self.current().kind {
        Plus => BinaryOp::Add,
        Minus => BinaryOp::Subtract,
        _ => break,
      };
      self.bump(); // bump operator
      let right = self.span(Self::parse_expr_mul)?;
      left = Spanned::new(left.span.join(right.span), expr::binary(op, left, right));
    }
    Ok(left.into_inner())
  }

  fn parse_expr_mul(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::{Percent, Slash, Star};

    let mut left = self.span(Self::parse_expr_pow)?;
    loop {
      let op = match self.current().kind {
        Star => BinaryOp::Multiply,
        Slash => BinaryOp::Divide,
        Percent => BinaryOp::Remainder,
        _ => break,
      };
      self.bump(); // bump operator
      let right = self.span(Self::parse_expr_pow)?;
      left = Spanned::new(left.span.join(right.span), expr::binary(op, left, right));
    }
    Ok(left.into_inner())
  }

  fn parse_expr_pow(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::StarStar;

    let mut left = self.span(Self::parse_expr_unary)?;
    while self.bump_if(StarStar) {
      let right = self.span(Self::parse_expr_unary)?;
      left = Spanned::new(
        left.span.join(right.span),
        expr::binary(BinaryOp::Power, left, right),
      );
    }
    Ok(left.into_inner())
  }

  fn parse_expr_unary(&mut self) -> Result<ExprKind<'a>, Error> {
    self.parse_expr_prefix()
  }

  fn parse_expr_prefix(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::{Bang, Minus};

    let op = match self.current().kind {
      Minus => UnaryOp::Invert,
      Bang => UnaryOp::Negate,
      _ => return self.parse_expr_postfix(),
    };
    self.bump(); // bump operator
    let inner = self.span(Self::parse_expr_unary)?;
    Ok(expr::unary(op, inner))
  }

  fn parse_expr_postfix(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::{BraceL, BracketL, Colon, Comma, Dot, Less, Optional, ParenL};

    let mut expr = self.span(Self::parse_expr_primary)?;
    loop {
      let opt = self.bump_if(Optional);
      match self.current().kind {
        // expr_call
        ParenL => {
          let args = self.span(|p| p.wrap_list(PARENS, Comma, |p| p.span(Self::parse_expr)))?;
          expr = Spanned::new(
            expr.span.join(args.span),
            expr::call(opt, expr, args.into_inner()),
          );
        }
        // expr_index
        BracketL => {
          let key = self.span(|p| p.wrap(BRACKETS, |p| p.span(Self::parse_expr)))?;
          expr = Spanned::new(
            expr.span.join(key.span),
            expr::get_index(opt, expr, key.into_inner()),
          );
        }
        // expr_field, expr_cast, expr_inst
        Dot => {
          self.bump();
          if self.current().is(ParenL) {
            // expr_cast
            let ty = self.span(|p| p.wrap(PARENS, |p| p.span(Self::parse_type)))?;
            expr = Spanned::new(
              expr.span.join(ty.span),
              expr::cast(opt, expr, ty.into_inner()),
            );
          } else if !opt && self.current().is(Less) {
            // expr_inst
            let args = self.span(|p| p.wrap_list(ANGLES, Comma, |p| p.span(Self::parse_type)))?;
            expr = Spanned::new(
              expr.span.join(args.span),
              expr::inst(expr, args.into_inner()),
            );
          } else {
            // expr_field
            let key = self.parse_ident()?;
            expr = Spanned::new(expr.span.join(key.span), expr::get_field(opt, expr, key));
          }
        }
        // expr_class
        BraceL if !opt && !self.ctx.expr_before_block => {
          let Some(target) = expr_class_target_to_path(&expr) else {
            return Err(Error::invalid_class_inst(expr.span))
          };
          let fields = self.span(|p| {
            p.wrap_list(BRACES, Comma, |p| {
              let key = p.parse_ident()?;
              p.expect(Colon)?;
              let value = p.span(Self::parse_expr)?;
              Ok((key, value))
            })
          })?;
          expr = Spanned::new(
            expr.span.join(fields.span),
            expr::class(target, fields.into_inner()),
          );
        }
        _ if opt => {
          return Err(Error::invalid_optional(
            self.previous().span.join(self.current().span),
          ));
        }
        _ => break,
      }
    }
    Ok(expr.into_inner())
  }

  fn parse_expr_primary(&mut self) -> Result<ExprKind<'a>, Error> {
    use TokenKind::{
      Bool, BraceL, BraceR, BracketL, BracketR, Break, Catch, Comma, Continue, Else, Eof, Float,
      Ident, If, Int, Lambda, Null, ParenL, ParenR, Return, Semicolon, Spawn, String, Throw, Try,
    };
    check_recursion_limit(self.current().span)?;

    // expr_null
    if self.bump_if(Null) {
      return Ok(expr::null());
    }

    // expr_bool
    if let Bool(v) = self.current().kind {
      self.bump();
      return Ok(expr::bool(v));
    }

    // expr_number -> int
    if self.bump_if(Int) {
      let token = self.previous();
      return Ok(expr::int(
        token
          .lexeme
          .parse()
          .map_err(|e| Error::parse_int(token.span, e))?,
      ));
    }

    // expr_number -> float
    if self.bump_if(Float) {
      let token = self.previous();
      let value = if token.lexeme.as_ref() == "inf" {
        std::f32::INFINITY
      } else {
        token
          .lexeme
          .parse()
          .map_err(|e| Error::parse_float(token.span, e))?
      };
      return Ok(expr::float(value));
    }

    // expr_string
    if self.bump_if(String) {
      let token = self.previous();
      let mut value = token.lexeme.trim_matches('"').to_string();
      if unescape_in_place(&mut value).is_none() {
        return Err(Error::invalid_escape_sequence(token.span));
      }
      return Ok(expr::string(value));
    }

    // expr_array
    if self.bump_if(BracketL) {
      if self.bump_if(BracketR) {
        // empty array
        return Ok(expr::array_list(vec![]));
      }

      let item = self.span(Self::parse_expr)?;
      if self.bump_if(Semicolon) {
        // array_copy
        let n = self.span(Self::parse_expr)?;
        self.expect(BracketR)?;
        return Ok(expr::array_copy(item, n));
      }

      // array_list
      let mut items = vec![item];
      while self.bump_if(Comma) && !self.current().is(BracketR) {
        items.push(self.span(Self::parse_expr)?);
      }
      self.expect(BracketR)?;
      return Ok(expr::array_list(items));
    }

    // expr_var
    if self.bump_if(Ident) {
      let ident = to_ident(self.previous());
      return Ok(expr::get_var(ident));
    }

    // expr_ctrl
    if self.bump_if(Return) {
      let value = if !self
        .current()
        .is_any(&[ParenR, BracketR, BraceR, Comma, Semicolon, Eof])
      {
        Some(self.span(Self::parse_expr)?)
      } else {
        None
      };
      return Ok(expr::return_(value));
    }

    if self.bump_if(Throw) {
      let value = if !self
        .current()
        .is_any(&[ParenR, BracketR, BraceR, Comma, Semicolon, Eof])
      {
        Some(self.span(Self::parse_expr)?)
      } else {
        None
      };
      return Ok(expr::throw_(value));
    }

    if self.bump_if(Continue) {
      return Ok(expr::continue_());
    }

    if self.bump_if(Break) {
      return Ok(expr::break_());
    }

    // expr_block
    if self.current().is(BraceL) {
      let block = self.parse_block()?;
      return Ok(expr::block(block));
    }

    // expr_if
    if self.bump_if(If) {
      let mut branches = vec![];
      let mut else_ = None;

      // TODO: make block optional for keyword-prefixed nodes:
      // return, throw, break, continue, for, while, loop, try, spawn
      // if
      branches.push((
        self.span(Self::parse_expr_before_block)?,
        self.parse_block()?,
      ));
      while self.bump_if(Else) {
        if self.bump_if(If) {
          // else if
          branches.push((
            self.span(Self::parse_expr_before_block)?,
            self.parse_block()?,
          ));
        } else {
          // else
          else_ = Some(self.parse_block()?);
          break;
        }
      }

      return Ok(expr::if_(branches, else_));
    }

    // expr_try
    if self.bump_if(Try) {
      let body = if self.current().is(BraceL) {
        self.parse_block()?
      } else {
        let expr = self.span(Self::parse_expr)?;
        Block {
          items: vec![],
          last: Some(expr),
        }
      };

      self.expect(Catch)?;
      let catch_var = if self.bump_if(ParenL) {
        let ident = self.parse_ident()?;
        self.expect(ParenR)?;
        ident
      } else {
        self.parse_ident()?
      };

      let catch_body = self.parse_block()?;

      return Ok(expr::try_(body, (catch_var, catch_body)));
    }

    // expr_spawn
    if self.bump_if(Spawn) {
      if self.current().is(BraceL) {
        // spawn_block
        let body = self.parse_block()?;
        return Ok(expr::spawn_block(body));
      } else {
        // spawn_call
        let expr = self.span(Self::parse_expr)?;
        let span = expr.span;
        let ExprKind::Call(c) = expr.into_inner() else {
          return Err(Error::invalid_spawn(span))
        };
        return Ok(expr::spawn_call(*c));
      }
    }

    // expr_lambda
    if self.bump_if(Lambda) {
      let params = if self.current().is(ParenL) {
        self.wrap_list(PARENS, Comma, Self::parse_ident)?
      } else if self.current().is(Ident) {
        vec![self.parse_ident()?]
      } else {
        vec![]
      };
      let body = self.parse_block()?;
      return Ok(expr::lambda(params, body));
    }

    // expr_group or expr_tuple
    if self.current().is(ParenL) {
      let expr = self.wrap(PARENS, |p| {
        let first = p.span(Self::parse_expr)?;

        if p.current().is(Comma) {
          // expr_tuple
          let mut items = vec![first];
          while p.bump_if(Comma) && !p.current().is(ParenR) {
            items.push(p.span(Self::parse_expr)?);
          }
          return Ok(expr::tuple(items));
        }

        // expr_group
        Ok(first.into_inner())
      })?;
      return Ok(expr);
    }

    // + unexpected token
    Err(Error::unexpected(self.current()))
  }

  // `"{" (stmt ";")* expr? "}"
  fn parse_block(&mut self) -> Result<Block<'a>, Error> {
    use TokenKind::{BraceL, BraceR, For, Let, Loop, Semicolon, While};
    check_recursion_limit(self.current().span)?;

    let mut block = Block {
      items: vec![],
      last: None,
    };

    self.expect(BraceL)?;
    while !self.current().is(BraceR) {
      let stmt = if self.bump_if(For) {
        self.span(Self::parse_stmt_loop_for)?
      } else if self.bump_if(While) {
        self.span(Self::parse_stmt_loop_while)?
      } else if self.bump_if(Loop) {
        self.span(Self::parse_stmt_loop_inf)?
      } else if self.bump_if(Let) {
        let stmt = self.span(Self::parse_stmt_let)?;
        self.expect(Semicolon)?;
        stmt
      } else {
        let expr = self.span(Self::parse_expr)?;

        // final expression without semicolon = return value
        if self.current().is(BraceR) {
          block.last = Some(expr);
          break;
        }
        self.expect(Semicolon)?;
        Spanned::new(expr.span, stmt::expr(expr))
      };
      block.items.push(stmt);
    }
    self.expect(BraceR)?;

    Ok(block)
  }

  fn parse_type(&mut self) -> Result<TypeKind<'a>, Error> {
    check_recursion_limit(self.current().span)?;
    self.parse_type_option()
  }

  fn parse_type_option(&mut self) -> Result<TypeKind<'a>, Error> {
    use TokenKind::Optional;

    let ty = self.span(Self::parse_type_primary)?;
    if self.bump_if(Optional) {
      // type_optional
      return Ok(ty::option(ty));
    };

    Ok(ty.into_inner())
  }

  fn parse_type_primary(&mut self) -> Result<TypeKind<'a>, Error> {
    use TokenKind::{ArrowThin, BracketL, Comma, Fn, Ident, ParenL, ParenR};

    // type_path
    if self.current().is(Ident) {
      let path = self.parse_path()?;
      return Ok(ty::path(path));
    }

    // type_fn
    if self.bump_if(Fn) {
      let args = self.wrap_list(PARENS, Comma, |p| p.span(Self::parse_type))?;
      let ret = self.maybe(ArrowThin, |p, _| p.span(Self::parse_type))?;
      let ty = ty::fn_(args, ret);
      return Ok(ty);
    }

    // type_array
    if self.current().is(BracketL) {
      let item = self.wrap(BRACKETS, |p| p.span(Self::parse_type))?;
      let ty = ty::array(item);
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
          return Ok(ty::tuple(items));
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
    self.expect(Ident).map(to_ident)
  }

  fn parse_path(&mut self) -> Result<Path<'a>, Error> {
    use TokenKind::Dot;

    let mut segments = vec![];

    segments.push(self.parse_segment()?);
    while self.bump_if(Dot) {
      segments.push(self.parse_segment()?);
    }

    Ok(Path { segments })
  }

  fn parse_segment(&mut self) -> Result<Segment<'a>, Error> {
    use TokenKind::{BracketL, Comma};

    let ident = self.parse_ident()?;
    let generic_args = if self.current().is(BracketL) {
      self.wrap_list(BRACKETS, Comma, |p| p.span(Self::parse_type))?
    } else {
      vec![]
    };

    Ok(Segment {
      ident,
      generic_args,
    })
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
    use TokenKind::{
      Class, Eof, Fn, For, Impl, Let, Loop, Pub, Semicolon, Trait, Type, Use, While,
    };

    self.bump();
    while !self.previous().is(Eof) {
      // semicolons terminate statement, so it's safe to start parsing again after
      // we encounter one.
      if self.previous().is(Semicolon) {
        break;
      }

      // all of these tokens begin a statement that cannot also appear as an
      // expression, so they are good candidates for synchronization.
      match self.current().kind {
        Pub | Use | Fn | Type | Class | Trait | Impl | Let | For | While | Loop => break,
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
    let start = self.current().span;
    cons(self).map(|value| {
      let end = self.previous().span;
      Spanned::new(start.join(end), value)
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

  /// Calls `cons` in the context `ctx`.
  /// `ctx` is used only for the duration of the call to `cons`.
  #[inline]
  fn with_ctx<T>(
    &mut self,
    ctx: Context,
    cons: impl FnOnce(&mut Self) -> Result<T, Error>,
  ) -> Result<T, Error> {
    let mut prev_ctx = std::mem::replace(&mut self.ctx, ctx);
    let res = cons(self);
    std::mem::swap(&mut self.ctx, &mut prev_ctx);
    res
  }
}

// On average, a single parse_XXX() method consumes between 10 and 700 bytes of
// stack space. Assuming ~50 recursive calls per dive and 700 bytes of stack
// space per call, we'll require 50 * 700 = 35k bytes of stack space in order
// to dive. For future proofing, we round this value up to 64k bytes.
const MINIMUM_STACK_REQUIRED: usize = 64_000;

// On WASM, remaining_stack() will always return None. Stack overflow panics
// are converted to exceptions and handled by the host, which means a
// `try { ... } catch { ... }` around a call to one of the Mu compiler functions
// would be enough.
#[cfg(target_family = "wasm")]
fn check_recursion_limit(_span: Span) -> Result<(), Error> {
  Ok(())
}

#[cfg(not(target_family = "wasm"))]
fn check_recursion_limit(span: Span) -> Result<(), Error> {
  // On the platforms we're targeting (linux, windows, osx), remaining_stack()
  // should always return Some, so we'll able to bail out if an overflow is
  // likely. On all other platforms, we'll continue parsing, but warn the user
  // at compile time.
  #[cfg(not(any(target_os = "linux", target_os = "windows", target_os = "macos")))]
  {
    const WARNING: &str = "The detected platform is neither Linux, Windows, MacOS, or WASM, which means that the parser may panic on stack overflow. Use with care.";
  }

  if stacker::remaining_stack()
    .map(|available| available > MINIMUM_STACK_REQUIRED)
    .unwrap_or(true)
  {
    Ok(())
  } else {
    Err(Error::NestingLimitReached(span))
  }
}

// Adapted from https://docs.rs/snailquote/0.3.0/x86_64-pc-windows-msvc/src/snailquote/lib.rs.html.
/// Unescapes the given string in-place. Returns `None` if the string contains
/// an invalid escape sequence.
fn unescape_in_place(s: &mut String) -> Option<()> {
  let mut out = String::with_capacity(s.len());
  let mut chars = s.chars();
  while let Some(ch) = chars.next() {
    if ch == '\\' {
      if let Some(next) = chars.next() {
        let escape = match next {
          'a' => Some('\u{07}'),
          'b' => Some('\u{08}'),
          'v' => Some('\u{0B}'),
          'f' => Some('\u{0C}'),
          'n' => Some('\n'),
          'r' => Some('\r'),
          't' => Some('\t'),
          '\'' => Some('\''),
          '"' => Some('"'),
          'e' | 'E' => Some('\u{1B}'),
          'u' => Some(parse_unicode(&mut chars)?),
          _ => None,
        };
        match escape {
          Some(esc) => {
            out.push(esc);
          }
          None => {
            out.push(ch);
            out.push(next);
          }
        }
      }
    } else {
      out.push(ch);
    }
  }
  *s = out;
  Some(())
}

// Adapted from https://docs.rs/snailquote/0.3.0/x86_64-pc-windows-msvc/src/snailquote/lib.rs.html.
fn parse_unicode<I>(chars: &mut I) -> Option<char>
where
  I: Iterator<Item = char>,
{
  match chars.next() {
    Some('{') => {}
    _ => {
      return None;
    }
  }

  let unicode_seq: String = chars.take_while(|&c| c != '}').collect();

  u32::from_str_radix(&unicode_seq, 16)
    .ok()
    .and_then(char::from_u32)
}

fn to_ident<'a>(t: &Token<'a>) -> Ident<'a> {
  Spanned::new(t.span, t.lexeme.clone())
}

fn check_assign_target<'a>(target: &Expr<'a>) -> Result<(), Error> {
  match target.deref() {
    ExprKind::GetVar(..) => Ok(()),
    ExprKind::GetField(ref get) => {
      if get.opt {
        Err(Error::invalid_assign(target.span))
      } else {
        check_assign_target(&get.target)
      }
    }
    ExprKind::GetIndex(ref get) => check_assign_target(&get.target),
    _ => Err(Error::invalid_assign(target.span)),
  }
}

fn expr_class_target_to_path<'a>(target: &Expr<'a>) -> Option<Path<'a>> {
  let mut segments = vec![];

  // walk the AST, looking for Inst/GetVar/GetField, and the segments onto a stack
  let mut expr = target;
  let mut next_generic_args = None;
  loop {
    match expr.deref() {
      // A generic instantiation sets the `next_generic_args`, which will be applied to the next
      // segment in the path
      // T.<int>.<int> is invalid, so `next_generic_args` must be `None`
      ExprKind::Inst(v) if next_generic_args.is_none() => {
        next_generic_args = Some(&v.args);
        expr = &v.target;
      }
      // A variable is the base case, as it has no sub-expressions
      ExprKind::GetVar(v) => {
        segments.push(Segment {
          ident: v.name.clone(),
          generic_args: next_generic_args.take().cloned().unwrap_or_default(),
        });
        break;
      }
      // For fields, we walk the subtree
      ExprKind::GetField(v) => {
        segments.push(Segment {
          ident: v.key.clone(),
          generic_args: next_generic_args.take().cloned().unwrap_or_default(),
        });
        expr = &v.target;
      }
      _ => return None,
    }
  }

  // The expr tree is walked backwards, so we have to reverse the segments:
  segments.reverse();

  Some(Path { segments })
}

trait WithElem<T> {
  /// Clone `Self` and append `elem` to it
  fn with_elem(&self, elem: T) -> Self;
}

impl<T: Clone> WithElem<T> for Vec<T> {
  #[inline]
  fn with_elem(&self, elem: T) -> Self {
    let mut out = Vec::<T>::with_capacity(self.len() + 1);
    out.extend(self.iter().cloned());
    out.extend([elem]);
    out
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
  #[error("invalid assignment target at {0}")]
  InvalidAssign(Span),
  #[error("string at {0} contains an invalid escape sequence")]
  InvalidEscapeSequence(Span),
  #[error("expr at {0} cannot be the instantiated")]
  InvalidClassInst(Span),
  #[error("only blocks or function calls may be spawned")]
  InvalidSpawn(Span),
  #[error("invalid optional expr at {0}")]
  InvalidOptional(Span),
  #[error("failed to parse int at {0}: {1}")]
  ParseInt(Span, #[source] std::num::ParseIntError),
  #[error("failed to parse float at {0}: {1}")]
  ParseFloat(Span, #[source] std::num::ParseFloatError),
  // NOTE: temporary error to avoid false-positives while fuzzing.
  #[error("parsing {0} is not yet implemented")]
  NotImplemented(&'static str),
  #[error("the maximum amount of nesting has been reached")]
  NestingLimitReached(Span),
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

  fn invalid_assign(span: Span) -> Self {
    Error::InvalidAssign(span)
  }

  fn invalid_escape_sequence(span: Span) -> Self {
    Error::InvalidEscapeSequence(span)
  }

  fn invalid_class_inst(span: Span) -> Self {
    Error::InvalidClassInst(span)
  }

  fn invalid_spawn(span: Span) -> Self {
    Error::InvalidSpawn(span)
  }

  fn invalid_optional(span: Span) -> Self {
    Error::InvalidOptional(span)
  }

  fn parse_int(span: Span, e: std::num::ParseIntError) -> Self {
    Error::ParseInt(span, e)
  }

  fn parse_float(span: Span, e: std::num::ParseFloatError) -> Self {
    Error::ParseFloat(span, e)
  }
}

#[cfg(test)]
mod tests;
