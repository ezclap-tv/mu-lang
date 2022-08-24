use std::mem::discriminant;

use crate::ast::*;
use crate::error::*;
use crate::lexer::*;

struct Parser<'a> {
  lexer: Lexer<'a>,
  previous: Token<'a>,
  current: Token<'a>,
  eof: Token<'a>,
}

impl<'a> Parser<'a> {
  fn run(mut self) -> std::result::Result<Vec<Expr<'a>>, Vec<Error>> {
    let mut exprs = vec![];
    let mut errors = vec![];

    self.advance();

    while !self.done() {
      match self.parse_top_level_expr() {
        Ok(expr) => exprs.push(expr),
        Err(error) => {
          errors.push(error);
          self.sync();
        }
      }
    }

    if errors.is_empty() {
      Ok(exprs)
    } else {
      Err(errors)
    }
  }

  fn parse_top_level_expr(&mut self) -> Result<Expr<'a>> {
    let expr = self.parse_expr()?;
    self.consume(TokenKind::Semicolon)?;
    Ok(expr)
  }

  fn parse_expr(&mut self) -> Result<Expr<'a>> {
    let expr = if self.match_(TokenKind::Let) {
      self.parse_let_expr()
    } else if self.match_(TokenKind::If) {
      self.parse_if_expr()
    } else {
      self.parse_simple_expr()
    };
    expr
  }

  fn parse_let_expr(&mut self) -> Result<Expr<'a>> {
    let ident = self.consume(TokenKind::Ident)?;

    let kind = if self.match_(TokenKind::Ident) {
      // function
      let param = {
        let ident = self.previous.clone();
        self.consume(TokenKind::Colon)?;
        let type_ = self.parse_type()?;
        (ident, type_)
      };
      self.consume(TokenKind::Arrow)?;
      let ret = self.parse_type()?;
      self.consume(TokenKind::Equal)?;
      let body = Box::new(self.parse_expr()?);
      LetKind::Func(Func {
        ident,
        param,
        ret,
        body,
      })
    } else {
      let type_ = if self.match_(TokenKind::Colon) {
        Some(self.parse_type()?)
      } else {
        None
      };
      // variable
      self.consume(TokenKind::Equal)?;
      let value = Box::new(self.parse_expr()?);
      LetKind::Var(Var {
        ident,
        value,
        type_,
      })
    };

    let in_ = if self.match_(TokenKind::In) {
      Some(Box::new(self.parse_expr()?))
    } else {
      None
    };

    Ok(Expr::Let(Let { kind, in_ }))
  }

  fn parse_if_expr(&mut self) -> Result<Expr<'a>> {
    let cond = Box::new(self.parse_expr()?);
    self.consume(TokenKind::Then)?;
    let then = Box::new(self.parse_expr()?);
    let else_ = if self.match_(TokenKind::Else) {
      Some(Box::new(self.parse_expr()?))
    } else {
      None
    };
    Ok(Expr::If(If { cond, then, else_ }))
  }

  fn parse_simple_expr(&mut self) -> Result<Expr<'a>> {
    self.parse_or_expr()
  }

  /* fn parse_assign_expr(&mut self) -> Result<Expr<'a>> {
    let expr = self.parse_or_expr()?;
    if self.match_(TokenKind::Equal) {
      let rhs = Box::new(self.parse_or_expr()?);
      Ok(Expr::Assign(Assign {
        lhs: Box::new(expr),
        rhs,
      }))
    } else {
      Ok(expr)
    }
  } */

  fn parse_or_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_and_expr()?;
    while self.match_(TokenKind::Or) {
      expr = Expr::Binary(Binary {
        op: BinaryOp::Or,
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_and_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_and_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_eq_expr()?;
    while self.match_(TokenKind::And) {
      expr = Expr::Binary(Binary {
        op: BinaryOp::And,
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_eq_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_eq_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_comp_expr()?;
    while self.match_any(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
      expr = Expr::Binary(Binary {
        op: match self.previous.kind {
          TokenKind::EqualEqual => BinaryOp::Equal,
          TokenKind::BangEqual => BinaryOp::NotEqual,
          _ => unreachable!(),
        },
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_comp_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_comp_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_term_expr()?;
    while self.match_any(&[
      TokenKind::More,
      TokenKind::Less,
      TokenKind::MoreEqual,
      TokenKind::LessEqual,
    ]) {
      expr = Expr::Binary(Binary {
        op: match self.previous.kind {
          TokenKind::More => BinaryOp::GreaterThan,
          TokenKind::Less => BinaryOp::LessThan,
          TokenKind::MoreEqual => BinaryOp::GreaterEqual,
          TokenKind::LessEqual => BinaryOp::LessEqual,
          _ => unreachable!(),
        },
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_term_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_term_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_factor_expr()?;
    while self.match_any(&[TokenKind::Plus, TokenKind::Minus]) {
      expr = Expr::Binary(Binary {
        op: match self.previous.kind {
          TokenKind::Plus => BinaryOp::Add,
          TokenKind::Minus => BinaryOp::Sub,
          _ => unreachable!(),
        },
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_factor_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_factor_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_power_expr()?;
    while self.match_any(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
      expr = Expr::Binary(Binary {
        op: match self.previous.kind {
          TokenKind::Star => BinaryOp::Mult,
          TokenKind::Slash => BinaryOp::Div,
          TokenKind::Percent => BinaryOp::Rem,
          _ => unreachable!(),
        },
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_power_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_power_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_unary_expr()?;
    while self.match_any(&[TokenKind::Power]) {
      expr = Expr::Binary(Binary {
        op: BinaryOp::Power,
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_unary_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_unary_expr(&mut self) -> Result<Expr<'a>> {
    // TODO: can easily overflow the stack here
    if self.match_any(&[TokenKind::Bang, TokenKind::Minus]) {
      Ok(Expr::Unary(Unary {
        op: match self.previous.kind {
          TokenKind::Bang => UnaryOp::Not,
          TokenKind::Minus => UnaryOp::Negate,
          _ => unreachable!(),
        },
        rhs: Box::new(self.parse_unary_expr()?),
      }))
    } else {
      self.parse_call_expr()
    }
  }

  fn parse_call_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_primary_expr()?;
    while self.match_any(&[TokenKind::Dot, TokenKind::LParen, TokenKind::LBrace]) {
      expr = match self.previous.kind {
        TokenKind::Dot => Expr::Access(Access {
          target: Box::new(expr),
          field: self.consume(TokenKind::Ident)?,
        }),
        TokenKind::LParen => {
          let arg = Box::new(self.parse_expr()?);
          self.consume(TokenKind::RParen)?;
          Expr::Call(Call {
            func: Box::new(expr),
            arg,
          })
        }
        TokenKind::LBrace => {
          // `parse_record_expr` consumes the `RBrace`
          let arg = Box::new(self.parse_record_expr()?);
          Expr::Call(Call {
            func: Box::new(expr),
            arg,
          })
        }
        _ => unreachable!(),
      };
    }
    Ok(expr)
  }

  fn parse_primary_expr(&mut self) -> Result<Expr<'a>> {
    if self.match_(TokenKind::Integer) {
      Ok(Expr::Lit(Lit::Number(
        self
          .previous
          .lexeme
          .parse()
          .map_err(|inner| Error::InvalidNumber {
            token: self.previous.clone().into_static(),
            inner: format!("{inner}"),
          })?,
      )))
    } else if self.match_(TokenKind::Bool) {
      Ok(Expr::Lit(Lit::Bool(match self.previous.lexeme.as_ref() {
        "true" => true,
        "false" => false,
        _ => unreachable!(),
      })))
    } else if self.match_(TokenKind::String) {
      Ok(Expr::Lit(Lit::String(self.previous.lexeme.clone())))
    } else if self.match_(TokenKind::LBrace) {
      self.parse_record_expr()
    } else if self.match_(TokenKind::Ident) {
      let ident = self.previous.clone();
      Ok(Expr::Use(Use { ident }))
    } else if self.match_(TokenKind::LParen) {
      // count parentheses to prevent recursion problems
      let mut n = 1;
      while self.match_(TokenKind::LParen) {
        n += 1;
      }
      let expr = self.parse_expr()?;
      // consume all closing parentheses
      for _ in 0..n {
        self.consume(TokenKind::RParen)?;
      }
      Ok(expr)
    } else {
      self.advance();
      Err(Error::UnexpectedToken {
        token: self.previous.clone().into_static(),
      })
    }
  }

  fn parse_record_expr(&mut self) -> Result<Expr<'a>> {
    let mut fields = vec![];
    if !self.check(TokenKind::RBrace) {
      let ident = self.consume(TokenKind::Ident)?;
      self.consume(TokenKind::Colon)?;
      let value = Box::new(self.parse_expr()?);
      fields.push(Field { ident, value });
      while self.match_(TokenKind::Comma) {
        let ident = self.consume(TokenKind::Ident)?;
        self.consume(TokenKind::Colon)?;
        let value = Box::new(self.parse_expr()?);
        fields.push(Field { ident, value })
      }
    }
    self.consume(TokenKind::RBrace)?;
    Ok(Expr::Lit(Lit::Record(fields)))
  }

  fn parse_type(&mut self) -> Result<Type<'a>> {
    if self.match_(TokenKind::LBrace) {
      self.parse_record_type()
    } else if self.match_(TokenKind::Ident) {
      let ident = self.previous.clone();
      Ok(Type::Ident(ident))
    } else {
      self.advance();
      Err(Error::UnexpectedToken {
        token: self.previous.clone().into_static(),
      })
    }
  }

  fn parse_record_type(&mut self) -> Result<Type<'a>> {
    let mut fields = vec![];
    if !self.check(TokenKind::RBrace) {
      let ident = self.consume(TokenKind::Ident)?;
      self.consume(TokenKind::Colon)?;
      let value = Box::new(self.parse_type()?);
      fields.push((ident, value));
      while self.match_(TokenKind::Comma) {
        let ident = self.consume(TokenKind::Ident)?;
        self.consume(TokenKind::Colon)?;
        let value = Box::new(self.parse_type()?);
        fields.push((ident, value))
      }
    }
    self.consume(TokenKind::RBrace)?;
    Ok(Type::Record(fields))
  }

  fn consume(&mut self, kind: TokenKind) -> Result<Token<'a>> {
    if self.check(kind) {
      Ok(self.advance())
    } else {
      Err(Error::MissingToken {
        expected: kind,
        found: self.current.clone().into_static(),
      })
    }
  }

  fn match_(&mut self, kind: TokenKind) -> bool {
    if self.check(kind) {
      self.advance();
      true
    } else {
      false
    }
  }

  fn match_any(&mut self, kinds: &[TokenKind]) -> bool {
    if self.check_any(kinds) {
      self.advance();
      true
    } else {
      false
    }
  }

  fn check(&mut self, kind: TokenKind) -> bool {
    discriminant(&self.current.kind) == discriminant(&kind)
  }

  fn check_any(&mut self, kinds: &[TokenKind]) -> bool {
    kinds.iter().any(|k| self.check(*k))
  }

  fn sync(&mut self) {
    while !self.done() {
      match self.current.kind {
        TokenKind::Let => return,
        TokenKind::If => return,
        _ => self.advance(),
      };
    }
  }

  fn advance(&mut self) -> Token<'a> {
    std::mem::swap(&mut self.previous, &mut self.current);
    self.current = self.lexer.next().unwrap_or_else(|| self.eof.clone());
    self.previous.clone()
  }

  fn done(&self) -> bool {
    self.current.kind == TokenKind::Eof
  }
}

pub fn parse(source: &str) -> std::result::Result<Vec<Expr<'_>>, Vec<Error>> {
  let end = if source.is_empty() {
    0
  } else {
    source.len() - 1
  };
  let eof = Token::eof(end);

  Parser {
    lexer: Lexer::new(source),
    previous: eof.clone(),
    current: eof.clone(),
    eof,
  }
  .run()
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn isolated() {
    let source = r#"
    let v = 10;
    let v: int = 10;
    "#;

    match parse(source) {
      Ok(_) => {}
      Err(errors) => {
        let mut buf = String::new();
        crate::error::report(source, &errors, &mut buf);
        panic!("{buf}");
      }
    }
  }
}
