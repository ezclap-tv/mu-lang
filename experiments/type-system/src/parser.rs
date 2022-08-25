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

    self.bump();

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
    self.expect(TokenKind::Semicolon)?;
    Ok(expr)
  }

  fn parse_expr(&mut self) -> Result<Expr<'a>> {
    let expr = if self.bump_if(TokenKind::Let) {
      self.parse_let_expr()
    } else if self.bump_if(TokenKind::If) {
      self.parse_if_expr()
    } else {
      self.parse_simple_expr()
    };
    expr
  }

  fn parse_let_expr(&mut self) -> Result<Expr<'a>> {
    let ident = self.expect(TokenKind::Ident)?;

    let kind = if self.bump_if(TokenKind::Ident) {
      self.parse_let_func_expr(ident)?
    } else {
      self.parse_let_var_expr(ident)?
    };

    let in_ = if self.bump_if(TokenKind::In) {
      Some(Box::new(self.parse_expr()?))
    } else {
      None
    };

    Ok(Expr::Let(Let { kind, in_ }))
  }

  fn parse_let_func_expr(&mut self, ident: Token<'a>) -> Result<LetKind<'a>> {
    // function
    let param = {
      let ident = self.previous.clone();
      self.expect(TokenKind::Colon)?;
      let type_ = self.parse_type()?;
      (ident, type_)
    };
    self.expect(TokenKind::Arrow)?;
    let ret = self.parse_type()?;
    self.expect(TokenKind::Equal)?;
    let body = Box::new(self.parse_expr()?);
    Ok(LetKind::Func(Func {
      ident,
      param,
      ret,
      body,
    }))
  }

  fn parse_let_var_expr(&mut self, ident: Token<'a>) -> Result<LetKind<'a>> {
    let type_ = if self.bump_if(TokenKind::Colon) {
      Some(self.parse_type()?)
    } else {
      None
    };
    // variable
    self.expect(TokenKind::Equal)?;
    let value = Box::new(self.parse_expr()?);
    Ok(LetKind::Var(Var {
      ident,
      value,
      type_,
    }))
  }

  fn parse_if_expr(&mut self) -> Result<Expr<'a>> {
    let cond = Box::new(self.parse_expr()?);
    self.expect(TokenKind::Then)?;
    let then = Box::new(self.parse_expr()?);
    let else_ = if self.bump_if(TokenKind::Else) {
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
    while self.bump_if(TokenKind::Or) {
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
    while self.bump_if(TokenKind::And) {
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
    while self.bump_for(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
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
    while self.bump_for(&[
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
    while self.bump_for(&[TokenKind::Plus, TokenKind::Minus]) {
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
    while self.bump_for(&[TokenKind::Star, TokenKind::Slash, TokenKind::Percent]) {
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
    let mut expr = self.parse_prefix_expr()?;
    while self.bump_for(&[TokenKind::Power]) {
      expr = Expr::Binary(Binary {
        op: BinaryOp::Power,
        lhs: Box::new(expr),
        rhs: Box::new(self.parse_prefix_expr()?),
      })
    }
    Ok(expr)
  }

  fn parse_prefix_expr(&mut self) -> Result<Expr<'a>> {
    if self.bump_if(TokenKind::Tilde) {
      self.parse_negate_expr()
    } else {
      self.parse_postfix_expr()
    }
  }

  fn parse_negate_expr(&mut self) -> Result<Expr<'a>> {
    // `1` because we've already parsed the first `~`
    let mut n = 1;
    while self.bump_if(TokenKind::Tilde) {
      n += 1;
    }
    let rhs = self.parse_postfix_expr()?;

    if n % 2 == 0 {
      // `N` negations cancel out for any even `N`
      Ok(rhs)
    } else {
      // `N` negations are equal to 1 negation for any odd `N`
      Ok(Expr::Unary(Unary {
        op: UnaryOp::Negate,
        rhs: Box::new(rhs),
      }))
    }
  }

  fn parse_postfix_expr(&mut self) -> Result<Expr<'a>> {
    let mut expr = self.parse_primary_expr()?;
    while self.bump_for(&[TokenKind::Dot, TokenKind::LParen, TokenKind::LBrace]) {
      expr = match self.previous.kind {
        TokenKind::Dot => self.parse_access_expr(expr)?,
        TokenKind::LParen => self.parse_call_expr(expr)?,
        TokenKind::LBrace => self.parse_record_call_expr(expr)?,
        _ => unreachable!(),
      };
    }
    Ok(expr)
  }

  fn parse_access_expr(&mut self, prev: Expr<'a>) -> Result<Expr<'a>> {
    Ok(Expr::Access(Access {
      target: Box::new(prev),
      field: self.expect(TokenKind::Ident)?,
    }))
  }

  fn parse_call_expr(&mut self, prev: Expr<'a>) -> Result<Expr<'a>> {
    let arg = Box::new(self.parse_expr()?);
    self.expect(TokenKind::RParen)?;
    Ok(Expr::Call(Call {
      func: Box::new(prev),
      arg,
    }))
  }

  fn parse_record_call_expr(&mut self, prev: Expr<'a>) -> Result<Expr<'a>> {
    let arg = Box::new(self.parse_record_expr()?);
    // `parse_record_expr` consumes the `RBrace`
    Ok(Expr::Call(Call {
      func: Box::new(prev),
      arg,
    }))
  }

  fn parse_primary_expr(&mut self) -> Result<Expr<'a>> {
    if self.bump_if(TokenKind::Integer) {
      self.parse_int_expr()
    } else if self.bump_if(TokenKind::Bool) {
      self.parse_bool_expr()
    } else if self.bump_if(TokenKind::String) {
      self.parse_str_expr()
    } else if self.bump_if(TokenKind::LBrace) {
      self.parse_record_expr()
    } else if self.bump_if(TokenKind::Ident) {
      self.parse_use_expr()
    } else if self.bump_if(TokenKind::LParen) {
      self.parse_group_expr()
    } else {
      self.bump();
      Err(Error::UnexpectedToken {
        token: self.previous.clone().into_static(),
      })
    }
  }

  fn parse_int_expr(&mut self) -> Result<Expr<'a>> {
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
  }

  fn parse_bool_expr(&mut self) -> Result<Expr<'a>> {
    Ok(Expr::Lit(Lit::Bool(match self.previous.lexeme.as_ref() {
      "true" => true,
      "false" => false,
      _ => unreachable!(),
    })))
  }

  fn parse_str_expr(&mut self) -> Result<Expr<'a>> {
    Ok(Expr::Lit(Lit::String(self.previous.lexeme.clone())))
  }

  fn parse_use_expr(&mut self) -> Result<Expr<'a>> {
    let ident = self.previous.clone();
    Ok(Expr::Use(Use { ident }))
  }

  fn parse_record_expr(&mut self) -> Result<Expr<'a>> {
    let mut fields = vec![];
    if !self.check_if(TokenKind::RBrace) {
      let ident = self.expect(TokenKind::Ident)?;
      self.expect(TokenKind::Colon)?;
      let value = Box::new(self.parse_expr()?);
      fields.push(Field { ident, value });
      while self.bump_if(TokenKind::Comma) {
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let value = Box::new(self.parse_expr()?);
        fields.push(Field { ident, value })
      }
    }
    self.expect(TokenKind::RBrace)?;
    Ok(Expr::Lit(Lit::Record(fields)))
  }

  fn parse_group_expr(&mut self) -> Result<Expr<'a>> {
    // count parentheses to prevent recursion problems
    let mut n = 1;
    while self.bump_if(TokenKind::LParen) {
      n += 1;
    }
    let expr = self.parse_expr()?;
    // consume all closing parentheses
    for _ in 0..n {
      self.expect(TokenKind::RParen)?;
    }
    Ok(expr)
  }

  fn parse_type(&mut self) -> Result<Type<'a>> {
    if self.bump_if(TokenKind::LBrace) {
      self.parse_record_type()
    } else if self.bump_if(TokenKind::Ident) {
      self.parse_use_type()
    } else if self.bump_if(TokenKind::LParen) {
      self.parse_group_type()
    } else {
      self.bump();
      Err(Error::UnexpectedToken {
        token: self.previous.clone().into_static(),
      })
    }
  }

  fn parse_record_type(&mut self) -> Result<Type<'a>> {
    let mut fields = vec![];
    if !self.check_if(TokenKind::RBrace) {
      let ident = self.expect(TokenKind::Ident)?;
      self.expect(TokenKind::Colon)?;
      let ty = Box::new(self.parse_type()?);
      fields.push((ident, ty));
      while self.bump_if(TokenKind::Comma) {
        let ident = self.expect(TokenKind::Ident)?;
        self.expect(TokenKind::Colon)?;
        let ty = Box::new(self.parse_type()?);
        fields.push((ident, ty))
      }
    }
    self.expect(TokenKind::RBrace)?;
    Ok(Type::Record(fields))
  }

  fn parse_use_type(&mut self) -> Result<Type<'a>> {
    let ident = self.previous.clone();
    Ok(Type::Ident(ident))
  }

  fn parse_group_type(&mut self) -> Result<Type<'a>> {
    // count parentheses to prevent recursion problems
    let mut n = 1;
    while self.bump_if(TokenKind::LParen) {
      n += 1;
    }
    let ty = self.parse_type()?;
    // consume all closing parentheses
    for _ in 0..n {
      self.expect(TokenKind::RParen)?;
    }
    Ok(ty)
  }

  // utilities

  fn expect(&mut self, kind: TokenKind) -> Result<Token<'a>> {
    if self.check_if(kind) {
      Ok(self.bump())
    } else {
      Err(Error::MissingToken {
        expected: kind,
        found: self.current.clone().into_static(),
      })
    }
  }

  fn bump_if(&mut self, kind: TokenKind) -> bool {
    if self.check_if(kind) {
      self.bump();
      true
    } else {
      false
    }
  }

  fn bump_for(&mut self, kinds: &[TokenKind]) -> bool {
    if self.check_for(kinds) {
      self.bump();
      true
    } else {
      false
    }
  }

  fn check_if(&mut self, kind: TokenKind) -> bool {
    discriminant(&self.current.kind) == discriminant(&kind)
  }

  fn check_for(&mut self, kinds: &[TokenKind]) -> bool {
    kinds.iter().any(|k| self.check_if(*k))
  }

  fn bump(&mut self) -> Token<'a> {
    std::mem::swap(&mut self.previous, &mut self.current);
    self.current = self.lexer.next().unwrap_or_else(|| self.eof.clone());
    self.previous.clone()
  }

  fn done(&self) -> bool {
    self.current.kind == TokenKind::Eof
  }

  fn sync(&mut self) {
    while !self.done() {
      match self.current.kind {
        TokenKind::Let => return,
        TokenKind::If => return,
        _ => self.bump(),
      };
    }
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
