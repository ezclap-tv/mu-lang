use std::string::ParseError;

use mu_ast::*;
use mu_errors::{Error as MuError, ErrorMsg, ErrorSink};
use mu_lexer::numbers::FloatBits;
use mu_lexer::strings::{MalformedStringLiteral, StringLiteral};
use mu_lexer::{lexer, Lexer, NextTokenExt};

mod macros;

pub type Result<'t> = std::result::Result<Ast<'t>, ErrorSink<'t>>;
pub type ParseResult<T> = std::result::Result<T, MuError>;

pub fn parse(source: &'_ str) -> Result<'_> {
  let lexer = lexer(source);
  let parser = Parser::new(lexer);
  parser.parse()
}

struct Parser<'t> {
  lexer: Lexer<'t>,
  errors: ErrorSink<'t>,
  previous: Token<'t>,
  current: Token<'t>,
  eof: Token<'t>,
}

impl<'t> Parser<'t> {
  fn new(lexer: Lexer<'t>) -> Self {
    let source = lexer.source();
    let end = if source.is_empty() {
      0
    } else {
      source.len() - 1
    };
    let eof = Token::new("", end..end, TokenKind::EOF);

    Self {
      lexer,
      errors: ErrorSink::new(source),
      previous: eof.clone(),
      current: eof.clone(),
      eof,
    }
  }

  fn parse(mut self) -> Result<'t> {
    self.advance();

    let mut statements = Vec::new();
    while !self.at_end() {
      match self.top_level_stmt() {
        Ok(stmt) => statements.push(stmt),
        Err(e) => {
          self.errors.record(e);
          self.synchronize();
        }
      }
    }

    Ok(Ast { statements })
  }

  fn top_level_stmt(&mut self) -> ParseResult<Stmt<'t>> {
    if self.match_(&TokenKind::Import) {
      todo!("imports")
    } else if self.match_(&TokenKind::Export) {
      todo!("exports")
    } else {
      self.stmt()
    }
  }

  fn stmt(&mut self) -> ParseResult<Stmt<'t>> {
    if self.match_any(&[TokenKind::LeftBrace]) {
      return match self.previous.kind {
        TokenKind::LeftBrace => self.block_stmt(),
        _ => unreachable!(),
      };
    }

    self.expr_stmt()
  }

  fn expr_stmt(&mut self) -> ParseResult<Stmt<'t>> {
    let span_start = self.previous.span.start;
    let expr = self.expr()?;
    let span_end = self.current.span.end;

    self.skip_semi();
    self.skip_newline();
    Ok(Stmt::new(StmtKind::ExprStmt(expr), span_start..span_end))
  }

  fn expr(&mut self) -> ParseResult<Expr<'t>> {
    if self.match_any(&[
      TokenKind::Do,
      TokenKind::Match,
      TokenKind::Catch,
      TokenKind::Fn,
      TokenKind::Class,
      TokenKind::Enum,
      TokenKind::Spawn,
    ]) {
      todo!()
    }

    self.assignment()
  }

  fn assignment(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.pipeline()?;

    assignment! {
      if self matched = [
        Equal,
        PlusEqual,
        MinusEqual,
        SlashEqual,
        StarEqual,
        PercentEqual,
        PowerEqual,
        BitOrEqual,
        BitAndEqual,
        BitXorEqual,
        ShiftLeftEqual,
        ShiftRightEqual,
        OrEqual,
        AndEqual,
        CoalescingEqual
      ] => {
        self.check_assignment_target(&expr)?;
        let start = self.previous.span.start;
        let value = self.expr()?;
        let end = value.span.end;
        expr = Expr::new(ExprKind::Assignment(Box::new(Assignment {
          operator: matched,
          target: expr,
          value
        })), start..end);
      }
    }

    Ok(expr)
  }

  fn check_assignment_target(&self, expr: &Expr<'t>) -> ParseResult<()> {
    match expr.kind {
      ExprKind::Identifier(_) => Ok(()),
      _ => unimplemented!(),
    }
  }

  fn pipeline(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.logic_or()?;

    // expr |> expr
    while self.match_(&TokenKind::Pipeline) {
      let token = self.previous.clone();
      let right = self.logic_or()?;
      let span = expr.span.start..right.span.end;
      expr = Expr::new(
        ExprKind::Pipeline(Box::new(Pipeline {
          token,
          input: expr,
          output: right,
        })),
        span,
      );
    }

    Ok(expr)
  }

  fn logic_or(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.logic_and()?;

    while self.match_(&TokenKind::Or) {
      expr = binary!(self, expr, self.logic_and()?).unwrap();
    }

    Ok(expr)
  }

  fn logic_and(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.bitwise_or()?;

    while self.match_(&TokenKind::And) {
      expr = binary!(self, expr, self.bitwise_or()?).unwrap();
    }

    Ok(expr)
  }

  fn bitwise_or(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.bitwise_xor()?;

    while self.match_(&TokenKind::BitOr) {
      expr = binary!(self, expr, self.bitwise_xor()?).unwrap();
    }

    Ok(expr)
  }

  fn bitwise_xor(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.bitwise_and()?;

    while self.match_(&TokenKind::BitXor) {
      expr = binary!(self, expr, self.bitwise_and()?).unwrap();
    }

    Ok(expr)
  }

  fn bitwise_and(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.equality()?;

    while self.match_(&TokenKind::BitAnd) {
      expr = binary!(self, expr, self.equality()?).unwrap();
    }

    Ok(expr)
  }

  fn equality(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.comparison()?;

    while self.match_any(&[TokenKind::EqualEqual, TokenKind::BangEqual]) {
      expr = binary!(self, expr, self.comparison()?).unwrap();
    }

    Ok(expr)
  }

  fn comparison(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.bit_shift()?;

    while self.match_any(&[
      TokenKind::Less,
      TokenKind::LessEqual,
      TokenKind::Greater,
      TokenKind::GreaterEqual,
    ]) {
      expr = binary!(self, expr, self.bit_shift()?).unwrap();
    }

    Ok(expr)
  }

  fn bit_shift(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.addition()?;

    while self.match_any(&[TokenKind::ShiftLeft, TokenKind::ShiftRight]) {
      expr = binary!(self, expr, self.addition()?).unwrap();
    }

    Ok(expr)
  }

  fn addition(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.multiplication()?;

    while self.match_any(&[TokenKind::Minus, TokenKind::Plus]) {
      expr = binary!(self, expr, self.multiplication()?).unwrap();
    }

    Ok(expr)
  }

  fn multiplication(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.exponentiation()?;

    while self.match_any(&[TokenKind::Star, TokenKind::Percent, TokenKind::Slash]) {
      expr = binary!(self, expr, self.exponentiation()?).unwrap();
    }

    Ok(expr)
  }

  fn exponentiation(&mut self) -> ParseResult<Expr<'t>> {
    let mut expr = self.unary()?;

    while self.match_any(&[TokenKind::Power]) {
      expr = binary!(self, expr, self.exponentiation()?).unwrap();
    }

    Ok(expr)
  }

  fn unary(&mut self) -> ParseResult<Expr<'t>> {
    if self.match_any(&[
      TokenKind::Bang,
      TokenKind::Minus,
      TokenKind::BitNot,
      TokenKind::Try,
      TokenKind::TryBang,
      TokenKind::MinusMinus,
      TokenKind::PlusPlus,
    ]) {
      let op = self.previous.clone();
      return Ok(match self.previous.kind {
        TokenKind::Bang => unary!(self.unary()?, UnOpKind::Not, op),
        TokenKind::Minus => unary!(self.unary()?, UnOpKind::Negate, op),
        TokenKind::BitNot => unary!(self.unary()?, UnOpKind::BitNot, op),
        TokenKind::Try => try_expr!(prefix => self.unary()?, TryKind::Try, op),
        TokenKind::TryBang => try_expr!(prefix => self.unary()?, TryKind::TryUnwrap, op),
        TokenKind::MinusMinus | TokenKind::PlusPlus => todo!(),
        _ => unreachable!(),
      });
    }

    self.postfix()
  }

  fn postfix(&mut self) -> ParseResult<Expr<'t>> {
    let expr = self.primary()?;

    while self.match_any(&[
      TokenKind::LeftParen,
      TokenKind::Dot,
      TokenKind::QuestionDot,
      TokenKind::LeftBracket,
      TokenKind::PlusPlus,
      TokenKind::MinusMinus,
      TokenKind::Try,
      TokenKind::TryBang,
    ]) {
      todo!()
    }

    Ok(expr)
  }

  fn primary(&mut self) -> ParseResult<Expr<'t>> {
    if self.match_(&TokenKind::Identifier) {
      return Ok(Expr::new(
        ExprKind::Identifier(self.previous.clone()),
        self.previous.span.clone(),
      ));
    }
    if self.match_(&TokenKind::Null) {
      return Ok(literal!(self, LiteralValue::Null));
    }
    if self.match_(&TokenKind::True) {
      return Ok(literal!(self, LiteralValue::Bool(true)));
    }
    if self.match_(&TokenKind::False) {
      return Ok(literal!(self, LiteralValue::Bool(false)));
    }
    if self.match_(&TokenKind::IntLit(Disregard::disregard())) {
      let int = match self.previous.kind {
        TokenKind::IntLit(int) => int,
        _ => unreachable!(),
      };
      return Ok(literal!(self, LiteralValue::Int(int)));
    }
    if self.match_(&TokenKind::FloatLit(Disregard::disregard())) {
      let float = match self.previous.kind {
        TokenKind::FloatLit(FloatBits(float)) => float,
        _ => unreachable!(),
      };
      return Ok(literal!(self, LiteralValue::Float(float)));
    }
    if self.match_(&TokenKind::StringLit(Disregard::disregard())) {
      let fragments = match &self.previous.kind {
        TokenKind::StringLit(lit) => match lit {
          StringLiteral::Interpolated(fragments) => fragments,
          StringLiteral::Plain(s) => return Ok(literal!(self, LiteralValue::String(s.clone()))),
          StringLiteral::Invalid(error) => {
            return Err(MuError::new(
              "Invalid interpolated string literal",
              self.previous.span.clone(),
              mu_errors::ErrorKind::Lexer(match error {
                MalformedStringLiteral::MissingQuote => {
                  mu_errors::LexerErrorKind::UnterminatedString
                }
                MalformedStringLiteral::MissingBrace => {
                  mu_errors::LexerErrorKind::UnterminatedFragment
                }
              }),
            ))
          }
        },
        _ => unreachable!(),
      };

      unimplemented!("todo: interpolated strings {:?}", fragments)
    }

    if self.match_(&TokenKind::LeftBracket) {
      return self.array_literal();
    }

    if self.match_(&TokenKind::LeftParen) {
      return self.tuple_or_grouping();
    }

    if self.match_(&TokenKind::LeftBrace) {
      return self.record_literal();
    }

    if self.match_(&TokenKind::BackSlash) {
      return self.lambda_literal();
    }

    todo!("{:?} {:?}", self.previous, self.current);
  }

  fn type_expr(&mut self) -> ParseResult<TypeExpr<'t>> {
    self.primitive_type()
  }

  fn primitive_type(&mut self) -> ParseResult<TypeExpr<'t>> {
    let ty = if self.match_(&TokenKind::BoolType) {
      PrimitiveType::Bool
    } else if self.match_(&TokenKind::Null) {
      PrimitiveType::Null
    } else if self.match_(&TokenKind::IntType) {
      PrimitiveType::Int
    } else if self.match_(&TokenKind::FloatType) {
      PrimitiveType::Float
    } else if self.match_(&TokenKind::StringType) {
      PrimitiveType::String
    } else if self.match_(&TokenKind::Identifier) {
      let mut path = vec![self.previous.clone()];
      while self.match_(&TokenKind::Dot) {
        let name = self.consume(
          &TokenKind::Identifier,
          "Expected an identifier after the `.`",
        )?;
        path.push(name);
      }
      PrimitiveType::Named(path)
    } else {
      return Err(MuError::new(
        "Expected `bool`, `int`, `float`, `string`, `null`, or a type name",
        self.current.span.clone(),
        mu_errors::ErrorKind::Parser(mu_errors::ParserErrorKind::InvalidPrimitiveType),
      ));
    };

    Ok(TypeExpr::Primitive(Box::new(ty)))
  }

  fn param_list(&mut self) -> ParseResult<ParameterList<'t>> {
    let mut params = Vec::new();

    if !self.check(&TokenKind::RightParen) {
      params.push(self.param()?);

      while self.match_(&TokenKind::Comma) {
        if self.check(&TokenKind::RightParen) {
          break;
        }
        params.push(self.param()?);
      }
    }

    self.consume(
      &TokenKind::RightParen,
      "Expected a `)` after the parameter list.",
    )?;

    Ok(ParameterList(params))
  }

  fn param(&mut self) -> ParseResult<Param<'t>> {
    let name = self.consume(&TokenKind::Identifier, "Expected a parameter name")?;
    let ty = if self.match_(&TokenKind::Colon) {
      Some(self.type_expr()?)
    } else {
      None
    };
    let value = if self.match_(&TokenKind::Equal) {
      Some(self.expr()?)
    } else {
      None
    };
    Ok(Param { name, ty, value })
  }

  fn lambda_literal(&mut self) -> ParseResult<Expr<'t>> {
    let start = self.current.span.start;
    let params = if self.match_(&TokenKind::LeftParen) {
      let params = self.param_list()?;
      LambdaParams::List(params)
    } else {
      let param = self.consume(
        &TokenKind::Identifier,
        "Expected either a parameter name or parameter list after the `\\`.",
      )?;
      LambdaParams::Single(param)
    };

    self.consume(
      &TokenKind::LeftBrace,
      "Expected a `{` before the lambda body",
    )?;
    let body = self.block_stmt()?;
    let end = body.span.end;

    let lambda = LambdaLiteral { params, body };
    Ok(Expr::new(
      ExprKind::LambdaLiteral(Box::new(lambda)),
      start..end,
    ))
  }

  fn record_literal(&mut self) -> ParseResult<Expr<'t>> {
    let start = self.previous.span.start;
    let mut fields = Vec::new();

    while !self.check(&TokenKind::RightBrace) {
      let start = self.current.span.start;

      if self.match_(&TokenKind::Spread) {
        let spread = self.previous.clone();
        let field = self.consume(
          &TokenKind::Identifier,
          "Expected a variable name after the `...`.",
        )?;
        fields.push(RecordField::Spread { spread, field })
      } else if self.match_(&TokenKind::LeftBracket) {
        let field = self.expr()?;
        self.consume(
          &TokenKind::RightBracket,
          "Expected a `]` after the computed field name.",
        )?;
        self.consume(&TokenKind::Colon, "Expected a `:` after the field name.")?;
        let value = self.expr()?;
        fields.push(RecordField::Computed { field, value });
      } else if self.match_(&TokenKind::Identifier) {
        let field = self.previous.clone();
        if self.match_(&TokenKind::Colon) {
          let value = self.expr()?;
          fields.push(RecordField::Named { field, value })
        } else {
          fields.push(RecordField::Variable { field })
        }
      } else {
        let end = self.advance().span.end;
        self.errors.record(MuError::parse(
          "Expected a field, computed field, or spread expression.",
          start..end,
        ));
        continue;
      }

      let had_comma = self.match_(&TokenKind::Comma);
      if !had_comma && !self.check(&TokenKind::RightBrace) {
        let end = self.previous.span.end;
        return Err(MuError::parse(
          "Expected a `,` after a record field",
          start..end,
        ));
      }
    }

    let end = self
      .consume(
        &TokenKind::RightBrace,
        "Expected a `}` at the end of a record literal.",
      )?
      .span
      .end;

    Ok(Expr::new(
      ExprKind::RecordLiteral(RecordLiteral(fields)),
      start..end,
    ))
  }

  fn tuple_or_grouping(&mut self) -> ParseResult<Expr<'t>> {
    let start = self.previous.span.start;
    if self.match_(&TokenKind::RightParen) {
      let end = self.previous.span.end;
      return Ok(Expr::new(ExprKind::Tuple(Tuple::Unit), start..end));
    }
    let expr = self.expr()?;
    if self.match_(&TokenKind::RightParen) {
      let end = self.previous.span.end;
      return Ok(Expr::new(ExprKind::Grouping(Box::new(expr)), start..end));
    }
    self.consume(
      &TokenKind::Comma,
      "Expected a `,` after the first tuple element.",
    )?;
    let mut elements = vec![expr];
    while !self.check(&TokenKind::RightParen) {
      elements.push(self.expr()?);
      if !self.check(&TokenKind::RightParen) {
        self.consume(&TokenKind::Comma, "Expected a `,` after a tuple element")?;
      }
    }
    self.consume(
      &TokenKind::RightParen,
      "Expected a `)` after the tuple elements",
    )?;
    let end = self.previous.span.end;
    Ok(Expr::new(
      ExprKind::Tuple(Tuple::Tuple(elements)),
      start..end,
    ))
  }

  fn array_literal(&mut self) -> ParseResult<Expr<'t>> {
    let start_span = self.previous.span.clone();
    let mut items = vec![];
    if !self.check(&TokenKind::RightBracket) {
      let mut first_spread = None;
      if self.match_(&TokenKind::Spread) {
        first_spread = Some(self.previous.clone());
      }
      let first_item = self.expr()?;

      if first_spread.is_none() && self.match_(&TokenKind::Semicolon) {
        let length = self.expr()?;
        let literal = ArrayLiteral::Initialized(Box::new(InitializedArray {
          value: first_item,
          length,
        }));
        self.consume(
          &TokenKind::RightBracket,
          "Expected a `]` after the array length",
        )?;

        let end_span = self.previous.span.start;
        return Ok(Expr::new(
          ExprKind::ArrayLiteral(literal),
          start_span.start..end_span,
        ));
      }

      items.push(ArrayItem {
        spread: first_spread,
        value: first_item,
      });
      while self.match_(&TokenKind::Comma) {
        if self.check(&TokenKind::RightBracket) {
          break;
        }
        let spread = if self.match_(&TokenKind::Spread) {
          Some(self.previous.clone())
        } else {
          None
        };
        items.push(ArrayItem {
          spread,
          value: self.expr()?,
        });
      }
    }
    self.consume(
      &TokenKind::RightBracket,
      "Expected a `]` at the end of an array literal",
    )?;
    let literal = ArrayLiteral::Plain(items);
    let end_span = self.previous.span.start;

    Ok(Expr::new(
      ExprKind::ArrayLiteral(literal),
      start_span.start..end_span,
    ))
  }

  fn skip_semi(&mut self) -> bool {
    self.match_(&TokenKind::Semicolon)
  }

  fn skip_newline(&mut self) -> bool {
    self.match_(&TokenKind::LineEnd)
  }

  fn block_stmt(&mut self) -> ParseResult<Stmt<'t>> {
    let span_start = self.previous.span.start;
    let body = self.block()?;
    let span_end = self.previous.span.end;
    Ok(Stmt::new(StmtKind::Block(body), span_start..span_end))
  }

  fn block(&mut self) -> ParseResult<Vec<Stmt<'t>>> {
    let mut body = Vec::new();

    // if the next token is a RightBrace, the block is empty
    if !self.check(&TokenKind::RightBrace) {
      while !self.at_end() && !self.check(&TokenKind::RightBrace) {
        match self.stmt() {
          Ok(stmt) => body.push(stmt),
          Err(e) => {
            self.errors.record(e);
            self.synchronize();
          }
        }
      }
    }

    self.consume(&TokenKind::RightBrace, "Expected a '}' after a block")?;
    Ok(body)
  }

  #[inline]
  fn consume(&mut self, kind: &TokenKind<'t>, err_msg: impl ErrorMsg) -> ParseResult<Token<'t>> {
    if self.check(kind) {
      Ok(self.advance())
    } else {
      Err(MuError::parse(err_msg, self.current.span.clone()))
    }
  }

  #[inline]
  fn consume_any(
    &mut self,
    kinds: &[TokenKind<'t>],
    err_msg: impl ErrorMsg,
  ) -> ParseResult<Token<'t>> {
    for kind in kinds {
      if self.check(kind) {
        return Ok(self.advance());
      }
    }
    Err(MuError::parse(err_msg, self.current.span.clone()))
  }

  #[inline]
  fn match_(&mut self, kind: &TokenKind<'t>) -> bool {
    if self.check(kind) {
      self.advance();
      true
    } else {
      false
    }
  }

  #[inline]
  fn match_any(&mut self, kinds: &[TokenKind<'t>]) -> bool {
    for kind in kinds {
      if self.check(kind) {
        self.advance();
        return true;
      }
    }
    false
  }

  #[inline(always)]
  fn check(&mut self, kind: &TokenKind<'t>) -> bool {
    std::mem::discriminant(kind) == std::mem::discriminant(&self.current.kind)
  }

  #[inline(always)]
  fn check_any(&mut self, kinds: &[TokenKind<'t>]) -> bool {
    for kind in kinds {
      if std::mem::discriminant(kind) == std::mem::discriminant(&self.current.kind) {
        return true;
      }
    }
    false
  }

  /// In case of an error, consume tokens until we reach one
  /// which has a high chance of beginning a new valid segment
  /// of the source code
  fn synchronize(&mut self) {
    self.advance();
    while !self.at_end()
      && [TokenKind::Semicolon, TokenKind::LineEnd].contains(&self.previous.kind)
      && !([
        TokenKind::Fn,
        TokenKind::Loop,
        TokenKind::For,
        TokenKind::While,
        TokenKind::If,
        TokenKind::Return,
        TokenKind::Type,
        TokenKind::Class,
        TokenKind::LineEnd,
        TokenKind::Semicolon,
        TokenKind::LeftBracket,
        TokenKind::Export,
        TokenKind::Import,
      ]
      .contains(&self.current.kind))
    {
      self.advance();
    }
  }

  /// Move to the next token
  #[inline]
  fn advance(&mut self) -> Token<'t> {
    std::mem::swap(&mut self.previous, &mut self.current);
    self.current = self.lexer.next_token().unwrap_or_else(|| self.eof.clone());
    if self.previous.kind == TokenKind::Invalid {
      self.errors.record(MuError::lex(
        format!("Unexpected character sequence `{}`", self.previous.lexeme),
        self.previous.span.clone(),
      ));
      self.advance();
    }
    self.previous.clone()
  }

  /// Check if the parser has reached EOF
  #[inline(always)]
  fn at_end(&self) -> bool {
    self.current.kind == TokenKind::EOF
  }
}

/// Indicates that the value isn't important, doesn't affect the algorithm, or is unused.
trait Disregard {
  fn disregard() -> Self;
}

impl Disregard for FloatBits {
  fn disregard() -> Self {
    FloatBits(0.0)
  }
}

impl Disregard for i64 {
  fn disregard() -> Self {
    0
  }
}

impl<'t> Disregard for StringLiteral<'t> {
  fn disregard() -> Self {
    StringLiteral::Invalid(MalformedStringLiteral::MissingQuote)
  }
}

#[cfg(test)]
#[mu_testing::mu_test_suite]
mod tests {
  use mu_ast::ast2str::AstToStr;

  use super::*;

  #[mu_tests = "tests"]
  mod parser {
    #[ok_callback]
    use super::test_parse as parse_ok;
    #[err_callback]
    use super::test_parse as parse_err;
  }

  fn test_parse(source: std::borrow::Cow<'_, str>) -> std::result::Result<String, String> {
    match parse(&source) {
      Ok(ast) => Ok(ast.ast_to_str_impl(&mu_ast::ast2str::TestSymbols)),
      Err(errors) => Err(format!("{:#?}", errors.errors)),
    }
  }
}
