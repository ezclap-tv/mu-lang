use std::fmt;

use serde::{Deserialize, Serialize};

use crate::lexer::{Token, TokenKind};
use crate::span::Span;

// TODO: proper diagnostics

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Error {
  MissingToken {
    expected: TokenKind,
    found: Token<'static>,
  },
  MissingOneOf {
    expected: Vec<TokenKind>,
    found: Token<'static>,
  },
  InvalidNumber {
    token: Token<'static>,
    inner: String,
  },
  UnexpectedToken {
    token: Token<'static>,
  },
  NotCallable {
    span: Span,
  },
  VarNotDefined {
    token: Token<'static>,
  },
  TypeNotDefined {
    token: Token<'static>,
  },
  NotRecord {
    span: Span,
  },
  NotField {
    span: Span,
    field: Token<'static>,
  },
  TypeMismatch {
    lhs: Span,
    rhs: String,
  },
}

impl fmt::Display for Error {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      Error::MissingToken { expected, found } => {
        write!(f, "expected {expected:?}, found {:?}", found.kind)
      }
      Error::MissingOneOf { expected, found } => write!(
        f,
        "found {:?}, but expected one of: {:?}",
        found.kind,
        FormatList(expected)
      ),
      Error::InvalidNumber { token, inner } => {
        write!(
          f,
          "failed to parse `{}` as an integer: {}",
          token.lexeme, inner
        )
      }
      Error::UnexpectedToken { token } => {
        write!(f, "unexpected token: {:?}", token.kind)
      }
      Error::NotCallable { .. } => {
        write!(f, "expression is not callable")
      }
      Error::VarNotDefined { token } => {
        write!(f, "variable `{}` is not defined", token.lexeme)
      }
      Error::TypeNotDefined { token } => {
        write!(f, "type `{}` is not defined", token.lexeme)
      }
      Error::NotRecord { .. } => {
        write!(f, "expression does not evaluate to a record")
      }
      Error::NotField { field, .. } => {
        write!(f, "field has no field `{}`", field.lexeme)
      }
      Error::TypeMismatch { rhs, .. } => {
        write!(f, "expected type {}", rhs)
      }
    }
  }
}

impl std::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

struct FormatList<'a, T>(&'a Vec<T>);

impl<'a, T: fmt::Display> fmt::Display for FormatList<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      self
        .0
        .iter()
        .map(|e| format!("`{e}`"))
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

impl<'a, T: fmt::Debug> fmt::Debug for FormatList<'a, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{}",
      self
        .0
        .iter()
        .map(|e| format!("`{e:?}`"))
        .collect::<Vec<_>>()
        .join(", ")
    )
  }
}

pub fn report(source: &str, errors: &Vec<Error>, mut to: impl std::fmt::Write) {
  fn find_line_from_span(source: &str, span: Span) -> Span {
    let start = source[0..span.start].rfind('\n').unwrap_or(0);
    let end = source[span.end..]
      .find('\n')
      .map(|v| span.end + v)
      .unwrap_or(source.len() - 1);
    Span { start, end }
  }
  fn get_highlight(pos: usize) -> String {
    format!("{: >pos$}", "^")
  }
  fn get_relevant_span(error: &Error) -> Span {
    match error {
      Error::MissingToken { found, .. } => found.span,
      Error::MissingOneOf { found, .. } => found.span,
      Error::InvalidNumber { token, .. } => token.span,
      Error::UnexpectedToken { token } => token.span,
      Error::NotCallable { span } => *span,
      Error::VarNotDefined { token } => token.span,
      Error::TypeNotDefined { token } => token.span,
      Error::NotRecord { span } => *span,
      Error::NotField { span, .. } => *span,
      Error::TypeMismatch { lhs, .. } => *lhs,
    }
  }
  for error in errors {
    let span = get_relevant_span(error);
    let line_span = find_line_from_span(source, span);
    let line = source[line_span.start..line_span.end].trim();
    let highlight = get_highlight(span.start - line_span.start);
    write!(to, "{error}:\n|  {line}\n|  {highlight}\n").unwrap();
  }
}
