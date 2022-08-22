use std::fmt;

use logos::Span;

use crate::lexer::{Token, TokenKind};

#[derive(Clone, Debug)]
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
    inner: std::num::ParseIntError,
  },
  UnexpectedToken {
    token: Token<'static>,
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
    format!("{: >start$}", "^", start = pos + 1)
  }
  fn get_relevant_span(error: &Error) -> Span {
    match error {
      Error::MissingToken { found, .. } => found.span.clone(),
      Error::MissingOneOf { found, .. } => found.span.clone(),
      Error::InvalidNumber { token, .. } => token.span.clone(),
      Error::UnexpectedToken { token } => token.span.clone(),
    }
  }
  for error in errors {
    let span = get_relevant_span(error);
    let line_span = find_line_from_span(source, span.clone());
    let line = &source[line_span.clone()];
    let highlight = get_highlight(span.start - line_span.start);
    write!(to, "\n{error}\n{line}\n{highlight}").unwrap();
  }
}
