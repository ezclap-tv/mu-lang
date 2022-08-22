use std::borrow::Cow;

pub type Span = core::ops::Range<usize>;

pub trait ErrorMsg {
  fn msg(self) -> Cow<'static, str>;
}

impl<T> ErrorMsg for T
where
  T: Into<Cow<'static, str>>,
{
  fn msg(self) -> Cow<'static, str> {
    self.into()
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerErrorKind {
  Generic,
  UnterminatedString,
  UnterminatedFragment,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserErrorKind {
  Generic,
  InvalidPrimitiveType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorKind {
  Lexer(LexerErrorKind),
  Parser(ParserErrorKind),
}

#[derive(Debug, Clone)]
pub struct Error {
  pub message: Cow<'static, str>,
  pub kind: ErrorKind,
  pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ErrorSink<'t> {
  pub source: Cow<'t, str>,
  pub errors: Vec<Error>,
}

impl<'t> ErrorSink<'t> {
  pub fn record(&mut self, e: Error) {
    self.errors.push(e);
  }
}

impl<'t> ErrorSink<'t> {
  pub fn new<S: Into<Cow<'t, str>>>(source: S) -> Self {
    Self {
      source: source.into(),
      errors: Vec::new(),
    }
  }
}

impl Error {
  pub fn new(message: impl ErrorMsg, span: Span, kind: ErrorKind) -> Self {
    Self {
      message: message.msg(),
      kind,
      span,
    }
  }

  pub fn lex(message: impl ErrorMsg, span: Span) -> Self {
    Self::new(message, span, ErrorKind::Lexer(LexerErrorKind::Generic))
  }

  pub fn parse(message: impl ErrorMsg, span: Span) -> Self {
    Self::new(message, span, ErrorKind::Parser(ParserErrorKind::Generic))
  }
}
