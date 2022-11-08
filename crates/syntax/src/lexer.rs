//! This module contains the implementation of Mu's lexer.
//! The lexer performs [lexical analysis](https://en.wikipedia.org/wiki/Lexical_analysis),
//! transforming the source code into a stream of [`Token`].
//! The implementation is generated from the [`TokenKind`] enum using [Logos](https://github.com/maciejhirsz/logos).
//!
//! The entrypoints for this module are:
//! - [`Lexer`]
//! - [`Token`]

use std::borrow::Cow;
use std::mem::discriminant;

use logos::{FilterResult, Logos};
use serde::{Deserialize, Serialize};

use crate::span::Span;

pub struct Lexer<'a> {
  inner: logos::Lexer<'a, TokenKind>,
  previous: Token<'a>,
  current: Token<'a>,
  eof: Token<'a>,
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str) -> Self {
    let end = source.len().saturating_sub(1);
    let eof = Token {
      lexeme: "".into(),
      span: (end..end).into(),
      kind: TokenKind::Eof,
    };

    Self {
      inner: TokenKind::lexer(source),
      previous: eof.clone(),
      current: eof.clone(),
      eof,
    }
  }

  #[inline]
  pub fn previous(&self) -> &Token<'a> {
    &self.previous
  }

  #[inline]
  pub fn current(&self) -> &Token<'a> {
    &self.current
  }

  #[inline]
  pub fn eof(&self) -> &Token<'a> {
    &self.eof
  }

  #[inline]
  pub fn bump(&mut self) -> &Token<'a> {
    std::mem::swap(&mut self.previous, &mut self.current);
    self.current = match self.inner.next() {
      Some(kind) => Token {
        lexeme: self.inner.slice().into(),
        span: self.inner.span().into(),
        kind,
      },
      None => self.eof.clone(),
    };
    &self.previous
  }
}

pub struct Tokens<'a>(Lexer<'a>);

impl<'a> Iterator for Tokens<'a> {
  type Item = Token<'a>;

  fn next(&mut self) -> Option<Self::Item> {
    self.0.bump();
    let token = self.0.current().clone();
    if !token.is(TokenKind::Eof) {
      Some(token)
    } else {
      None
    }
  }
}

impl<'a> IntoIterator for Lexer<'a> {
  type Item = Token<'a>;

  type IntoIter = Tokens<'a>;

  fn into_iter(self) -> Self::IntoIter {
    Tokens(self)
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Token<'a> {
  pub lexeme: Cow<'a, str>,
  pub span: Span,
  pub kind: TokenKind,
}

impl<'a> Token<'a> {
  #[inline]
  pub fn is(&self, other: TokenKind) -> bool {
    discriminant(&self.kind) == discriminant(&other)
  }

  #[inline]
  pub fn is_any(&self, other: &[TokenKind]) -> bool {
    let d = discriminant(&self.kind);
    other.iter().any(|v| d == discriminant(v))
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Logos)]
pub enum TokenKind {
  // keywords
  #[token("use")]
  Use,
  #[token("as")]
  As,
  #[token("let")]
  Let,
  #[token("pub")]
  Pub,
  #[token("fn")]
  Fn,
  #[token("where")]
  Where,
  #[token("type")]
  Type,
  #[token("class")]
  Class,
  #[token("trait")]
  Trait,
  #[token("impl")]
  Impl,
  #[token("for")]
  For,
  #[token("in")]
  In,
  #[token("while")]
  While,
  #[token("loop")]
  Loop,
  #[token("return")]
  Return,
  #[token("break")]
  Break,
  #[token("continue")]
  Continue,
  #[token("throw")]
  Throw,
  #[token("if")]
  If,
  #[token("else")]
  Else,
  #[token("try")]
  Try,
  #[token("catch")]
  Catch,
  #[token("throws")]
  Throws,
  #[token("spawn")]
  Spawn,

  // Symbols
  #[token(";")]
  Semicolon,
  #[token(".")]
  Dot,
  #[token("{")]
  LeftBrace,
  #[token("}")]
  RightBrace,
  #[token("(")]
  LeftParen,
  #[token(")")]
  RightParen,
  #[token("[")]
  LeftBracket,
  #[token("]")]
  RightBracket,
  #[token(":")]
  Colon,
  #[token("=")]
  Equal,
  #[token("->")]
  ThinArrow,
  #[token("=>")]
  FatArrow,
  #[token("..")]
  RangeEx,
  #[token("..=")]
  RangeInc,
  #[token("||")]
  Or2,
  #[token("&&")]
  And2,
  #[token("==")]
  Equal2,
  #[token(">")]
  More,
  #[token(">=")]
  MoreEqual,
  #[token("<")]
  Less,
  #[token("<=")]
  LessEqual,
  #[token("+")]
  Plus,
  #[token("+=")]
  PlusEqual,
  #[token("-")]
  Minus,
  #[token("-=")]
  MinusEqual,
  #[token("/")]
  Slash,
  #[token("/=")]
  SlashEqual,
  #[token("%")]
  Percent,
  #[token("%=")]
  PercentEqual,
  #[token("*")]
  Star,
  #[token("*=")]
  StarEqual,
  #[token("**")]
  Star2,
  #[token("**=")]
  Star2Equal,
  #[token("!")]
  Exclamation,
  #[token("!=")]
  BangEqual,
  #[token("?")]
  Question,
  #[token("??")]
  Question2,
  #[token("??=")]
  Question2Equal,
  #[token("\\")]
  Backslash,

  // Literals
  #[token("true", |_| true)]
  #[token("false", |_| false)]
  Bool(bool),
  #[token("null")]
  Null,
  #[regex("0b[01]([01_]*[01])?")]
  #[regex("[0-9]([0-9_]*[0-9])?", priority = 2)]
  #[regex("0x[0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?")]
  Int,
  #[token("inf")]
  #[regex(r"[0-9]+(\.[0-9]*)?([Ee][+-]?[0-9]+)?")]
  Float,
  #[token("\"", lex_string)]
  String(StringKind),

  #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
  Ident,

  #[regex("[ \t\n\r]+", logos::skip)]
  Whitespace,
  #[regex("//[^\n]*", logos::skip)]
  Comment,
  #[regex("/\\*", lex_multi_line_comment)]
  MultiLineComment,
  #[error]
  Error,

  Eof,
}

fn lex_multi_line_comment(lex: &mut logos::Lexer<'_, TokenKind>) -> FilterResult<()> {
  // how many characters we went through
  let mut n = 0;
  // Mitigate DOS attacks on the lexer with many unclosed comments:
  //
  // /*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
  //
  // Without this step, the lexer would re-attempt parsing until EOF from every
  // occurrence of /*, leading to O(N^2) worst case performance.
  let mut n_at_last_seen_opening = 0;

  // how many multi-line comment opening tokens we found
  // this starts at one, because the initial /* is already consumed
  let mut opening_count = 1;
  let mut previous_two = [b'*', b'\0'];

  for ch in lex.remainder().bytes() {
    n += 1;
    previous_two = [previous_two[1], ch];

    match previous_two {
      [b'/', b'*'] => {
        opening_count += 1;
        n_at_last_seen_opening = n
      }
      [b'*', b'/'] => opening_count -= 1,
      _ => {
        continue;
      }
    }

    if opening_count == 0 {
      break;
    }

    // Set the last byte to /0, so comments like /*/**/*/ get parsed correctly
    previous_two[1] = b'\0';
  }

  if opening_count == 0 {
    lex.bump(n);
    FilterResult::Skip
  } else {
    lex.bump(n_at_last_seen_opening);
    FilterResult::Error
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum StringKind {
  // This string does not contain any interpolations
  Plain,
  // This string has to be further processed by the parser
  Fmt,
}

fn lex_string(lex: &mut logos::Lexer<'_, TokenKind>) -> Option<StringKind> {
  #[derive(Clone, Copy, PartialEq, Eq)]
  enum State {
    // Lexer is in a string fragment
    String(usize),
    // Lexer is in an expr fragment
    Expr(usize),
  }

  let mut state = State::String(0);
  let mut kind = StringKind::Plain;

  let mut count = 0;
  let mut previous = b'"';
  let mut result = None;
  for current in lex.remainder().bytes() {
    count += 1;

    if previous != b'\\' {
      match (state, current) {
        // Close string (root)
        (State::String(0), b'"') => {
          result = Some(kind);
          break;
        }
        // Close string
        (State::String(n), b'"') => state = State::Expr(n - 1),
        // Open expr
        (State::String(n), b'{') => {
          kind = StringKind::Fmt;
          state = State::Expr(n + 1);
        }

        // Open string
        (State::Expr(n), b'"') => state = State::String(n + 1),
        // Close expr
        (State::Expr(n), b'}') => state = State::String(n - 1),
        // Error: Opening expr within another expr
        (State::Expr(_), b'{') => {
          // Consume bytes until we find an unescaped quote
          let mut previous = b'{';
          for current in lex.remainder()[count..].bytes() {
            count += 1;
            if previous != b'\\' && current == b'"' {
              break;
            }
            previous = current;
          }

          result = None;
          break;
        }

        _ => {}
      }
    }

    previous = current;
  }

  lex.bump(count);

  result
}

impl std::fmt::Display for TokenKind {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let str = match self {
      TokenKind::Use => "use",
      TokenKind::As => "as",
      TokenKind::Let => "let",
      TokenKind::Pub => "pub",
      TokenKind::Fn => "fn",
      TokenKind::Where => "where",
      TokenKind::Type => "type",
      TokenKind::Class => "class",
      TokenKind::Trait => "trait",
      TokenKind::Impl => "impl",
      TokenKind::For => "for",
      TokenKind::In => "in",
      TokenKind::While => "while",
      TokenKind::Loop => "loop",
      TokenKind::Return => "return",
      TokenKind::Break => "break",
      TokenKind::Continue => "continue",
      TokenKind::Throw => "throw",
      TokenKind::If => "if",
      TokenKind::Else => "else",
      TokenKind::Try => "try",
      TokenKind::Catch => "catch",
      TokenKind::Throws => "throws",
      TokenKind::Spawn => "spawn",
      TokenKind::Semicolon => ";",
      TokenKind::Dot => ".",
      TokenKind::LeftBrace => "{",
      TokenKind::RightBrace => "}",
      TokenKind::LeftParen => "(",
      TokenKind::RightParen => ")",
      TokenKind::LeftBracket => "[",
      TokenKind::RightBracket => "]",
      TokenKind::Colon => ":",
      TokenKind::Equal => "=",
      TokenKind::ThinArrow => "->",
      TokenKind::FatArrow => "=>",
      TokenKind::RangeEx => "..",
      TokenKind::RangeInc => "..=",
      TokenKind::Or2 => "||",
      TokenKind::And2 => "&&",
      TokenKind::Equal2 => "==",
      TokenKind::More => ">",
      TokenKind::MoreEqual => ">=",
      TokenKind::Less => "<",
      TokenKind::LessEqual => "<=",
      TokenKind::Plus => "+",
      TokenKind::PlusEqual => "+=",
      TokenKind::Minus => "-",
      TokenKind::MinusEqual => "-=",
      TokenKind::Slash => "/",
      TokenKind::SlashEqual => "/=",
      TokenKind::Percent => "%",
      TokenKind::PercentEqual => "%=",
      TokenKind::Star => "*",
      TokenKind::StarEqual => "*=",
      TokenKind::Star2 => "**",
      TokenKind::Star2Equal => "**=",
      TokenKind::Exclamation => "!",
      TokenKind::BangEqual => "!=",
      TokenKind::Question => "?",
      TokenKind::Question2 => "??",
      TokenKind::Question2Equal => "??=",
      TokenKind::Backslash => "\\",
      TokenKind::Ident => "identifier",
      TokenKind::Bool(_)
      | TokenKind::Null
      | TokenKind::Int
      | TokenKind::Float
      | TokenKind::String(_) => "literal",
      // deliberately `unknown` because they're probably never used with `parser.expect`
      TokenKind::Whitespace
      | TokenKind::Comment
      | TokenKind::MultiLineComment
      | TokenKind::Error
      | TokenKind::Eof => "unknown",
    };
    write!(f, "{str}")
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn compare(src: &str, tokens: &[(TokenKind, &str)]) {
    let mut diff = vec![];
    let actual = Lexer::new(src).into_iter().collect::<Vec<_>>();
    assert_eq!(tokens.len(), actual.len());

    for (actual, expected) in actual.iter().zip(tokens.iter()) {
      if (actual.kind, actual.lexeme.as_ref()) != (expected.0, expected.1) {
        diff.push((actual, expected));
      }
    }

    if !diff.is_empty() {
      eprintln!("Found differences:");
      for (actual, expected) in diff {
        eprintln!(
          "{:?} {} != {:?} {}",
          actual.kind, actual.lexeme, expected.0, expected.1
        );
      }
      panic!();
    }
  }

  #[test]
  fn test_lex_comments() {
    use TokenKind::*;

    let src = r#"
      test // test
      test /* test */
      test /* /* /* /* /* /* /* test */ */ */ */ */ */ */
    "#;
    let tokens = &[(Ident, "test"), (Ident, "test"), (Ident, "test")];

    compare(src, tokens)
  }

  #[test]
  fn test_lex_numbers() {
    use TokenKind::*;

    let src = r#"123
      123_321
      1_2_3_4_5
      999_999_999
      999_999_999_999_999
      999_999_999_999_999_999
      
      0xABCDEF
      0xA_B_C_D_E_F
      0xDEAD_CAFE
      0x1_F331_D0D
      
      0b1111_0000_0101_1010_1100_0011
      0b000000
      0b111111
      
      1.0
      1.0e1
      1.0e+1
      1.0e-1
      1e1
      1e+1
      1e-1
    "#;
    let tokens = &[
      (Int, "123"),
      (Int, "123_321"),
      (Int, "1_2_3_4_5"),
      (Int, "999_999_999"),
      (Int, "999_999_999_999_999"),
      (Int, "999_999_999_999_999_999"),
      (Int, "0xABCDEF"),
      (Int, "0xA_B_C_D_E_F"),
      (Int, "0xDEAD_CAFE"),
      (Int, "0x1_F331_D0D"),
      (Int, "0b1111_0000_0101_1010_1100_0011"),
      (Int, "0b000000"),
      (Int, "0b111111"),
      (Float, "1.0"),
      (Float, "1.0e1"),
      (Float, "1.0e+1"),
      (Float, "1.0e-1"),
      (Float, "1e1"),
      (Float, "1e+1"),
      (Float, "1e-1"),
    ];

    compare(src, tokens);
  }

  #[test]
  fn test_lex_strings() {
    use StringKind::*;
    use TokenKind::*;

    let src = r#"
      "a"
      "{a}"
      "\{a}"
      "{"a"}"
      "\{"a"}"
      "{{}"
      "{{}\""
      "{{"
      "{{\""
      "a"
    "#;
    let tokens = &[
      (String(Plain), r#""a""#),
      (String(Fmt), r#""{a}""#),
      (String(Plain), r#""\{a}""#),
      (String(Fmt), r#""{"a"}""#),
      (String(Plain), r#""\{""#),
      (Ident, r#"a"#),
      (String(Plain), r#""}""#),
      (Error, r#""{{}""#),
      (Error, r#""{{}\"""#),
      (Error, r#""{{""#),
      (Error, r#""{{\"""#),
      (String(Plain), r#""a""#),
    ];

    compare(src, tokens);
  }
}
