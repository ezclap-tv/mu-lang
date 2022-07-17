use std::borrow::Cow;

use logos::Logos;

use crate::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StringLiteral<'a> {
  Plain(Cow<'a, str>),
  Interpolated(Vec<StringFragment<'a>>),
  Invalid(MalformedStringLiteral),
}

impl<'a> StringLiteral<'a> {
  #[inline]
  pub fn error(&self) -> Option<&'static str> {
    match self {
      Self::Invalid(MalformedStringLiteral::MissingBrace) => {
        Some("One of this interpolated string's fragments is missing a closing brace")
      }
      Self::Invalid(MalformedStringLiteral::MissingQuote) => {
        Some("This interpolated string is missing a closing quote")
      }
      _ => None,
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MalformedStringLiteral {
  MissingQuote,
  MissingBrace,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum InterpolatedStringState {
  Closed,
  MissingQuote,
  MissingBrace,
}
impl Default for InterpolatedStringState {
  fn default() -> Self {
    InterpolatedStringState::Closed
  }
}

#[derive(Clone)]
pub enum StringFragment<'src> {
  Text(Token<'src>),
  Expr(logos::Lexer<'src, TokenKind<'src>>),
}

impl<'a> std::hash::Hash for StringFragment<'a> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    match self {
      StringFragment::Text(t) => t.hash(state),
      StringFragment::Expr(l) => {
        l.source().hash(state);
        l.span().hash(state);
      }
    }
  }
}

impl<'a> std::fmt::Debug for StringFragment<'a> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(
      f,
      "{}",
      match self {
        Self::Text(tok) =>
          if f.alternate() {
            format!("StringFragment::{:#?}", tok)
          } else {
            format!("StringFragment::{:?}", tok)
          },
        Self::Expr(_) => "StringFragment::Expr(*)".to_owned(),
      }
    )
  }
}

impl<'a> PartialEq for StringFragment<'a> {
  fn eq(&self, other: &StringFragment<'a>) -> bool {
    match (self, other) {
      (Self::Text(l), Self::Text(r)) => l.lexeme == r.lexeme,
      (left, right) => std::ptr::eq(left, right),
    }
  }
}
impl<'a> Eq for StringFragment<'a> {}

pub fn lex_string<'a>(lex: &mut logos::Lexer<'a, TokenKind<'a>>) -> StringLiteral<'a> {
  let quote_kind = lex.slice().chars().next().unwrap() as u8;
  let mut fragments = vec![];

  let remainder = lex.remainder();
  let bytes = remainder.as_bytes();
  let mut previous_fragment_end = 0;
  let mut state = InterpolatedStringState::MissingQuote;
  let mut bump_count = 0;

  let mut i = 0;
  while i < bytes.len() {
    // we just parsed a fragment and need to catch up to where it stopped parsing
    if i < previous_fragment_end {
      i = previous_fragment_end;
    }

    let byte = bytes[i];
    bump_count = i;
    match byte {
      c if c == quote_kind && bytes.get(i.saturating_sub(1)) != Some(&b'\\') => {
        if previous_fragment_end < i {
          let span = previous_fragment_end..i;
          let remainder = &remainder[span.clone()];
          if !remainder.is_empty() {
            fragments.push(StringFragment::Text(Token::new(
              remainder,
              span,
              TokenKind::StringLit(StringLiteral::Plain(Cow::from(remainder))),
            )));
          }
        }
        state = InterpolatedStringState::Closed;
        break;
      }
      b'{' if bytes.get(i.saturating_sub(1)) != Some(&b'\\') => {
        let span = if previous_fragment_end != 0 {
          previous_fragment_end..i
        } else {
          0..i
        };

        let lexeme = &remainder[span.clone()];
        if !lexeme.is_empty() {
          fragments.push(StringFragment::Text(Token::new(
            lexeme,
            span,
            TokenKind::StringLit(StringLiteral::Plain(Cow::from(lexeme))),
          )));
        }

        let mut unclosed_braces = 1;
        let mut quotes = 0;
        // + 1 to skip opening brace
        let fragment_start = i + 1;
        let fragment_bytes = remainder[fragment_start..].as_bytes();
        let mut fragment_end = 0;
        for (fi, fbyte) in fragment_bytes.iter().copied().enumerate() {
          let fprev = fragment_bytes.get(fi.saturating_sub(1));
          fragment_end = fragment_start + fi;

          // FIXME: this will break for nested opening curlies like in "{ "{" }", because it doesn't respect strings
          match fbyte {
            b'"' if fprev != Some(&b'\\') => quotes += 1,
            b'{' if quotes % 2 == 0 => unclosed_braces += 1,
            b'}' if quotes % 2 == 0 => {
              unclosed_braces -= 1;
              if unclosed_braces == 0 {
                break;
              }
            }
            _ => (),
          }
        }

        if quotes % 2 != 0 {
          bump_count = fragment_end;
          state = InterpolatedStringState::MissingQuote;
          break;
        }

        if unclosed_braces != 0 {
          bump_count = fragment_end;
          state = InterpolatedStringState::MissingBrace;
          break;
        }

        fragments.push(StringFragment::Expr(TokenKind::lexer(
          &remainder[fragment_start..fragment_end],
        )));
        previous_fragment_end = fragment_end + 1; // skip the closing brace
      }
      _ => (),
    }

    i += 1;
  }

  if state == InterpolatedStringState::Closed {
    lex.bump(bump_count + 1);
  } else {
    // Move the pointer to EOF
    lex.bump(lex.source().len() - lex.span().end);
  }

  match state {
    InterpolatedStringState::Closed => (),
    InterpolatedStringState::MissingQuote => {
      return StringLiteral::Invalid(MalformedStringLiteral::MissingQuote)
    }
    InterpolatedStringState::MissingBrace => {
      return StringLiteral::Invalid(MalformedStringLiteral::MissingBrace)
    }
  }

  if fragments.len() == 1 && matches!(fragments[0], StringFragment::Text(_)) {
    if let StringFragment::Text(value) = &fragments[0] {
      return StringLiteral::Plain(value.lexeme.clone());
    }
  } else if fragments.is_empty() {
    return StringLiteral::Plain(Cow::from(""));
  }

  StringLiteral::Interpolated(fragments)
}
