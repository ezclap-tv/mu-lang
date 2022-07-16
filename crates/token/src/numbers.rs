use std::num::{ParseFloatError, ParseIntError};

use crate::TokenKind;

#[derive(Clone, Debug)]
pub struct FloatBits(pub f64);

impl PartialEq for FloatBits {
  fn eq(&self, other: &Self) -> bool {
    self.0.to_bits() == other.0.to_bits()
  }
}
impl std::hash::Hash for FloatBits {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.0.to_bits().hash(state);
  }
}
impl Eq for FloatBits {}

pub fn lex_integer<'src>(
  lex: &mut logos::Lexer<'src, TokenKind<'src>>,
) -> Result<i64, ParseIntError> {
  let mut slice = lex.slice();
  let mut radix = 10;

  if slice.starts_with("0b") || slice.starts_with("0x") {
    radix = if slice.starts_with("0b") { 2 } else { 16 };
    slice = &slice[2..];
  }

  let clean = strip_underscore_and_suffix(slice, if radix <= 10 { "i" } else { "" });
  i64::from_str_radix(&clean, radix)
}

pub fn lex_float<'src>(
  lex: &mut logos::Lexer<'src, TokenKind<'src>>,
) -> Result<FloatBits, ParseFloatError> {
  let slice = lex.slice();
  let clean = strip_underscore_and_suffix(slice, "f");
  clean.parse().map(FloatBits)
}

fn strip_underscore_and_suffix(input: &str, suffix: &str) -> String {
  let input = input.strip_suffix(suffix).unwrap_or(input);
  let mut out = String::with_capacity(input.len());
  for c in input.chars() {
    if c == '_' {
      continue;
    }
    out.extend(&[c]);
  }
  out
}
