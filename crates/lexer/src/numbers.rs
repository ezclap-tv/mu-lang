use std::num::{ParseFloatError, ParseIntError};

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

#[derive(Clone, Copy)]
pub enum Radix {
  Binary = 2,
  Decimal = 10,
  Hexadecimal = 16,
}

pub fn str_to_int(lexeme: &str, radix: Radix) -> Result<i64, ParseIntError> {
  let clean = strip_underscores(lexeme);
  i64::from_str_radix(&clean, radix as u32)
}

pub fn str_to_float(lexeme: &str) -> Result<FloatBits, ParseFloatError> {
  lexeme.parse().map(FloatBits)
}

fn strip_underscores(input: &str) -> String {
  let mut out = input.to_string();
  out.retain(|c| c != '_');
  out
}
