#[macro_use]
extern crate afl;

use syntax::lexer::Lexer;

fn main() {
  fuzz!(|data: &[u8]| {
    if let Ok(s) = std::str::from_utf8(data) {
      let _ = Lexer::new(&s).into_iter().collect::<Vec<_>>();
    }
  });
}
