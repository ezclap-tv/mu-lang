#![no_main]
use libfuzzer_sys::fuzz_target;
use syntax::lexer::Lexer;

fuzz_target!(|data: &[u8]| {
  if let Ok(s) = std::str::from_utf8(data) {
    let _ = Lexer::new(&s).into_iter().collect::<Vec<_>>();
  }
});
