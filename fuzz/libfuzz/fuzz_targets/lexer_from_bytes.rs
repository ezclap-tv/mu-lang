#![no_main]
use libfuzzer_sys::fuzz_target;
use mu_lexer::_run_lexer;

fuzz_target!(|data: &[u8]| {
  if let Ok(s) = std::str::from_utf8(data) {
    let _ = _run_lexer(&s);
  }
});
