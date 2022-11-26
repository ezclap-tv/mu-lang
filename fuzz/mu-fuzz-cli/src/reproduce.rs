use syntax::lexer::Lexer;
use syntax::parser::{parse, Error};

pub fn lexer(input: &str) {
  let tokens = Lexer::new(input).into_iter().collect::<Vec<_>>();
  let errors = tokens
    .iter()
    .filter(|t| t.kind == syntax::lexer::TokenKind::Error)
    .count();

  println!(
    "[REPRO] Lexed {} token(s) with {} error(s).",
    tokens.len(),
    errors,
  );
}

pub fn parser(input: &str) {
  match parse(input) {
    Ok(_) => println!("[REPRO] Gracefully parsed the input module."),
    Err(errors) => {
      let mut buf = String::new();
      errors
        .iter()
        .map(|e| diagnosis::ToReport::to_report(dbg!(e), input.into()).unwrap())
        .for_each(|r| match r.emit(&mut buf) {
          Ok(()) => (),
          Err(e) => {
            eprintln!("[REPRO] Warning: failed to report the error: {e}")
          }
        });

      eprintln!("[REPRO] Gracefully parsed the input module with errors:\n{buf}");
    }
  }
}
