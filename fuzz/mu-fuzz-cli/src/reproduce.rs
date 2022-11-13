use syntax::lexer::Lexer;
use syntax::parser::parse;

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
    Ok(_) => {
      println!("[REPRO] Gracefully parsed the input module.")
    }
    Err(errors) => {
      println!(
        "[REPRO] Gracefully parsed the input module with errors:\n{:#?}",
        errors
      );
    }
  }
}
