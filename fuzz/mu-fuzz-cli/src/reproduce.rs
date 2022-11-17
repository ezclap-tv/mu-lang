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
  let (_, errors) = parse(input);
  if !errors.is_empty() {
    let errors = errors
      .into_iter()
      .filter(|e| !matches!(e, Error::Lexer(_, _)))
      .collect::<Vec<_>>();
    println!(
      "[REPRO] Gracefully parsed the input module with errors (lexer errors filtered out):\n{:#?}",
      errors
    );
  } else {
    println!("[REPRO] Gracefully parsed the input module.")
  }
}
