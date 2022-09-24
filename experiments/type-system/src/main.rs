#![allow(dead_code)]

use std::fs;
use std::path::PathBuf;

use clap::Parser;
use compiler::{builtins, error, infer, js, parser};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(value_parser)]
  input: PathBuf,
  #[clap(value_parser)]
  output: PathBuf,
}

fn main() {
  let args = Args::parse();
  let input = std::fs::read_to_string(args.input).expect("failed to read input file");
  let ast = match parser::parse(&input) {
    Ok(ast) => ast,
    Err(e) => {
      let mut buf = String::new();
      error::report(&input, &e, &mut buf);
      panic!("failed to parse input file:\n{buf}");
    }
  };
  if let Err(e) = infer::check_program(ast.clone(), Some(builtins::bindings())) {
    let mut buf = String::new();
    error::report(&input, &e, &mut buf);
    panic!("failed to type check input file:\n{buf}");
  }
  let output = js::emit(ast);
  fs::write(args.output, output).expect("failed to write to output");
}
