#![allow(dead_code)]

use std::path::PathBuf;

use clap::Parser;
use compiler::{error, infer, parser};

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

  let input = std::fs::read_to_string(args.input).unwrap();
  let ast = match parser::parse(&input) {
    Ok(ast) => ast,
    Err(errors) => {
      let mut buf = String::new();
      error::report(&input, &errors, &mut buf);
      panic!("{buf}");
    }
  };
  match infer::check_program(ast, None) {
    Ok(bindings) => {
      use std::io::Write;
      let mut output = std::fs::File::options()
        .create(true)
        .write(true)
        .read(true)
        .open(args.output)
        .unwrap();
      write!(output, "{}", serde_yaml::to_string(&bindings).unwrap()).unwrap();
    }
    Err(errors) => {
      let mut buf = String::new();
      error::report(&input, &errors, &mut buf);
      panic!("{buf}");
    }
  }
}
