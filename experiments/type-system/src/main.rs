#![allow(dead_code)]

mod ast;
mod error;
mod lexer;
mod parser;

use std::path::PathBuf;

use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
  #[clap(value_parser)]
  input: PathBuf,
  #[clap(value_parser)]
  output: PathBuf,
}

struct Symbols;

impl ast2str::Symbols for Symbols {
  fn horizontal_bar(&self) -> &'static str {
    " "
  }

  fn vertical_bar(&self) -> &'static str {
    " "
  }

  fn right_branch(&self) -> &'static str {
    " "
  }

  fn indent(&self) -> &'static str {
    " "
  }

  fn left_upper_corner(&self) -> &'static str {
    " "
  }

  fn left_bottom_corner(&self) -> &'static str {
    " "
  }

  fn right_upper_corner(&self) -> &'static str {
    " "
  }

  fn right_bottom_corner(&self) -> &'static str {
    " "
  }

  fn missing_items_symbol(&self) -> &'static str {
    " "
  }

  fn item_list_symbol(&self) -> &'static str {
    " "
  }
}

fn main() {
  let args = Args::parse();

  let input = std::fs::read_to_string(args.input).unwrap();
  match parser::parse(&input) {
    Ok(ast) => {
      use std::io::Write;
      let mut output = std::fs::File::options()
        .create(true)
        .write(true)
        .read(true)
        .open(args.output)
        .unwrap();
      write!(
        output,
        "{}",
        ast
          .into_iter()
          .map(|e| ast2str::AstToStr::ast_to_str_impl(&e, &Symbols))
          .collect::<Vec<_>>()
          .join("\n")
      )
      .unwrap();
    }
    Err(errors) => {
      let mut buf = String::new();
      error::report(&input, &errors, &mut buf);
      panic!("{buf}");
    }
  }
}
