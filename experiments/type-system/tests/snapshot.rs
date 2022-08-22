use std::fs::read_to_string;

use compiler::*;

#[test]
fn tests() {
  insta::glob!("inputs/*", |path| {
    let source = read_to_string(path).unwrap();
    let name = path.file_name().unwrap().to_string_lossy();
    match parser::parse(&source) {
      Ok(ast) => {
        if name.ends_with("-ok") {
          insta::assert_yaml_snapshot!(ast)
        } else {
          panic!("{name} should have failed to parse, but it was successful")
        }
      }
      Err(errors) => {
        if name.ends_with("-fail") {
          insta::assert_yaml_snapshot!(errors)
        } else {
          panic!("{name} should have parsed sucessfully, but it failed")
        }
      }
    }
  });
}
