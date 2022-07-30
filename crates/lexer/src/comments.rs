use crate::TokenKind;

pub fn lex_multi_line_comment<'a>(lex: &mut logos::Lexer<'a, TokenKind<'a>>) -> bool {
  // how many characters we went through
  let mut n = 0;
  // Mitigate DOS attacks on the lexer with many unclosed comments: /*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
  // Without this step, the lexer would re-attempt parsing until EOF from every occurrence of /*, leading to O(N^2) worst case performance.
  let mut n_at_last_seen_opening = 0;

  // how many multi-line comment opening tokens we found
  // this starts at one, because the initial /* is already consumed
  let mut opening_count = 1;
  let mut previous_two = [b'*', b'\0'];

  for ch in lex.remainder().bytes() {
    n += 1;
    previous_two[0] = previous_two[1];
    previous_two[1] = ch;

    match previous_two {
      [b'/', b'*'] => {
        opening_count += 1;
        n_at_last_seen_opening = n
      }
      [b'*', b'/'] => opening_count -= 1,
      _ => {
        continue;
      }
    }

    if opening_count == 0 {
      break;
    }

    // Set the last byte to /0, so comments like /*/**/*/ get parsed correctly
    previous_two[1] = b'\0';
  }

  if opening_count == 0 {
    lex.bump(n);
    true
  } else {
    lex.bump(n_at_last_seen_opening);
    false
  }
}

#[cfg(test)]
mod tests {
  use super::lex_multi_line_comment;
  // use crate::comments::SkipOnSuccess;
  use crate::tests::{assert_eq, *};

  #[test]
  fn test_multi_line_comment_parsing() {
    struct Case {
      input: &'static str,
      expected: bool,
    }

    let tests = [
      Case {
        input: "/**/",
        expected: true,
      },
      Case {
        input: "/* */",
        expected: true,
      },
      Case {
        input: "/*/**/*/",
        expected: true,
      },
      Case {
        input: "/* /**/ */",
        expected: true,
      },
      Case {
        input: "/* /* */ */",
        expected: true,
      },
      Case {
        input: "/*/* */*/",
        expected: true,
      },
      Case {
        input: "/*/***/*/",
        expected: true,
      },
      Case {
        input: "/**** /***/*/",
        expected: true,
      },
      Case {
        input: "/*/***/*****/",
        expected: true,
      },
      Case {
        input: "/* \\\\////// &*&#E*(@798712,)........ ///// // / // /  */",
        expected: true,
      },
      Case {
        input: "/* /* /* /* /* /* */ */ */ */ */ */",
        expected: true,
      },
      Case {
        input: "/*",
        expected: false,
      },
      Case {
        input: "/* /* */",
        expected: false,
      },
      Case {
        input: "/*/*/*/* */*/*/",
        expected: false,
      },
      // NOTE: this is a success because the lexer stops as soon as it sees the closing */
      Case {
        input: "/**/*/",
        expected: true,
      },
    ];

    for (i, case) in tests.iter().enumerate() {
      assert_eq!(
        &case.input[..2],
        "/*",
        "[Test #{}] All test cases must start with /*",
        i + 1
      );

      let mut lex = TokenKind::lexer(&case.input[2..]);
      assert_eq!(
        lex_multi_line_comment(&mut lex),
        case.expected,
        "[Test #{}] Failed when lexing {:?}",
        i + 1,
        case.input
      );
    }
  }
}
