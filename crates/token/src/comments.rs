use logos::internal::{CallbackResult, LexerInternal};
use logos::Logos;

use crate::TokenKind;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SkipOnSuccess {
  Success,
  Error,
}

impl<'a> CallbackResult<'a, (), TokenKind<'a>> for SkipOnSuccess {
  fn construct<Constructor>(self, _: Constructor, lex: &mut logos::Lexer<'a, TokenKind<'a>>)
  where
    Constructor: Fn(()) -> TokenKind<'a>,
  {
    match self {
      SkipOnSuccess::Success => {
        // Taken from the callback for Filter::Skip
        lex.trivia();
        TokenKind::lex(lex);
      }
      SkipOnSuccess::Error => {
        // Taken from the callback for bool
        lex.set(TokenKind::ERROR)
      }
    }
  }
}

pub fn lex_multi_line_comment<'a>(lex: &mut logos::Lexer<'a, TokenKind<'a>>) -> SkipOnSuccess {
  // how many characters we went through
  let mut n = 0;
  // Mitigate DOS attacks on the lexer with many unclosed comments: /*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*
  // Without this step, the lexer would re-attempt parsing until EOF from every occurrence of /*, leading to O(N^2) worst case performance.
  let mut n_at_last_seen_opening = 0;

  // how many multi-line comment opening tokens we found
  // this starts at one, because the initial /* is already consumed
  let mut opening_count = 1;
  let mut previous_two = [b'*', b'\0'];
  let mut bytes = lex.remainder().bytes();

  while let Some(ch) = bytes.next() {
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
    SkipOnSuccess::Success
  } else {
    lex.bump(n_at_last_seen_opening);
    SkipOnSuccess::Error
  }
}

#[cfg(test)]
mod tests {
  use super::lex_multi_line_comment;
  use crate::comments::SkipOnSuccess;
  use crate::tests::{assert_eq, *};

  #[test]
  fn test_multi_line_comment_parsing() {
    struct Case {
      input: &'static str,
      expected: SkipOnSuccess,
    }

    let tests = [
      Case {
        input: "/**/",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/* */",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/*/**/*/",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/* /**/ */",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/* /* */ */",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/*/* */*/",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/*/***/*/",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/**** /***/*/",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/*/***/*****/",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/* \\\\////// &*&#E*(@798712,)........ ///// // / // /  */",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/* /* /* /* /* /* */ */ */ */ */ */",
        expected: SkipOnSuccess::Success,
      },
      Case {
        input: "/*",
        expected: SkipOnSuccess::Error,
      },
      Case {
        input: "/* /* */",
        expected: SkipOnSuccess::Error,
      },
      Case {
        input: "/*/*/*/* */*/*/",
        expected: SkipOnSuccess::Error,
      },
      // NOTE: this is a success because the lexer stops as soon as it sees the closing */
      Case {
        input: "/**/*/",
        expected: SkipOnSuccess::Success,
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
