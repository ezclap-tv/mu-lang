use std::borrow::Cow;

use logos::{Logos, Span};

use crate::comments::lex_multi_line_comment;
use crate::numbers::{lex_float, lex_integer, FloatBits};
use crate::strings::{lex_string, StringLiteral};

pub mod comments;
pub mod numbers;
pub mod strings;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'a> {
  pub lexeme: Cow<'a, str>,
  /// Start+end range of this token in the line where it was parsed
  pub span: Span,
  pub kind: TokenKind<'a>,
}

impl<'a> std::hash::Hash for Token<'a> {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.lexeme.hash(state);
    self.span.hash(state);
  }
}

impl<'a> Token<'a> {
  pub fn new<S: Into<Cow<'a, str>>>(lexeme: S, span: Span, kind: TokenKind<'a>) -> Token<'a> {
    Token {
      lexeme: lexeme.into(),
      span,
      kind,
    }
  }
}

pub trait NextTokenExt<'a> {
  fn next_token(&mut self) -> Option<Token<'a>>;
  fn collect_tokens(&mut self) -> Vec<Token<'a>>;
}

impl<'a> NextTokenExt<'a> for logos::Lexer<'a, TokenKind<'a>> {
  fn next_token(&mut self) -> Option<Token<'a>> {
    self
      .next()
      .map(|next| Token::new(self.slice(), self.span(), next))
  }

  fn collect_tokens(&mut self) -> Vec<Token<'a>> {
    let mut out = Vec::new();
    while let Some(t) = self.next_token() {
      out.push(t);
    }
    out
  }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Logos)]
pub enum TokenKind<'src> {
  // keywords
  #[token("import")]
  Import,
  #[token("export")]
  Export,
  #[token("as")]
  As,
  #[token("for")]
  For,
  #[token("while")]
  While,
  #[token("loop")]
  Loop,
  #[token("in")]
  In,
  #[token("return")]
  Return,
  #[token("throw")]
  Throw,
  #[token("break")]
  Break,
  #[token("continue")]
  Continue,
  #[token("with")]
  With,
  #[token("fn")]
  Fn,
  #[token("throws")]
  Throws,
  #[token("class")]
  Class,
  #[token("enum")]
  Enum,
  #[token("type")]
  Type,
  #[token("bool")]
  BoolType,
  #[token("null")]
  Null,
  #[token("int")]
  IntType,
  #[token("float")]
  FloatType,
  #[token("string")]
  StringType,
  #[token("where")]
  Where,
  #[token("do")]
  Do,
  #[token("if")]
  If,
  #[token("else")]
  Else,
  #[token("match")]
  Match,
  #[token("spawn")]
  Spawn,
  #[token("try")]
  Try,
  #[token("try!")]
  TryBang,
  // symbols
  #[token("{")]
  LeftBrace,
  #[token("}")]
  RightBrace,
  #[token("(")]
  LeftParen,
  #[token(")")]
  RightParen,
  #[token("[")]
  LeftBracket,
  #[token("]")]
  RightBracket,
  #[token(";")]
  Semicolon,
  #[token(",")]
  Comma,
  #[token(".")]
  Dot,
  #[token("?.")]
  QuestionDot,
  #[token("..")]
  DotDot,
  #[token("..=")]
  DotDotEqual,
  #[token("...")]
  Spread,
  #[token(":=")]
  ColonEqual,
  #[token(":")]
  Colon,
  #[token("=")]
  Equal,
  #[token("->")]
  ArrowThin,
  #[token("=>")]
  ArrowFat,
  #[token("+")]
  Plus,
  #[token("-")]
  Minus,
  #[token("/")]
  Slash,
  #[token("*")]
  Star,
  #[token("%")]
  Percent,
  #[token("**")]
  Power,
  #[token("&")]
  BitAnd,
  #[token("|")]
  BitOr,
  #[token("^")]
  BitXor,
  #[token("||")]
  Or,
  #[token("&&")]
  And,
  #[token("?")]
  Question,
  #[token("+=")]
  PlusEqual,
  #[token("-=")]
  MinusEqual,
  #[token("/=")]
  SlashEqual,
  #[token("*=")]
  StarEqual,
  #[token("%=")]
  PercentEqual,
  #[token("**=")]
  PowerEqual,
  #[token("&=")]
  BitAndEqual,
  #[token("|=")]
  BitOrEqual,
  #[token("^=")]
  BitXorEqual,
  #[token("<<=")]
  ShiftLeftEqual,
  #[token(">>=")]
  ShiftRightEqual,
  #[token("||=")]
  OrEqual,
  #[token("&&=")]
  AndEqual,
  #[token("??=")]
  CoalescingEqual,
  #[token("|>")]
  Pipeline,
  #[token("#")]
  PoundSign,
  #[token("==")]
  EqualEqual,
  #[token("!=")]
  BangEqual,
  #[token(">")]
  Greater,
  #[token(">=")]
  GreaterEqual,
  #[token("<")]
  Less,
  #[token("<=")]
  LessEqual,
  #[token("<<")]
  ShiftLeft,
  #[token(">>")]
  ShiftRight,
  #[token("!")]
  Bang,
  #[token("~")]
  BitNot,
  #[token("++")]
  PlusPlus,
  #[token("--")]
  MinusMinus,
  #[token("\\")]
  BackSlash,

  // Identifiers
  #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
  Identifier,

  #[regex("'[a-zA-Z_][a-zA-Z_0-9]*")]
  Label,
  // Literals
  #[token("true")]
  True,
  #[token("false")]
  False,
  #[regex("\"", lex_string)]
  StringLit(StringLiteral<'src>),

  // NOTE: Int has precedence over floats
  #[regex("[0-9]([0-9_]*[0-9])?i?", |lex| lex_integer(lex), priority = 100)]
  #[regex("0b[01]([01_]*[01])?", |lex| lex_integer(lex))]
  #[regex("0x[0-9a-fA-F]([0-9a-fA-F_]*[0-9a-fA-F])?", |lex| lex_integer(lex))]
  IntLit(i64),

  #[token("inf", |_| FloatBits(f64::INFINITY), priority=99)]
  #[regex(r"[0-9]+(\.[0-9]+)?([Ee][+-]?[0-9]+)?f?", lex_float)]
  FloatLit(FloatBits),

  // Misc
  #[token("\n")]
  LineEnd,

  #[regex("//[^\n]*", logos::skip)]
  Comment,

  /// Multi-line comment (they can be nested)
  #[regex("/\\*", lex_multi_line_comment)]
  MultiLineComment,

  #[regex("[ \r\t]+", logos::skip)]
  Whitespace,

  #[error]
  Invalid,
  EOF,
}

#[cfg(feature = "fuzz")]
#[doc(hidden)]
pub fn _run_lexer(s: &'_ str) -> Vec<Token<'_>> {
  let mut output = Vec::new();

  fn collect_tokens<'a>(output: &mut Vec<Token<'a>>, lex: &mut logos::Lexer<'a, TokenKind<'a>>) {
    while let Some(mut t) = lex.next_token() {
      match &mut t.kind {
        TokenKind::StringLit(s) => match s {
          StringLiteral::Plain(_) => output.push(t),
          StringLiteral::Invalid(_) => output.push(t),
          StringLiteral::Interpolated(fragments) => {
            let i = output.len();
            output.push(Token::new("", 0..1, TokenKind::Invalid));

            for frag in fragments {
              match frag {
                strings::StringFragment::Text(text) => {
                  output.push(text.clone());
                }
                strings::StringFragment::Expr(lex) => collect_tokens(output, lex),
              }
            }

            output[i] = t;
          }
        },
        _ => output.push(t),
      }
    }
  }

  collect_tokens(&mut output, &mut TokenKind::lexer(s));
  output
}

#[cfg(test)]
pub(crate) mod tests {
  pub(crate) use pretty_assertions::assert_eq;
  use utils::*;

  pub(crate) use super::{Logos, NextTokenExt, Token, TokenKind};
  use crate::numbers::FloatBits;
  use crate::strings::{MalformedStringLiteral, StringFragment, StringLiteral};

  pub(crate) struct TestToken<'a>(Token<'a>);
  impl<'a> std::convert::From<Token<'a>> for TestToken<'a> {
    fn from(token: Token<'a>) -> Self {
      TestToken(token)
    }
  }
  impl<'a> std::ops::Deref for TestToken<'a> {
    type Target = Token<'a>;
    fn deref(&self) -> &Self::Target {
      &self.0
    }
  }
  impl<'a> std::ops::DerefMut for TestToken<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
      &mut self.0
    }
  }
  impl<'a> PartialEq<TestToken<'a>> for TestToken<'a> {
    fn eq(&self, other: &TestToken<'a>) -> bool {
      self.0.kind == other.0.kind && self.0.lexeme == other.0.lexeme
    }
  }
  impl<'a> std::fmt::Debug for TestToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      f.debug_struct("TestToken")
        .field("lexeme", &self.0.lexeme)
        .field("kind", &self.0.kind)
        .finish()
    }
  }

  fn test_tokenize_inner<'a>(
    lex: &mut logos::Lexer<'a, TokenKind<'a>>,
    newlines: bool,
  ) -> Vec<TestToken<'a>> {
    let mut out = Vec::new();
    while let Some(token) = lex.next_token() {
      if !newlines && matches!(token.kind, TokenKind::LineEnd) {
        continue;
      }
      out.push(token.into());
    }
    out
  }
  fn test_tokenize(src: &'_ str) -> Vec<TestToken<'_>> {
    test_tokenize_inner(&mut TokenKind::lexer(src), false)
  }

  pub fn int(i: i64) -> TokenKind<'static> {
    TokenKind::IntLit(i)
  }

  pub fn float(f: f64) -> TokenKind<'static> {
    TokenKind::FloatLit(FloatBits(f))
  }

  pub fn string(s: &'_ str) -> TokenKind<'_> {
    TokenKind::StringLit(crate::strings::StringLiteral::Plain(s.into()))
  }

  macro_rules! token {
    ($kind:ident, $lexeme:literal) => {
      TestToken(Token::new($lexeme, 0..1, TokenKind::$kind))
    };
    ($kind:expr, $lexeme:literal) => {
      TestToken(Token::new($lexeme, 0..1, $kind))
    };
  }

  #[test]
  fn ident() {
    const SOURCE: &str = "ident";
    assert_eq!(test_tokenize(SOURCE), vec![token!(Identifier, "ident")]);
  }

  pub(crate) use token;

  #[test]
  fn suffixes() {
    const SOURCE: &str = r#"

    weight := 123kg
    uuid   := "123"uuid
    
    "#;

    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Identifier, "weight"),
        token!(ColonEqual, ":="),
        token!(int(123), "123"),
        token!(Identifier, "kg"),
        token!(Identifier, "uuid"),
        token!(ColonEqual, ":="),
        token!(string("123"), "\"123\""),
        token!(Identifier, "uuid"),
      ]
    )
  }

  #[test]
  fn pipeline() {
    const SOURCE: &str = "expensive() |> (#, #, #)";
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Identifier, "expensive"),
        token!(LeftParen, "("),
        token!(RightParen, ")"),
        token!(Pipeline, "|>"),
        token!(LeftParen, "("),
        token!(PoundSign, "#"),
        token!(Comma, ","),
        token!(PoundSign, "#"),
        token!(Comma, ","),
        token!(PoundSign, "#"),
        token!(RightParen, ")"),
      ]
    );
  }

  #[test]
  fn arithmetic() {
    const SOURCE: &str = r#"+-*/%**"#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Plus, "+"),
        token!(Minus, "-"),
        token!(Star, "*"),
        token!(Slash, "/"),
        token!(Percent, "%"),
        token!(Power, "**")
      ]
    );
  }

  #[test]
  fn boolean() {
    const SOURCE: &str = r#"! < > == != <= >="#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Bang, "!"),
        token!(Less, "<"),
        token!(Greater, ">"),
        token!(EqualEqual, "=="),
        token!(BangEqual, "!="),
        token!(LessEqual, "<="),
        token!(GreaterEqual, ">=")
      ]
    );
  }

  #[test]
  fn dot_tokens() {
    const SOURCE: &str = r#"ident.ident ident?.ident 0..0 0..=0 ...ident"#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Identifier, "ident"),
        token!(Dot, "."),
        token!(Identifier, "ident"),
        token!(Identifier, "ident"),
        token!(QuestionDot, "?."),
        token!(Identifier, "ident"),
        token!(int(0), "0"),
        token!(DotDot, ".."),
        token!(int(0), "0"),
        token!(int(0), "0"),
        token!(DotDotEqual, "..="),
        token!(int(0), "0"),
        token!(Spread, "..."),
        token!(Identifier, "ident")
      ]
    );
  }

  #[test]
  fn field_access_range() {
    const SOURCE: &str = r#"ident.ident..ident.ident"#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Identifier, "ident"),
        token!(Dot, "."),
        token!(Identifier, "ident"),
        token!(DotDot, ".."),
        token!(Identifier, "ident"),
        token!(Dot, "."),
        token!(Identifier, "ident"),
      ]
    );
  }

  #[test]
  fn float_range() {
    const SOURCE: &str = r#"0.0..1.0"#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(float(0.0), "0.0"),
        token!(DotDot, ".."),
        token!(float(1.0), "1.0")
      ]
    );
  }

  #[test]
  fn simple_range() {
    const SOURCE: &str = r#"0..1"#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(int(0), "0"),
        token!(DotDot, ".."),
        token!(int(1), "1")
      ]
    );
  }

  #[test]
  fn comment() {
    const SOURCE: &str = "// asdfasdfasdfasdfasdfasdfasdf\nident";
    assert_eq!(test_tokenize(SOURCE), vec![token!(Identifier, "ident")]);
  }

  #[test]
  fn multi_comment() {
    const SOURCE: &str = "/* */ /* /* /* /******afhišuhůů§ßß×××$$ůĐ[đĐ[đĐ[đ*/ */ */ */ ident";
    assert_eq!(test_tokenize(SOURCE), vec![token!(Identifier, "ident")]);
  }

  #[test]
  fn unterminated_multi_comment() {
    const SOURCE: &str = "/* ident";
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(TokenKind::Invalid, "/*"),
        token!(Identifier, "ident")
      ]
    );
  }

  #[test]
  fn labeled_loop() {
    const SOURCE: &str = "'label: loop {}";
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Label, "'label"),
        token!(Colon, ":"),
        token!(Loop, "loop"),
        token!(LeftBrace, "{"),
        token!(RightBrace, "}"),
      ]
    );
  }

  #[test]
  fn bad_tokens() {
    const SOURCE: &str = "ß$÷×";
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Invalid, "ß"),
        token!(Invalid, "$"),
        token!(Invalid, "÷"),
        token!(Invalid, "×")
      ]
    );
  }

  #[test]
  fn strings() {
    const SOURCE: &str = r#"
      " a string"
      "more strings"
      "string string string "
      "ß$÷×"
      // 'ß$÷×'
      "}}}}"
      "ok}"

      "multi
      line
      string
      "
    "#;

    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(string(" a string"), "\" a string\""),
        token!(string("more strings"), "\"more strings\""),
        token!(string("string string string "), "\"string string string \""),
        token!(string("ß$÷×"), "\"ß$÷×\""),
        // token!(string("ß$÷×"), "'ß$÷×'"),
        token!(string("}}}}"), "\"}}}}\""),
        token!(string("ok}"), "\"ok}\""),
        token!(
          string("multi\n      line\n      string\n      "),
          "\"multi\n      line\n      string\n      \""
        )
      ]
    );
  }

  #[test]
  fn unterminated_string() {
    const SOURCE: &str = r#"
      " a string
    "#;

    assert_eq!(
      test_tokenize(SOURCE),
      vec![token!(
        TokenKind::StringLit(StringLiteral::Invalid(MalformedStringLiteral::MissingQuote)),
        "\" a string\n    "
      )]
    );
  }

  #[test]
  fn unterminated_fragment() {
    const SOURCE: &str = r#"
      " bonjour {""{bonjour "
    "#;

    assert_eq!(
      test_tokenize(SOURCE),
      vec![token!(
        TokenKind::StringLit(StringLiteral::Invalid(MalformedStringLiteral::MissingBrace)),
        "\" bonjour {\"\"{bonjour \"\n    "
      )]
    );
  }

  #[test]
  fn test_adjacent_curlies_with_unicode_modifier() {
    // NOTE: the first } has a unicode marker on it (0x065F, ARABIC WAVY HAMZA BELOW)
    // bytes: [34, 123, 125, 217, 159, 123]
    const SOURCE: &str = r#""{}ٟ{}"  "{}{}""#;

    let mut actual = test_tokenize(SOURCE);
    compare_interpolated_strings(
      &mut actual,
      &[
        interp_token!(
          "\"{}ٟ{}\"",
          frag_list![frag_expr!(), frag_text!("\u{65f}"), frag_expr!()]
        ),
        interp_token!("\"{}{}\"", frag_list![frag_expr!(), frag_expr!()]),
      ],
    );
  }

  #[test]
  fn test_lex_unterminated_empty_string_at_the_end_of_file() {
    const SOURCE: &str = "some_other_code \"";
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Identifier, "some_other_code"),
        token!(
          TokenKind::StringLit(StringLiteral::Invalid(MalformedStringLiteral::MissingQuote)),
          "\""
        )
      ]
    );
  }

  #[test]
  fn test_unterminated_interpolation_with_codepoint_at_the_end() {
    let source: &str = std::str::from_utf8(&[
      219, 188, 43, 125, 62, 0, 46, 70, 43, 125, 125, 0, 112, 123, 125, 0, 112, 123, 44, 0, 112,
      34, 44, 70, 123, 43, 125, 0, 35, 112, 70, 43, 209, 188,
    ])
    .unwrap();
    assert_eq!(
      test_tokenize(source),
      vec![
        token!(Invalid, "ۼ"),
        token!(Plus, "+"),
        token!(RightBrace, "}"),
        token!(Greater, ">"),
        token!(Invalid, "\0"),
        token!(Dot, "."),
        token!(Identifier, "F"),
        token!(Plus, "+"),
        token!(RightBrace, "}"),
        token!(RightBrace, "}"),
        token!(Invalid, "\0"),
        token!(Identifier, "p"),
        token!(LeftBrace, "{"),
        token!(RightBrace, "}"),
        token!(Invalid, "\0"),
        token!(Identifier, "p"),
        token!(LeftBrace, "{"),
        token!(Comma, ","),
        token!(Invalid, "\0"),
        token!(Identifier, "p"),
        token!(
          TokenKind::StringLit(StringLiteral::Invalid(MalformedStringLiteral::MissingQuote)),
          "\",F{+}\0#pF+Ѽ"
        ),
      ]
    );
  }

  #[test]
  fn string_interpolation_edge_cases() {
    const SOURCE: &str = r#"
      "{}"
      " {}"
      "{} "
      "{a}"
      " {a}"
      "{a} "
    "#;

    let mut actual = test_tokenize(SOURCE);
    compare_interpolated_strings(
      &mut actual,
      &[
        interp_token!("\"{}\"", frag_list![frag_expr!()]),
        interp_token!("\" {}\"", frag_list![frag_text!(" "), frag_expr!()]),
        interp_token!("\"{} \"", frag_list![frag_expr!(), frag_text!(" ")]),
        interp_token!("\"{a}\"", frag_list![frag_expr!(token!(Identifier, "a"))]),
        interp_token!(
          "\" {a}\"",
          frag_list![frag_text!(" "), frag_expr!(token!(Identifier, "a"))]
        ),
        interp_token!(
          "\"{a} \"",
          frag_list![frag_expr!(token!(Identifier, "a")), frag_text!(" ")]
        ),
      ],
    )
  }

  #[test]
  fn test_interpolated_string_escape() {
    const SOURCE: &str = r#" "\{a}" "\{}" "\{" "\{\{{}" "\n\n" "#;
    let mut actual = test_tokenize(SOURCE);
    compare_interpolated_strings(
      &mut actual,
      &[
        interp_token!("\"\\{a}\"", "\\{a}"),
        interp_token!("\"\\{}\"", "\\{}"),
        interp_token!("\"\\{\"", "\\{"),
        interp_token!(
          "\"\\{\\{{}\"",
          frag_list![frag_text!("\\{\\{"), frag_expr!()]
        ),
        interp_token!("\"\\n\\n\"", "\\n\\n"),
      ],
    );
  }

  #[test]
  fn string_interpolation() {
    const SOURCE: &str = r#" "1 + 1 = {1 + 1}""#;
    let mut actual = test_tokenize(SOURCE);

    compare_interpolated_strings(
      &mut actual,
      &[interp_token!(
        "\"1 + 1 = {1 + 1}\"",
        frag_list![
          frag_text!("1 + 1 = "),
          frag_expr!(token!(int(1), "1"), token!(Plus, "+"), token!(int(1), "1"))
        ]
      )],
    )
  }

  #[test]
  fn string_interpolation_nested_block() {
    const SOURCE: &str = r#" "a { do { "nested block" } }" "#;
    let mut actual = test_tokenize(SOURCE);
    compare_interpolated_strings(
      &mut actual,
      &[interp_token!(
        "\"a { do { \"nested block\" } }\"",
        frag_list![
          frag_text!("a "),
          frag_expr![
            token!(Do, "do"),
            token!(LeftBrace, "{"),
            token!(string("nested block"), "\"nested block\""),
            token!(RightBrace, "}")
          ]
        ]
      )],
    )
  }

  #[test]
  fn string_interpolation_nested_string() {
    // FIXME: escaped nested curly leads to the entire file being parsed as Invalid
    const SOURCE: &str = r#" 
    "a { "hello I'm nested" }" 
    "b { "nested \{ curly" }"
"#;
    let mut actual = test_tokenize(SOURCE);
    compare_interpolated_strings(
      &mut actual,
      &[
        interp_token!(
          "\"a { \"hello I'm nested\" }\"",
          frag_list![
            frag_text!("a "),
            frag_expr![token!(string("hello I'm nested"), "\"hello I'm nested\"")]
          ]
        ),
        interp_token!(
          "\"b { \"nested \\{ curly\" }\"",
          frag_list![
            frag_text!("b "),
            frag_expr![token!(string("nested \\{ curly"), "\"nested \\{ curly\"")]
          ]
        ),
      ],
    )
  }

  #[test]
  fn unicode_string() {
    const SOURCE: &str = r#""😂""#;
    assert_eq!(test_tokenize(SOURCE), vec![token!(string("😂"), "\"😂\"")]);
  }

  #[test]
  fn array() {
    const SOURCE: &str = r#"[0, "a", null, a]"#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(LeftBracket, "["),
        token!(int(0), "0"),
        token!(Comma, ","),
        token!(string("a"), "\"a\""),
        token!(Comma, ","),
        token!(Null, "null"),
        token!(Comma, ","),
        token!(Identifier, "a"),
        token!(RightBracket, "]"),
      ]
    );
  }

  #[test]
  fn import_and_export() {
    const SOURCE: &str = r#"
      import thing
      export v := 0
      "#;
    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Import, "import"),
        token!(Identifier, "thing"),
        token!(Export, "export"),
        token!(Identifier, "v"),
        token!(ColonEqual, ":="),
        token!(int(0), "0"),
      ]
    )
  }

  #[test]
  fn numeric_literals() {
    let source: &str = r#"123
      123_321
      1_2_3_4_5
      999_999_999
      999_999_999_999_999
      999_999_999_999_999_999
      999_999_999_999_999_999_999
      1.0
      1e300
      1e300f
      100f
      100i
      
      0xABCDEF
      0xA_B_C_D_E_F
      0xDEAD_CAFE
      0x1_F331_D0D
      
      0b1111_0000_0101_1010_1100_0011
      0b000000
      0b111111
"#;
    assert_eq!(
      test_tokenize(source),
      vec![
        token!(int(123), "123"),
        token!(int(123_321), "123_321"),
        token!(int(1_2_3_4_5), "1_2_3_4_5"),
        token!(int(999_999_999), "999_999_999"),
        token!(int(999_999_999_999_999), "999_999_999_999_999"),
        token!(int(999_999_999_999_999_999), "999_999_999_999_999_999"),
        token!(Invalid, "999_999_999_999_999_999_999"),
        token!(float(1.0), "1.0"),
        token!(float(1e300), "1e300"),
        token!(float(1e300), "1e300f"),
        token!(float(100.0), "100f"),
        token!(int(100), "100i"),
        token!(int(0xABCDEF), "0xABCDEF"),
        token!(int(0xA_B_C_D_E_F), "0xA_B_C_D_E_F"),
        token!(int(0xDEAD_CAFE), "0xDEAD_CAFE"),
        token!(int(0x1_F331_D0D), "0x1_F331_D0D"),
        token!(
          int(0b1111_0000_0101_1010_1100_0011),
          "0b1111_0000_0101_1010_1100_0011"
        ),
        token!(int(0b000000), "0b000000"),
        token!(int(0b111111), "0b111111"),
      ]
    )
  }

  #[test]
  fn keywords() {
    const SOURCE: &str = r"
    import export as for while loop in return throw break continue with fn throws class enum type bool null int float string where do if else match spawn try try!
    ";

    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(Import, "import"),
        token!(Export, "export"),
        token!(As, "as"),
        token!(For, "for"),
        token!(While, "while"),
        token!(Loop, "loop"),
        token!(In, "in"),
        token!(Return, "return"),
        token!(Throw, "throw"),
        token!(Break, "break"),
        token!(Continue, "continue"),
        token!(With, "with"),
        token!(Fn, "fn"),
        token!(Throws, "throws"),
        token!(Class, "class"),
        token!(Enum, "enum"),
        token!(Type, "type"),
        token!(BoolType, "bool"),
        token!(Null, "null"),
        token!(IntType, "int"),
        token!(FloatType, "float"),
        token!(StringType, "string"),
        token!(Where, "where"),
        token!(Do, "do"),
        token!(If, "if"),
        token!(Else, "else"),
        token!(Match, "match"),
        token!(Spawn, "spawn"),
        token!(Try, "try"),
        token!(TryBang, "try!"),
      ]
    )
  }

  #[test]
  fn symbols() {
    const SOURCE: &str = "{ } ( ) [ ] ; , . ?. .. ..= ... := : = -> => + - / * % ** & | ^ || && ? += -= /= *= %= **= &= |= ^= <<= >>= ||= &&= ??= |> # == != > >= < <= << >> ! ~ ++ -- \\";

    assert_eq!(
      test_tokenize(SOURCE),
      vec![
        token!(LeftBrace, "{"),
        token!(RightBrace, "}"),
        token!(LeftParen, "("),
        token!(RightParen, ")"),
        token!(LeftBracket, "["),
        token!(RightBracket, "]"),
        token!(Semicolon, ";"),
        token!(Comma, ","),
        token!(Dot, "."),
        token!(QuestionDot, "?."),
        token!(DotDot, ".."),
        token!(DotDotEqual, "..="),
        token!(Spread, "..."),
        token!(ColonEqual, ":="),
        token!(Colon, ":"),
        token!(Equal, "="),
        token!(ArrowThin, "->"),
        token!(ArrowFat, "=>"),
        token!(Plus, "+"),
        token!(Minus, "-"),
        token!(Slash, "/"),
        token!(Star, "*"),
        token!(Percent, "%"),
        token!(Power, "**"),
        token!(BitAnd, "&"),
        token!(BitOr, "|"),
        token!(BitXor, "^"),
        token!(Or, "||"),
        token!(And, "&&"),
        token!(Question, "?"),
        token!(PlusEqual, "+="),
        token!(MinusEqual, "-="),
        token!(SlashEqual, "/="),
        token!(StarEqual, "*="),
        token!(PercentEqual, "%="),
        token!(PowerEqual, "**="),
        token!(BitAndEqual, "&="),
        token!(BitOrEqual, "|="),
        token!(BitXorEqual, "^="),
        token!(ShiftLeftEqual, "<<="),
        token!(ShiftRightEqual, ">>="),
        token!(OrEqual, "||="),
        token!(AndEqual, "&&="),
        token!(CoalescingEqual, "??="),
        token!(Pipeline, "|>"),
        token!(PoundSign, "#"),
        token!(EqualEqual, "=="),
        token!(BangEqual, "!="),
        token!(Greater, ">"),
        token!(GreaterEqual, ">="),
        token!(Less, "<"),
        token!(LessEqual, "<="),
        token!(ShiftLeft, "<<"),
        token!(ShiftRight, ">>"),
        token!(Bang, "!"),
        token!(BitNot, "~"),
        token!(PlusPlus, "++"),
        token!(MinusMinus, "--"),
        token!(BackSlash, "\\"),
      ]
    );
  }

  static CRATE_ROOT: &str = env!("CARGO_MANIFEST_DIR");
  static TESTS_DIR: &str = "tests";

  fn dump_tokens(src: String) -> String {
    let mut lex = TokenKind::lexer(&src[..]);
    lex
      .collect_tokens()
      .iter_mut()
      .filter(|t| t.kind != TokenKind::LineEnd)
      .map(dump_token)
      .collect::<Vec<_>>()
      .join("\n")
  }

  fn dump_token<'a>(t: &mut Token<'a>) -> String {
    let mut out = String::new();
    match &mut t.kind {
      TokenKind::StringLit(s) => match s {
        StringLiteral::Plain(_) | StringLiteral::Invalid(_) => {
          add_lexeme(&mut out, format!("StringLiteral::{:?}", s), &t.lexeme);
        }
        StringLiteral::Interpolated(frags) => {
          add_lexeme(&mut out, "StringLiteral::Interpolated {", &t.lexeme);

          for (_, frag) in frags.iter_mut().enumerate() {
            out.push_str("\n  ");
            match frag {
              StringFragment::Text(text) => {
                out.push_str("Text {\n    ");
                out.push_str(&dump_token(text));
                out.push_str("\n  }");
              }
              StringFragment::Expr(expr) => {
                out.push_str("Fragment {");
                let mut expr_tokens = expr.collect_tokens();
                for token in &mut expr_tokens {
                  for line in dump_token(token).trim_end().split('\n') {
                    out.push_str("\n    ");
                    out.push_str(&line);
                  }
                }
                if !expr_tokens.is_empty() {
                  out.push('\n');
                }
                out.push_str("  }");
              }
            }
          }

          if !frags.is_empty() {
            out.push('\n');
          }

          out.push('}');
        }
      },
      _ => add_lexeme(&mut out, format!("{:?}", t.kind), &t.lexeme),
    }
    out
  }

  fn add_lexeme<S: Into<String>>(out: &mut String, s: S, lexeme: impl AsRef<str>) {
    out.push_str(&format!("{: <32} {:?}", s.into(), lexeme.as_ref()))
  }

  mu_testing::make_test_macros!(eq => CRATE_ROOT, TESTS_DIR, dump_tokens);
  test_eq!(tour);

  mod utils {
    pub(crate) use super::{assert_eq, *};

    macro_rules! interp_token {
      ($lexeme:tt, $literal:tt) => {
        ExpectedString {
          lexeme: $lexeme,
          literal: ExpectedStringLiteral::Plain($literal),
        }
      };
      ($lexeme:tt, $literal:expr) => {
        ExpectedString {
          lexeme: $lexeme,
          literal: $literal,
        }
      };
    }

    macro_rules! frag_text {
      ($text:literal) => {
        ExpectedFragment::Text(token!(string($text), $text))
      };
    }
    macro_rules! frag_expr {
      ($($val:expr),*) => {
        ExpectedFragment::Expr(vec![$($val),*])
      };
    }

    macro_rules! frag_list {
      ($($val:expr),*) => {
        frag_list!(internal vec![$($val),*])
      };
      (internal $vec:expr) => {
        ExpectedStringLiteral::Interpolated($vec)
      };
    }

    pub(crate) use {frag_expr, frag_list, frag_text, interp_token};

    pub struct True<T: std::fmt::Debug>(pub T);
    impl<T: std::fmt::Debug> std::fmt::Debug for True<T> {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
      }
    }
    pub struct False<T: std::fmt::Debug>(pub T);
    impl<T: std::fmt::Debug> std::fmt::Debug for False<T> {
      fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
      }
    }
    impl<T: std::fmt::Debug, U: std::fmt::Debug> std::cmp::PartialEq<False<U>> for True<T> {
      fn eq(&self, _other: &False<U>) -> bool {
        false
      }
    }
    impl<T: std::fmt::Debug, U: std::fmt::Debug> std::cmp::PartialEq<True<U>> for False<T> {
      fn eq(&self, _other: &True<U>) -> bool {
        false
      }
    }

    #[derive(Debug)]
    pub(crate) struct ExpectedString<'a> {
      pub(crate) lexeme: &'a str,
      pub(crate) literal: ExpectedStringLiteral<'a>,
    }
    #[derive(Debug)]
    pub(crate) enum ExpectedFragment<'a> {
      Text(TestToken<'a>),
      Expr(Vec<TestToken<'a>>),
    }
    #[allow(dead_code)]
    #[derive(Debug)]
    pub(crate) enum ExpectedStringLiteral<'a> {
      Plain(&'a str),
      Interpolated(Vec<ExpectedFragment<'a>>),
      Invalid(MalformedStringLiteral),
    }
    pub(crate) fn compare_interpolated_strings<'a>(
      tokens: &mut [TestToken<'a>],
      expected: &[ExpectedString<'a>],
    ) {
      if tokens.len() != expected.len() {
        assert_eq!(
          False(&tokens),
          True(expected),
          "Lexer produced a token sequence with invalid length: {} != {}",
          tokens.len(),
          expected.len(),
        );
        unreachable!();
      }

      for (i, (t, e)) in tokens.iter_mut().zip(expected.iter()).enumerate() {
        assert_eq!(t.lexeme, e.lexeme, "Mismatching lexemes at token={i}");

        let lit = if let TokenKind::StringLit(lit) = &mut t.kind {
          lit
        } else {
          assert_eq!(
            utils::False(&t),
            utils::True(e),
            "Token={i} isn't a string literal: {:?}",
            t
          );
          unreachable!();
        };

        match (lit, &e.literal) {
          (StringLiteral::Plain(actual), ExpectedStringLiteral::Plain(expected)) => {
            assert_eq!(actual, expected, "Mismatching literal strings at token={i}");
          }
          (StringLiteral::Interpolated(actual), ExpectedStringLiteral::Interpolated(expected)) => {
            assert_eq!(
            actual.len(),
            expected.len(),
            "Mismatching fragment lengths at token={i}:\nactual -> {actual:#?}\nexpected -> {expected:#?}\n"
          );
            for (frag_idx, (actual_frag, expected_frag)) in
              actual.iter_mut().zip(expected.iter()).enumerate()
            {
              match (actual_frag, expected_frag) {
                (StringFragment::Text(actual_text), ExpectedFragment::Text(expected_text)) => {
                  let token: TestToken = actual_text.clone().into();
                  assert_eq!(
                    &token, expected_text,
                    "Mismatching text at fragment={frag_idx} of token={i}"
                  );
                }
                (StringFragment::Expr(actual_expr), ExpectedFragment::Expr(expected_tokens)) => {
                  let actual_tokens = test_tokenize_inner(actual_expr, true);
                  if actual_tokens.len() != expected_tokens.len() {
                    assert_eq!(
                      utils::False(&actual_tokens),
                      utils::True(expected_tokens),
                      "Mismatching tokens at fragment={frag_idx} of token={i}: {} != {}",
                      actual_tokens.len(),
                      expected_tokens.len(),
                    );
                    unreachable!();
                  }
                  assert_eq!(
                    &actual_tokens, expected_tokens,
                    "Mismatching tokens at fragment={frag_idx} of token={i}"
                  );
                }
                (actual_frag, expected_frag) => {
                  assert_eq!(
                    utils::False(actual_frag),
                    utils::True(expected_frag),
                    "Mismatching interpolated string fragments at fragment={frag_idx} of token={i}",
                  );
                }
              }
            }
          }
          (StringLiteral::Invalid(actual), ExpectedStringLiteral::Invalid(expected)) => {
            assert_eq!(
              actual, expected,
              "Mismatching invalid string literals at token={i}"
            );
          }
          (lit, _) => {
            assert_eq!(
              utils::False(lit),
              utils::True(&e.literal),
              "Mismatching string literal kinds at token={i}"
            );
          }
        }
      }
    }
  }
}
