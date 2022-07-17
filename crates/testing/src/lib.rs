use std::borrow::Cow;
use std::path::Path;

pub use lazy_static::lazy_static;
pub use mu_testing_macro::mu_test_suite;
pub use pretty_assertions;

/// The kind of output a test may have.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputKind {
  OkOrEq,
  Err,
}

/// Loads a test file from the given tests directory and returns a tuple of
/// (source code, expected output).
pub fn load_test_file(tests_dir: &Path, name: &str) -> (String, String, OutputKind) {
  let path = std::path::PathBuf::from(tests_dir).join(format!("{}.test", name));
  let source = std::fs::read_to_string(&path)
    .map_err(|e| format!("Failed to read `{}`: {}", path.display(), e))
    .unwrap();
  let source = source.replace("\r\n", "\n");
  let (code, expected) = source
    .split_once("%output")
    .expect("Invalid test file format. Make sure that your test contains the %output directive.");
  let (output_kind, expected) = expected.split_once('\n').expect(
    "Invalid test file format. Make sure that the %output directive is on a separate line.",
  );
  let output_kind = match output_kind.trim() {
    "err" => OutputKind::Err,
    "ok" | "eq" | "" => OutputKind::OkOrEq,
    rest => panic!(
      "Unrecognized test type -- `{}`. Expected either `ok`, `err`, or `eq`",
      rest
    ),
  };
  (
    code.trim().to_owned(),
    expected.trim().to_owned(),
    output_kind,
  )
}

/// Asserts that output of the given closure matches the expected output.
pub fn test_eq<F>(test_name: &str, source: String, expected: String, f: F)
where
  F: Fn(String) -> String,
{
  let output = f(source);

  let pretty_assertions = option_env!("MU_TEST_PLAIN_ASSERT")
    .map(|v| v.trim() != test_name)
    .unwrap_or(true);
  if pretty_assertions {
    pretty_assertions::assert_eq!(
      DisplayAsDebugWrapper(output.trim()),
      DisplayAsDebugWrapper(&expected[..]),
      "Failed the error test `{}`",
      test_name
    );
  } else {
    assert_eq!(
      DisplayAsDebugWrapper(output.trim()),
      DisplayAsDebugWrapper(&expected[..]),
      "Failed the error test `{}`",
      test_name
    );
  }
}

// Asserts that output of the given pipeline closure is a list of errors.
/// Compares the diagnostics for the errors to the given expected output.
pub fn test_err<T, F>(test_name: &str, source: String, expected: String, pipeline: F)
where
  T: std::fmt::Debug,
  F: for<'a> Fn(Cow<'a, str>) -> Result<T, String>,
{
  let src = std::borrow::Cow::Borrowed(&source[..]);
  let errors = pipeline(src).expect_err("Test succeeded unexpectedly");
  pretty_assertions::assert_eq!(
    DisplayAsDebugWrapper(errors),
    DisplayAsDebugWrapper(expected),
    "Failed the error test `{}`",
    test_name
  );
}

/// Compares the output of the given function and the expected output.
pub fn test_ok<E, F>(test_name: &str, src: String, expected: String, pipeline: F)
where
  E: std::error::Error,
  F: for<'a> Fn(Cow<'a, str>) -> Result<String, E>,
{
  let src = std::borrow::Cow::Borrowed(&src[..]);
  pretty_assertions::assert_eq!(
    DisplayAsDebugWrapper(pipeline(src).expect("Test failed unexpectedly")),
    DisplayAsDebugWrapper(expected),
    "Failed the test `{}`",
    test_name
  );
}

/// A hack to allow the use of `$` in nested macro declarations.
/// Source: https://github.com/rust-lang/rust/issues/35853#issuecomment-415993963.
#[macro_export]
macro_rules! with_dollar_sign {
    ($($body:tt)*) => {
        macro_rules! __with_dollar_sign { $($body)* }
        __with_dollar_sign!($);
    }
}

/// A macro that crates two macros for declaring tests: `test_ok!(test_name)` and `test_err!(test_name)`.
/// The user needs to supply four arguments:
//
/// 1. $crate_root - a static variable pointing at the crate root or another root directory
/// 2. $tests_dir - a static variable with the path to the directory with the tests relative to $crate_root
/// 3. $ok_pipeline - a function with the signature
/// ```rust,ignore
///         Fn(Cow<'a, str>, FileId, &mut VesFileDatabase<'a>) -> Result<String, ErrCtx>
/// ```
/// 4. $err_pipeline - a function with the signature
/// ```rust,ignore
///     Fn(Cow<'a, str>, FileId, &mut VesFileDatabase<'a>) -> Result<T, ErrCtx>,
/// ```
#[macro_export]
macro_rules! make_test_macros {
    (eq => $crate_root:ident, $tests_dir:ident, $f:expr) => {
        $crate::make_test_macros!(eq => $crate_root, $tests_dir, $f, |o: String| o);
    };
    (eq => $crate_root:ident, $tests_dir:ident, $f:expr, $output_preprocessor:expr) => {
        $crate::lazy_static! {
            static ref __TESTS_DIR: std::path::PathBuf
                = std::path::PathBuf::from($crate_root).join($tests_dir.replace('/', &std::path::MAIN_SEPARATOR.to_string()[..]));
        }

        $crate::with_dollar_sign! {
            ($d:tt) => {
                #[allow(unused_macros)]
                macro_rules! test_eq {
                    ($test_name:ident $d( $attr:ident ),*) => {
                        $d(#[$d attr])*
                        #[test]
                        fn $test_name() {
                            let (source, output, _kind) = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            $crate::test_eq(stringify!($test_name), source, $output_preprocessor(output), $f);
                        }
                    };
                }
            };
        }
    };
    ($crate_root:ident, $tests_dir:ident, $ok_pipeline:expr, $err_pipeline:expr) => {
        $crate::make_test_macros!($crate_root, $tests_dir, $ok_pipeline, $err_pipeline, |o: String| o);
    };
    ($crate_root:ident, $tests_dir:ident, $ok_pipeline:expr, $err_pipeline:expr, $output_preprocessor:expr) => {
        $crate::lazy_static! {
            static ref __TESTS_DIR: std::path::PathBuf
                = std::path::PathBuf::from($crate_root).join($tests_dir);
        }

        $crate::with_dollar_sign! {
            ($d:tt) => {
                macro_rules! test_ok {
                    ($test_name:ident $d( $d attr:ident ),*) => {
                        $d(#[$d attr])*
                        #[test]
                        fn $test_name() {
                            let (source, output, _kind) = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            $crate::test_ok(stringify!($test_name), source, $output_preprocessor(output), $ok_pipeline);
                        }
                    };
                }

                macro_rules! test_err {
                    ($test_name:ident $d( $d attr:ident ),*) => {
                        $d (#[$d attr])*
                        #[test]
                        fn $test_name() {
                            let (source, output, _kind) = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            $crate::test_err(stringify!($test_name), source, output, $err_pipeline);
                        }
                    };
                }

                macro_rules! test_auto {
                    ($test_name:ident $d( $d attr:ident ),*) => {
                        $d (#[$d attr])*
                        #[test]
                        fn $test_name() {
                            let (source, output, kind) = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            match kind {
                                $crate::OutputKind::OkOrEq => $crate::test_ok(stringify!($test_name), source, $output_preprocessor(output), $ok_pipeline),
                                $crate::OutputKind::Err => $crate::test_err(stringify!($test_name), source, output, $err_pipeline),
                            }
                        }
                    };
                }
            }
        }
    };
}

/// A wrapper that exposes the given object's [`Display`] impl as [`Debug`].
#[derive(Clone, PartialEq, Eq)]
pub struct DisplayAsDebugWrapper<T>(T);

impl<T> std::fmt::Debug for DisplayAsDebugWrapper<T>
where
  T: std::fmt::Display,
{
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

impl<T> std::ops::Deref for DisplayAsDebugWrapper<T> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.0
  }
}
