use std::borrow::Cow;
use std::io::Write;
use std::path::{Path, PathBuf};

pub use lazy_static::lazy_static;
pub use mu_testing_macro::mu_test_suite;
pub use pretty_assertions;

pub const ENV_PLAIN_ASSERT: &str = "MU_TEST_PLAIN_ASSERT";
pub const ENV_WRITE_SNAPSHOTS: &str = "MU_TEST_WRITE_SNAPSHOTS";

/// The kind of output a test may have.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputKind {
  Ok,
  Eq,
  ImplicitOk,
  Err,
}

#[derive(Debug, PartialEq)]
pub struct TestFile<'a> {
  pub test_name: &'a str,
  pub test_path: PathBuf,
  pub code: String,
  pub expected: String,
  pub output_kind: OutputKind,
}

impl<'a> TestFile<'a> {
  fn check<'b>(&self, actual: &'b str, mut expected: &'b str) {
    let pretty_assertions = std::env::var(ENV_PLAIN_ASSERT)
      .map(|v| v.trim() != "1")
      .unwrap_or(true);

    let write_snapshot = std::env::var(ENV_WRITE_SNAPSHOTS)
      .map(|v| {
        v.split(',').map(str::trim).any(|name| {
          if name.ends_with('*') {
            self
              .test_name
              .starts_with(name.get(..name.len() - 1).unwrap_or(""))
          } else if name.starts_with('*') {
            self.test_name.ends_with(name.get(1..).unwrap_or(""))
          } else {
            name == self.test_name
          }
        })
      })
      .unwrap_or(false);

    if write_snapshot {
      eprintln!("[MU_TEST] Overwriting `{}`", self.test_name);
      write_test_file(self, actual);
      expected = actual;
    }

    if pretty_assertions {
      pretty_assertions::assert_eq!(
        DisplayAsDebugWrapper(actual),
        DisplayAsDebugWrapper(expected),
        "Failed the error test `{}`",
        self.test_name
      );
    } else {
      assert_eq!(
        DisplayAsDebugWrapper(actual),
        DisplayAsDebugWrapper(expected),
        "Failed the error test `{}`",
        self.test_name
      );
    }
  }
}

/// Loads a test file from the given tests directory and returns a tuple of
/// (source code, expected output).
pub fn load_test_file<'a>(test_dir: &Path, name: &'a str) -> TestFile<'a> {
  let path = std::path::PathBuf::from(test_dir).join(format!("{}.test", name));
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
    "ok" => OutputKind::Ok,
    "eq" => OutputKind::Eq,
    "" => OutputKind::ImplicitOk,
    rest => panic!(
      "Unrecognized test type -- `{}`. Expected either `ok`, `err`, or `eq`",
      rest
    ),
  };
  TestFile {
    test_path: path,
    test_name: name,
    code: code.trim().to_owned(),
    expected: expected.trim().to_owned(),
    output_kind,
  }
}

/// Updates the given snapshot test file with `new_content`.
pub fn write_test_file(file: &TestFile<'_>, new_content: &str) {
  let mut f = std::fs::File::options()
    .write(true)
    .truncate(true)
    .open(&file.test_path)
    .expect("Failed to open the file for writing");

  || -> std::io::Result<()> {
    f.write_all(file.code.as_bytes())?;
    f.write_all("\n".as_bytes())?;
    f.write_all("\n".as_bytes())?;
    f.write_all("%output".as_bytes())?;
    f.write_all(
      match file.output_kind {
        OutputKind::Ok => " ok",
        OutputKind::Eq => " eq",
        OutputKind::Err => " err",
        OutputKind::ImplicitOk => "",
      }
      .as_bytes(),
    )?;
    f.write_all("\n".as_bytes())?;
    f.write_all("\n".as_bytes())?;
    f.write_all(new_content.as_bytes())?;
    f.flush()?;
    Ok(())
  }()
  .expect("Failed to write the snapshot");
}

/// Asserts that output of the given closure matches the expected output.
pub fn test_eq<F>(file: TestFile<'_>, f: F)
where
  F: for<'a> Fn(Cow<'a, str>) -> String,
{
  let output = f(std::borrow::Cow::Borrowed(&file.code[..]));
  file.check(output.trim(), &file.expected);
}

// Asserts that output of the given pipeline closure is a list of errors.
/// Compares the diagnostics for the errors to the given expected output.
pub fn test_err<T, F>(file: TestFile<'_>, pipeline: F)
where
  T: std::fmt::Debug,
  F: for<'a> Fn(Cow<'a, str>) -> Result<T, String>,
{
  let src = std::borrow::Cow::Borrowed(&file.code[..]);
  let errors = pipeline(src).expect_err("Test succeeded unexpectedly");
  file.check(errors.trim(), &file.expected);
}

/// Compares the output of the given function and the expected output.
pub fn test_ok<E, F>(file: TestFile<'_>, pipeline: F)
where
  E: std::error::Error,
  F: for<'a> Fn(Cow<'a, str>) -> Result<String, E>,
{
  let src = std::borrow::Cow::Borrowed(&file.code[..]);
  let ok = pipeline(src).expect("Test failed unexpectedly");
  file.check(ok.trim(), &file.expected);
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

/// A macro that crates three macros for declaring tests: `test_ok!(test_name)`, `test_err!(test_name)`, and test_auto!(test_name).
/// The user needs to supply four arguments:
//
/// 1. $crate_root - a static variable pointing at the crate root or another root directory
/// 2. $tests_dir - a static variable with the path to the directory with the tests relative to $crate_root
/// 3. $ok_pipeline - a function with the signature
/// ```rust,ignore
///         Fn(Cow<'a, str>) -> Result<String, String>
/// ```
/// 4. $err_pipeline - a function with the signature
/// ```rust,ignore
///     Fn(Cow<'a, str>) -> Result<T, String>,
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
                            let mut file = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            file.expected = $output_preprocessor(file.expected);
                            $crate::test_eq(file, $f);
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
                            let mut file = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            file.expected = $output_preprocessor(file.expected);
                            $crate::test_ok(file, $ok_pipeline);
                        }
                    };
                }

                macro_rules! test_err {
                    ($test_name:ident $d( $d attr:ident ),*) => {
                        $d (#[$d attr])*
                        #[test]
                        fn $test_name() {
                            let file = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            $crate::test_err(file, $err_pipeline);
                        }
                    };
                }

                macro_rules! test_auto {
                    ($test_name:ident $d( $d attr:ident ),*) => {
                        $d (#[$d attr])*
                        #[test]
                        fn $test_name() {
                            let mut file = $crate::load_test_file(&__TESTS_DIR, stringify!($test_name));
                            match file.kind {
                                $crate::OutputKind::Err => $crate::test_err(file, $err_pipeline),
                                _ => {
                                  file.expected = $output_preprocessor(file.expected);
                                  $crate::test_ok(file, $ok_pipeline)
                                }
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
