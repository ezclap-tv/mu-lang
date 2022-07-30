use std::os::unix::process::CommandExt;
use std::process::Command;

use clap::Parser;
use colored::*;

/// Cargo test wrapper for running mu tests.
#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
  /// The name of the package that should be tested.
  #[clap(value_parser)]
  package: Option<String>,

  /// A single test name or a list of comma separated test names that should be over-written.
  #[clap(
    short = 'w',
    long = "write",
    min_values = 0,
    name = "TEST_NAME",
    value_parser
  )]
  tests_to_overwrite: Option<Vec<String>>,

  /// Disable pretty assertions.
  #[clap(short, long)]
  plain: bool,

  /// Forces the CLI to overwrite *all* snapshots.
  #[clap(long, requires = "TEST_NAME")]
  force: bool,

  /// Any extra arguments that should be forwarded to `cargo test`.
  #[clap(raw = true)]
  cargo_test_args: Vec<String>,
}

fn main() {
  let mut args = Args::parse();

  if args.force && args.tests_to_overwrite.as_ref().unwrap().is_empty() {
    args
      .tests_to_overwrite
      .as_mut()
      .unwrap()
      .push(String::from("*"));
  }

  // Normalize the test names
  args.tests_to_overwrite = args.tests_to_overwrite.map(|tests| {
    tests
      .iter()
      .flat_map(|t| t.split(',').map(str::trim).map(ToOwned::to_owned))
      .collect()
  });

  let has_globs = args
    .tests_to_overwrite
    .as_ref()
    .map(|v| v.iter().any(|x| x.contains('*')))
    .unwrap_or(false);

  if !args.force && has_globs {
    eprintln!("{}", "Error: Globs can't be used without --force".red());
    eprintln!(
      "  * Example: use `{}` to overwrite every single snapshot",
      "--force -w".yellow()
    );
    eprintln!(
      "  * Example: use `{}` to overwrite all snapshots ending with `_lexing`",
      "--force -w \"*_lexing\"".yellow()
    );
    std::process::exit(1);
  }

  if let Some(&[]) = args.tests_to_overwrite.as_ref().map(|v| &v[..]) {
    eprintln!("{}", "Error: --write supplied without a test name".red());
    std::process::exit(1);
  }

  if args.force && !has_globs {
    eprintln!(
      "{}",
      "Warning: --force doesn't do anything unless a glob pattern is provided".yellow()
    )
  }

  // ${ENV_PLAIN_ASSERT}=? ${ENV_WRITE_SNAPSHOTS}=? cargo test {--package ?} -- {...}
  let mut command = Command::new("cargo");
  command.env(
    mu_testing::ENV_PLAIN_ASSERT,
    if args.plain { "1" } else { "0" },
  );

  if let Some(name_string) = args.tests_to_overwrite.as_ref().map(|v| v.join(",")) {
    if !name_string.is_empty() {
      command.env(mu_testing::ENV_WRITE_SNAPSHOTS, name_string);
    }
  }

  command.arg("test");

  if let Some(name) = args.package {
    command.arg("--package");
    command.arg(name);
  }

  command.args(args.cargo_test_args);

  // NOTE: this essentially spawns a new process, and only returns if that failed.
  #[allow(irrefutable_let_patterns)]
  if let err @ std::io::Error { .. } = command.exec() {
    eprintln!("Error: Failed to execute {:?}", command);
    eprintln!("Error: {}", err);
  }
}
