use clap::{Parser, Subcommand, ValueEnum};
use thiserror::Error;

mod libfuzzer;
mod process;
mod reproduce;

#[derive(Debug, Clone, Copy, PartialEq, ValueEnum)]
pub enum Module {
  Lexer,
  Parser,
}

#[derive(Debug, Clone, Copy, PartialEq, ValueEnum)]
pub enum Backend {
  Afl,
  Libfuzzer,
}

#[derive(Debug, Clone, Copy, PartialEq, ValueEnum)]
pub enum Target {
  LexerFromBytes,
  ParserFromBytes,
}

impl Target {
  pub fn option(&self) -> String {
    self
      .to_possible_value()
      .expect("There's no skipped variants, so this expect will never fail.")
      .get_name()
      .to_owned()
  }
  pub fn module(&self) -> Module {
    match self {
      Target::LexerFromBytes => Module::Lexer,
      Target::ParserFromBytes => Module::Parser,
    }
  }
  pub fn dir(&self) -> &'static str {
    match self {
      Target::LexerFromBytes => "lexer_from_bytes",
      Target::ParserFromBytes => "parser_from_bytes",
    }
  }
  pub fn description(&self) -> &'static str {
    match self {
      Target::LexerFromBytes => "Fuzz the lexer with random UTF-8 text",
      Target::ParserFromBytes => "Fuzz the parser with random UTF-8 text",
    }
  }
}

#[derive(Debug, Parser)]
pub struct Args {
  #[command(subcommand)]
  command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
  /// Fuzz a target using a backend choice (afl, libfuzzer).
  Fuzz {
    /// The desired fuzzing target.
    #[arg(value_enum)]
    target: Target,
    /// The fuzzing backend to use.
    #[arg(default_value_t = Backend::Libfuzzer, value_enum, short = 'b', long = "backend")]
    backend: Backend,
    /// The number of CPU cores to use while fuzzing. Only supported by the
    /// libfuzzer backend. Defaults to the number of physical cores.
    #[arg(short = 'j', long = "jobs")]
    parallelism: Option<usize>,
  },
  /// Reproduce a crash.
  Repro {
    /// The module to reproduce with.
    #[arg(value_enum)]
    module: Module,
    /// The input file to use when reproducing.
    input: std::path::PathBuf,
    /// Prints the input file the console.
    #[arg(default_value = "false", short = 'p', long = "print-input")]
    preview: bool,
  },
  /// Print the information about the available fuzzing targets and backends.
  Info {
    /// List the fuzzing targets.
    #[arg(long = "list-targets", default_value_t = true)]
    list_targets: bool,
    /// List the fuzzing backends.
    #[arg(long = "list-backends", default_value_t = true)]
    list_backends: bool,
  },
}

#[derive(Debug, Error)]
pub enum Error {
  #[error("Given input file does not exist: {0}")]
  InputNotFound(std::path::PathBuf),
  #[error("Expected a file, but received a directory: {0}")]
  DirectoryInput(std::path::PathBuf),
  #[error("Failed to read the input file: {0}")]
  BadInput(#[from] std::io::Error),
  #[error("The requested fuzzing backend {0:?} is not available")]
  BackendUnavailable(Backend),
  #[error("Failed to check the backend: {0}")]
  BackendCheckFailed(#[source] crate::process::ProcessError),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let args = Args::parse();

  match args.command {
    Command::Info {
      list_targets,
      list_backends,
    } => {
      print_info(list_targets, list_backends);
    }
    Command::Repro {
      module,
      input,
      preview,
    } => match reproduce(module, input, preview) {
      Ok(()) => (),
      Err(e) => {
        eprintln!("[ERROR] {}", e);
        return Ok(());
      }
    },
    Command::Fuzz {
      target,
      backend: Backend::Libfuzzer,
      parallelism,
    } => {
      check_backend(Backend::Libfuzzer)?;
      libfuzzer::run(target, parallelism)?;
    }
    Command::Fuzz {
      backend: Backend::Afl,
      ..
    } => {
      unimplemented!("Fuzzing with AFL is not implemented yet.")
    }
  }

  Ok(())
}

fn print_info(list_targets: bool, list_backends: bool) {
  if list_backends {
    println!("Fuzzing backends:");
    Backend::value_variants()
      .iter()
      // Temporary excluding, because AFL requires some manual tinkering on Linux
      .filter(|b| **b != Backend::Afl)
      .enumerate()
      .for_each(|(i, v)| {
        let (package, name) = match v {
          Backend::Afl => ("afl", "AFL"),
          Backend::Libfuzzer => ("cargo-fuzz", "libFuzzer"),
        };
        println!(
          "{}. {} ({} via {})",
          i + 1,
          v.to_possible_value()
            .expect("We're don't have any skipped variants, so this will never fail")
            .get_name(),
          name,
          package
        );
      });
  }
  if list_targets {
    println!("Fuzzing targets:");
    Target::value_variants()
      .iter()
      .enumerate()
      .for_each(|(i, target)| {
        println!("{}. {} ({})", i + 1, target.option(), target.description());
      })
  }
}

fn reproduce(module: Module, input: std::path::PathBuf, preview: bool) -> Result<(), Error> {
  if !input.exists() {
    if !input.is_absolute() {
      eprintln!("NOTE: The given input path is not absolute. \
      If you executed this program using just, relative filenames that are not in the workspace root will not be found, \
      as just changes the current directory to that of the justfile. \
      This warning will go away once https://github.com/casey/just/pull/1400 is released.");
    }
    return Err(Error::InputNotFound(input));
  }
  if input.is_dir() {
    return Err(Error::DirectoryInput(input));
  }
  let content = std::fs::read_to_string(&input).map_err(Error::BadInput)?;

  if preview {
    println!("INPUT:`\n{content}\n`");
  }

  eprintln!("[REPRO] Reproducing {:?} with {}", module, input.display());
  match module {
    Module::Lexer => reproduce::lexer(&content),
    Module::Parser => reproduce::parser(&content),
  }

  Ok(())
}

fn check_backend(backend: Backend) -> Result<(), Error> {
  let (cargo_pkg, available) = match backend {
    Backend::Afl => ("afl", check_cargo_binary("afl")?),
    Backend::Libfuzzer => ("cargo-fuzz", check_cargo_binary("fuzz")?),
  };

  if !available {
    eprintln!(
      "Could not detect the fuzzing backend. Install with: cargo install --force {cargo_pkg}"
    );
    return Err(Error::BackendUnavailable(backend));
  }

  Ok(())
}

fn check_cargo_binary(binary: &str) -> Result<bool, Error> {
  match crate::process::ProcessBuilder::new("cargo")
    .arg(binary)
    .arg("--version")
    .exec_with_output()
  {
    Ok(output) => Ok(output.stdout.starts_with("cargo-".as_bytes())),
    Err(e) => {
      let e: crate::process::ProcessError = e.downcast().unwrap();

      let stderr = e.stderr.as_ref().and_then(|v| std::str::from_utf8(v).ok());
      if let Some(stderr) = stderr {
        if stderr.starts_with(&format!("error: no such command: `{binary}`")) {
          return Ok(false);
        }
      }

      Err(Error::BackendCheckFailed(e))
    }
  }
}
