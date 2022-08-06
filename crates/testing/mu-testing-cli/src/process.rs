//! Code in this module is taken from https://github.com/rust-lang/cargo/tree/master/crates/cargo-util
//!
//! Licensed under either of
//! - Apache License, Version 2.0 (https://github.com/rust-lang/cargo/blob/40d71af13ac04f5fda99902112303df49cfc8ec5/LICENSE-APACHE)
//! - MIT license (https://github.com/rust-lang/cargo/blob/40d71af13ac04f5fda99902112303df49cfc8ec5/LICENSE-MIT)

#![allow(dead_code)]

use std::collections::BTreeMap;
use std::ffi::{OsStr, OsString};
use std::iter::once;
use std::path::Path;
use std::process::{Command, ExitStatus, Output};
use std::{env, fmt, str};

use anyhow::{Context, Result};
use shell_escape::escape;

/// A builder object for an external process, similar to [`std::process::Command`].
#[derive(Clone, Debug)]
pub struct ProcessBuilder {
  /// The program to execute.
  program: OsString,
  /// A list of arguments to pass to the program.
  args: Vec<OsString>,
  /// Any environment variables that should be set for the program.
  env: BTreeMap<String, Option<OsString>>,
  /// The directory to run the program from.
  cwd: Option<OsString>,
  /// `true` to include environment variable in display.
  display_env_vars: bool,
}

impl fmt::Display for ProcessBuilder {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "`")?;

    if self.display_env_vars {
      for (key, val) in self.env.iter() {
        if let Some(val) = val {
          let val = escape(val.to_string_lossy());
          if cfg!(windows) {
            write!(f, "set {}={}&& ", key, val)?;
          } else {
            write!(f, "{}={} ", key, val)?;
          }
        }
      }
    }

    write!(f, "{}", self.program.to_string_lossy())?;

    for arg in &self.args {
      write!(f, " {}", escape(arg.to_string_lossy()))?;
    }

    write!(f, "`")
  }
}

impl ProcessBuilder {
  /// Creates a new [`ProcessBuilder`] with the given executable path.
  pub fn new<T: AsRef<OsStr>>(cmd: T) -> ProcessBuilder {
    ProcessBuilder {
      program: cmd.as_ref().to_os_string(),
      args: Vec::new(),
      cwd: None,
      env: BTreeMap::new(),
      display_env_vars: false,
    }
  }

  /// (chainable) Adds `arg` to the args list.
  pub fn arg<T: AsRef<OsStr>>(&mut self, arg: T) -> &mut ProcessBuilder {
    self.args.push(arg.as_ref().to_os_string());
    self
  }

  /// (chainable) Adds multiple `args` to the args list.
  pub fn args<T: AsRef<OsStr>>(&mut self, args: &[T]) -> &mut ProcessBuilder {
    self
      .args
      .extend(args.iter().map(|t| t.as_ref().to_os_string()));
    self
  }

  /// (chainable) Sets the current working directory of the process.
  pub fn cwd<T: AsRef<OsStr>>(&mut self, path: T) -> &mut ProcessBuilder {
    self.cwd = Some(path.as_ref().to_os_string());
    self
  }

  /// (chainable) Sets an environment variable for the process.
  pub fn env<T: AsRef<OsStr>>(&mut self, key: &str, val: T) -> &mut ProcessBuilder {
    self
      .env
      .insert(key.to_string(), Some(val.as_ref().to_os_string()));
    self
  }

  /// (chainable) Unsets an environment variable for the process.
  pub fn env_remove(&mut self, key: &str) -> &mut ProcessBuilder {
    self.env.insert(key.to_string(), None);
    self
  }

  /// Gets the executable name.
  pub fn get_program(&self) -> &OsString {
    &self.program
  }

  /// Gets the program arguments.
  pub fn get_args(&self) -> &[OsString] {
    &self.args
  }

  /// Gets the current working directory for the process.
  pub fn get_cwd(&self) -> Option<&Path> {
    self.cwd.as_ref().map(Path::new)
  }

  /// Gets an environment variable as the process will see it (will inherit from environment
  /// unless explicitally unset).
  pub fn get_env(&self, var: &str) -> Option<OsString> {
    self
      .env
      .get(var)
      .cloned()
      .or_else(|| Some(env::var_os(var)))
      .and_then(|s| s)
  }

  /// Gets all environment variables explicitly set or unset for the process (not inherited
  /// vars).
  pub fn get_envs(&self) -> &BTreeMap<String, Option<OsString>> {
    &self.env
  }

  /// Enables environment variable display.
  pub fn display_env_vars(&mut self) -> &mut Self {
    self.display_env_vars = true;
    self
  }

  /// Runs the process, waiting for completion, and mapping non-success exit codes to an error.
  pub fn exec(&self) -> Result<()> {
    let mut command = self.build_command();
    let exit = command.status().with_context(|| {
      ProcessError::new(&format!("could not execute process {}", self), None, None)
    })?;

    if exit.success() {
      Ok(())
    } else {
      Err(
        ProcessError::new(
          &format!("process didn't exit successfully: {}", self),
          Some(exit),
          None,
        )
        .into(),
      )
    }
  }

  /// Replaces the current process with the target process.
  ///
  /// On Unix, this executes the process using the Unix syscall `execvp`, which will block
  /// this process, and will only return if there is an error.
  ///
  /// On Windows this isn't technically possible. Instead we emulate it to the best of our
  /// ability. One aspect we fix here is that we specify a handler for the Ctrl-C handler.
  /// In doing so (and by effectively ignoring it) we should emulate proxying Ctrl-C
  /// handling to the application at hand, which will either terminate or handle it itself.
  /// According to Microsoft's documentation at
  /// <https://docs.microsoft.com/en-us/windows/console/ctrl-c-and-ctrl-break-signals>.
  /// the Ctrl-C signal is sent to all processes attached to a terminal, which should
  /// include our child process. If the child terminates then we'll reap them in Cargo
  /// pretty quickly, and if the child handles the signal then we won't terminate
  /// (and we shouldn't!) until the process itself later exits.
  pub fn exec_replace(&self) -> Result<()> {
    imp::exec_replace(self)
  }

  /// Executes the process, returning the stdio output, or an error if non-zero exit status.
  pub fn exec_with_output(&self) -> Result<Output> {
    let mut command = self.build_command();

    let output = command.output().with_context(|| {
      ProcessError::new(&format!("could not execute process {}", self), None, None)
    })?;

    if output.status.success() {
      Ok(output)
    } else {
      Err(
        ProcessError::new(
          &format!("process didn't exit successfully: {}", self),
          Some(output.status),
          Some(&output),
        )
        .into(),
      )
    }
  }

  /// Converts `ProcessBuilder` into a `std::process::Command`
  pub fn build_command(&self) -> Command {
    let mut command = Command::new(&self.program);
    if let Some(cwd) = self.get_cwd() {
      command.current_dir(cwd);
    }
    for arg in &self.args {
      command.arg(arg);
    }
    for (k, v) in &self.env {
      match *v {
        Some(ref v) => {
          command.env(k, v);
        }
        None => {
          command.env_remove(k);
        }
      }
    }
    command
  }

  /// Wraps an existing command with the provided wrapper, if it is present and valid.
  ///
  /// # Examples
  ///
  /// ```rust
  /// use cargo_util::ProcessBuilder;
  /// // Running this would execute `rustc`
  /// let cmd = ProcessBuilder::new("rustc");
  ///
  /// // Running this will execute `sccache rustc`
  /// let cmd = cmd.wrapped(Some("sccache"));
  /// ```
  pub fn wrapped(mut self, wrapper: Option<impl AsRef<OsStr>>) -> Self {
    let wrapper = if let Some(wrapper) = wrapper.as_ref() {
      wrapper.as_ref()
    } else {
      return self;
    };

    if wrapper.is_empty() {
      return self;
    }

    let args = once(self.program).chain(self.args.into_iter()).collect();

    self.program = wrapper.to_os_string();
    self.args = args;

    self
  }
}

#[cfg(unix)]
mod imp {
  use std::os::unix::process::CommandExt;

  use anyhow::Result;

  use super::{ProcessBuilder, ProcessError};

  pub fn exec_replace(process_builder: &ProcessBuilder) -> Result<()> {
    let mut command = process_builder.build_command();
    let error = command.exec();
    Err(anyhow::Error::from(error).context(ProcessError::new(
      &format!("could not execute process {}", process_builder),
      None,
      None,
    )))
  }
}

#[cfg(windows)]
mod imp {
  use anyhow::Result;
  use winapi::shared::minwindef::{BOOL, DWORD, FALSE, TRUE};
  use winapi::um::consoleapi::SetConsoleCtrlHandler;

  use super::{ProcessBuilder, ProcessError};

  unsafe extern "system" fn ctrlc_handler(_: DWORD) -> BOOL {
    // Do nothing; let the child process handle it.
    TRUE
  }

  pub fn exec_replace(process_builder: &ProcessBuilder) -> Result<()> {
    unsafe {
      if SetConsoleCtrlHandler(Some(ctrlc_handler), TRUE) == FALSE {
        return Err(ProcessError::new("Could not set Ctrl-C handler.", None, None).into());
      }
    }

    // Just execute the process as normal.
    process_builder.exec()
  }
}

#[derive(Debug)]
pub struct ProcessError {
  /// A detailed description to show to the user why the process failed.
  pub desc: String,

  /// The exit status of the process.
  ///
  /// This can be `None` if the process failed to launch (like process not
  /// found) or if the exit status wasn't a code but was instead something
  /// like termination via a signal.
  pub code: Option<i32>,

  /// The stdout from the process.
  ///
  /// This can be `None` if the process failed to launch, or the output was
  /// not captured.
  pub stdout: Option<Vec<u8>>,

  /// The stderr from the process.
  ///
  /// This can be `None` if the process failed to launch, or the output was
  /// not captured.
  pub stderr: Option<Vec<u8>>,
}

impl fmt::Display for ProcessError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    self.desc.fmt(f)
  }
}

impl std::error::Error for ProcessError {}

impl ProcessError {
  /// Creates a new [`ProcessError`].
  ///
  /// * `status` can be `None` if the process did not launch.
  /// * `output` can be `None` if the process did not launch, or output was not captured.
  pub fn new(msg: &str, status: Option<ExitStatus>, output: Option<&Output>) -> ProcessError {
    let exit = match status {
      Some(s) => exit_status_to_string(s),
      None => "never executed".to_string(),
    };

    Self::new_raw(
      msg,
      status.and_then(|s| s.code()),
      &exit,
      output.map(|s| s.stdout.as_slice()),
      output.map(|s| s.stderr.as_slice()),
    )
  }

  /// Creates a new [`ProcessError`] with the raw output data.
  ///
  /// * `code` can be `None` for situations like being killed by a signal on unix.
  pub fn new_raw(
    msg: &str,
    code: Option<i32>,
    status: &str,
    stdout: Option<&[u8]>,
    stderr: Option<&[u8]>,
  ) -> ProcessError {
    let mut desc = format!("{} ({})", msg, status);

    if let Some(out) = stdout {
      match str::from_utf8(out) {
        Ok(s) if !s.trim().is_empty() => {
          desc.push_str("\n--- stdout\n");
          desc.push_str(s);
        }
        Ok(..) | Err(..) => {}
      }
    }
    if let Some(out) = stderr {
      match str::from_utf8(out) {
        Ok(s) if !s.trim().is_empty() => {
          desc.push_str("\n--- stderr\n");
          desc.push_str(s);
        }
        Ok(..) | Err(..) => {}
      }
    }

    ProcessError {
      desc,
      code,
      stdout: stdout.map(|s| s.to_vec()),
      stderr: stderr.map(|s| s.to_vec()),
    }
  }
}

/// Converts an [`ExitStatus`]  to a human-readable string suitable for
/// displaying to a user.
pub fn exit_status_to_string(status: ExitStatus) -> String {
  return status_to_string(status);

  #[cfg(unix)]
  fn status_to_string(status: ExitStatus) -> String {
    use std::os::unix::process::*;

    if let Some(signal) = status.signal() {
      let name = match signal as libc::c_int {
        libc::SIGABRT => ", SIGABRT: process abort signal",
        libc::SIGALRM => ", SIGALRM: alarm clock",
        libc::SIGFPE => ", SIGFPE: erroneous arithmetic operation",
        libc::SIGHUP => ", SIGHUP: hangup",
        libc::SIGILL => ", SIGILL: illegal instruction",
        libc::SIGINT => ", SIGINT: terminal interrupt signal",
        libc::SIGKILL => ", SIGKILL: kill",
        libc::SIGPIPE => ", SIGPIPE: write on a pipe with no one to read",
        libc::SIGQUIT => ", SIGQUIT: terminal quit signal",
        libc::SIGSEGV => ", SIGSEGV: invalid memory reference",
        libc::SIGTERM => ", SIGTERM: termination signal",
        libc::SIGBUS => ", SIGBUS: access to undefined memory",
        #[cfg(not(target_os = "haiku"))]
        libc::SIGSYS => ", SIGSYS: bad system call",
        libc::SIGTRAP => ", SIGTRAP: trace/breakpoint trap",
        _ => "",
      };
      format!("signal: {}{}", signal, name)
    } else {
      status.to_string()
    }
  }

  #[cfg(windows)]
  fn status_to_string(status: ExitStatus) -> String {
    use winapi::shared::minwindef::DWORD;
    use winapi::um::winnt::*;

    let mut base = status.to_string();
    let extra = match status.code().unwrap() as DWORD {
      STATUS_ACCESS_VIOLATION => "STATUS_ACCESS_VIOLATION",
      STATUS_IN_PAGE_ERROR => "STATUS_IN_PAGE_ERROR",
      STATUS_INVALID_HANDLE => "STATUS_INVALID_HANDLE",
      STATUS_INVALID_PARAMETER => "STATUS_INVALID_PARAMETER",
      STATUS_NO_MEMORY => "STATUS_NO_MEMORY",
      STATUS_ILLEGAL_INSTRUCTION => "STATUS_ILLEGAL_INSTRUCTION",
      STATUS_NONCONTINUABLE_EXCEPTION => "STATUS_NONCONTINUABLE_EXCEPTION",
      STATUS_INVALID_DISPOSITION => "STATUS_INVALID_DISPOSITION",
      STATUS_ARRAY_BOUNDS_EXCEEDED => "STATUS_ARRAY_BOUNDS_EXCEEDED",
      STATUS_FLOAT_DENORMAL_OPERAND => "STATUS_FLOAT_DENORMAL_OPERAND",
      STATUS_FLOAT_DIVIDE_BY_ZERO => "STATUS_FLOAT_DIVIDE_BY_ZERO",
      STATUS_FLOAT_INEXACT_RESULT => "STATUS_FLOAT_INEXACT_RESULT",
      STATUS_FLOAT_INVALID_OPERATION => "STATUS_FLOAT_INVALID_OPERATION",
      STATUS_FLOAT_OVERFLOW => "STATUS_FLOAT_OVERFLOW",
      STATUS_FLOAT_STACK_CHECK => "STATUS_FLOAT_STACK_CHECK",
      STATUS_FLOAT_UNDERFLOW => "STATUS_FLOAT_UNDERFLOW",
      STATUS_INTEGER_DIVIDE_BY_ZERO => "STATUS_INTEGER_DIVIDE_BY_ZERO",
      STATUS_INTEGER_OVERFLOW => "STATUS_INTEGER_OVERFLOW",
      STATUS_PRIVILEGED_INSTRUCTION => "STATUS_PRIVILEGED_INSTRUCTION",
      STATUS_STACK_OVERFLOW => "STATUS_STACK_OVERFLOW",
      STATUS_DLL_NOT_FOUND => "STATUS_DLL_NOT_FOUND",
      STATUS_ORDINAL_NOT_FOUND => "STATUS_ORDINAL_NOT_FOUND",
      STATUS_ENTRYPOINT_NOT_FOUND => "STATUS_ENTRYPOINT_NOT_FOUND",
      STATUS_CONTROL_C_EXIT => "STATUS_CONTROL_C_EXIT",
      STATUS_DLL_INIT_FAILED => "STATUS_DLL_INIT_FAILED",
      STATUS_FLOAT_MULTIPLE_FAULTS => "STATUS_FLOAT_MULTIPLE_FAULTS",
      STATUS_FLOAT_MULTIPLE_TRAPS => "STATUS_FLOAT_MULTIPLE_TRAPS",
      STATUS_REG_NAT_CONSUMPTION => "STATUS_REG_NAT_CONSUMPTION",
      STATUS_HEAP_CORRUPTION => "STATUS_HEAP_CORRUPTION",
      STATUS_STACK_BUFFER_OVERRUN => "STATUS_STACK_BUFFER_OVERRUN",
      STATUS_ASSERTION_FAILURE => "STATUS_ASSERTION_FAILURE",
      _ => return base,
    };
    base.push_str(", ");
    base.push_str(extra);
    base
  }
}

/// Returns `true` if the given process exit code is something a normal
/// process would exit with.
///
/// This helps differentiate from abnormal termination codes, such as
/// segmentation faults or signals.
pub fn is_simple_exit_code(code: i32) -> bool {
  // Typical unix exit codes are 0 to 127.
  // Windows doesn't have anything "typical", and is a
  // 32-bit number (which appears signed here, but is really
  // unsigned). However, most of the interesting NTSTATUS
  // codes are very large. This is just a rough
  // approximation of which codes are "normal" and which
  // ones are abnormal termination.
  (0..=127).contains(&code)
}
