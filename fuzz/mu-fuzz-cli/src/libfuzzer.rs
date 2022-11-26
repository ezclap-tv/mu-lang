use std::path::{Path, PathBuf};
use std::time::SystemTime;

use clap::ValueEnum;

use crate::process::{ProcessBuilder, ProcessError};

pub fn run(target: crate::Target, cpus: Option<usize>) -> Result<(), crate::Error> {
  let target_name = target.dir();
  let module = target.module();
  let cpus = cpus.unwrap_or_else(num_cpus::get_physical);

  let mut command = ProcessBuilder::new("cargo");
  command
    .cwd(get_libfuzzer_dir())
    .arg("fuzz")
    .arg("run")
    .arg(target_name)
    .args(&["--fuzz-dir", "."])
    .args(&["-j", &cpus.to_string()]);

  // NOTE: Tracking the time like this can technically prevent us from finding the
  // most recent crash, because the system clock may arbitrarily change
  // while the fuzzer is running (for example, due to NTP or the user adjusting
  // their system time).
  let start_time = SystemTime::now();
  match command.exec() {
    Ok(()) => {}
    Err(e) => {
      let e: ProcessError = e.downcast().unwrap();
      if cfg!(not(target_os = "windows")) && e.code != Some(1) {
        eprintln!("[FUZZ] Unexpected fuzzer exit: {}", e);
        return Ok(());
      }

      let most_recent_crash = find_most_recent_crash(target_name, Some(start_time));
      if let Some(path) = most_recent_crash {
        let path = try_minify_crash(target_name, &path).unwrap_or(path);

        let _ = clearscreen::clear(); // don't care if this fails
        eprintln!("[FUZZ] Reproduce this crash with:");
        eprintln!(
          "just repro {} {} --print-input",
          module.to_possible_value().unwrap().get_name(),
          path.display()
        );
      } else {
        eprintln!("Failed to detect the crash. Check the output above for more info.")
      }
    }
  }

  Ok(())
}

fn try_minify_crash(target_name: &str, path: &Path) -> Option<PathBuf> {
  let start_time = SystemTime::now();

  let mut command = ProcessBuilder::new("cargo");
  command
    .cwd(get_libfuzzer_dir())
    .arg("fuzz")
    .arg("tmin")
    .arg(target_name)
    .args(&["--fuzz-dir", "."])
    .arg(path);

  if command.exec().is_ok() {
    find_most_recent_crash(target_name, Some(start_time))
  } else {
    None
  }
}

fn find_most_recent_crash(target_name: &str, start_time: Option<SystemTime>) -> Option<PathBuf> {
  let target_artifacts = get_libfuzzer_dir().join("artifacts").join(target_name);
  if !target_artifacts.exists() || !target_artifacts.is_dir() {
    return None;
  }

  let start_time = &start_time.unwrap_or(SystemTime::UNIX_EPOCH);

  let crashes = std::fs::read_dir(&target_artifacts)
    .ok()?
    .filter_map(|entry| {
      let entry = entry.ok()?;
      let metadata = entry.metadata().ok()?;
      let modified = metadata.modified().ok()?;
      Some((entry, modified))
    })
    .filter(|(_, last_modified)| last_modified >= start_time);

  crashes
    .max_by_key(|(_, last_modified)| *last_modified)
    .map(|(entry, _)| entry.path())
}

fn get_libfuzzer_dir() -> PathBuf {
  static MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");

  let Some(project_root) = Path::new(MANIFEST_DIR).parent() else {
    panic!(
      "Failed to detect the libfuzzer directory relative to {}",
      MANIFEST_DIR
    )
  };
  project_root.join("libfuzzer")
}
