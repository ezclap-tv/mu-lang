use std::path::{Path, PathBuf};
use std::time::SystemTime;

use clap::ValueEnum;

use crate::process::ProcessError;

pub fn run(target: crate::Target, cpus: Option<usize>) -> Result<(), crate::Error> {
  let target_name = target.dir();
  let module = target.module();

  let mut command = crate::process::ProcessBuilder::new("cargo");
  command.cwd(get_cwd());
  command
    .arg("fuzz")
    .arg("run")
    .arg(&target_name)
    .args(&["--fuzz-dir", "."])
    .args(&[
      "-j",
      &cpus.unwrap_or_else(|| num_cpus::get_physical()).to_string(),
    ]);

  // NOTE: Tracking the time like this can technically prevent us from finding the
  // most recent crash, because the system clock may arbitrarily change
  // while the fuzzer is running (for example, due to NTP or the user adjusting
  // their system time).
  let start_time = std::time::SystemTime::now();
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

fn find_most_recent_crash(target_name: &str, start_time: Option<SystemTime>) -> Option<PathBuf> {
  let target_artifacts = get_cwd().join("artifacts").join(target_name);
  if !target_artifacts.exists() || !target_artifacts.is_dir() {
    return None;
  }

  let crashes = std::fs::read_dir(&target_artifacts)
    .ok()?
    .filter_map(Result::ok)
    .filter_map(|entry| {
      entry
        .metadata()
        .and_then(|m| m.modified())
        .ok()
        .map(|m| (entry, m))
    })
    .filter(|(_, last_modified)| {
      start_time
        .as_ref()
        .map(|t| last_modified >= t)
        .unwrap_or(true)
    })
    .max_by_key(|(_, last_modified)| *last_modified);

  crashes.map(|(entry, _)| entry.path())
}

fn get_cwd() -> PathBuf {
  static PKG_DIR: &'static str = env!("CARGO_MANIFEST_DIR");

  let libfuzzer_dir = Path::new(PKG_DIR).parent().map(|p| p.join("libfuzzer"));
  match libfuzzer_dir {
    Some(d) => d,
    None => panic!(
      "Failed to detect the libfuzzer directory relatively to {}",
      PKG_DIR
    ),
  }
}
