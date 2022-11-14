set windows-shell := ["pwsh.exe", "-NoLogo", "-Command"]

# Run the default fuzzer (libfuzz) with the specified target.
@fuzz target *ARGS:
  cargo run --quiet --bin mu-fuzz-cli fuzz {{target}} {{ARGS}}

# Reproduce a crash by re-running it with the specified module.
@repro module path *ARGS:
  RUST_BACKTRACE=1 cargo run --quiet --bin mu-fuzz-cli repro {{module}} {{path}} {{ARGS}}

# Print the available fuzz targets and backends.
@fuzz-targets:
  cargo run --quiet --bin mu-fuzz-cli info --list-backends --list-targets
