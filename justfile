set windows-shell := ["pwsh.exe", "-NoLogo", "-Command"]

test *ARGS:
  cargo run --quiet --release --bin mu-testing-cli -- {{ARGS}}
