# mu-fuzz-afl

1. Install cargo-afl
    
    ```bash
    $ cargo install afl
    ```

2. Build fuzz target binaries 

   ```bash
   $ cargo afl build
   # Or
   $ cargo afl build --release
   ```

3. Fuzz a binary

   ```bash
   $ cargo afl fuzz -i suite -o artifacts/lexer-from-bytes ./target/release/lexer_from_bytes 
   ```

## PROGRAM ABORT : Pipe at the beginning of 'core_pattern'
You might need to modify your /proc/sys/kernel/core_pattern if you're using a utility like coredumpctl:
```bash
# cat /proc/sys/kernel/core_pattern
|/usr/lib/systemd/systemd-coredump %P %u %g %s %t %c %h
# echo core >/proc/sys/kernel/core_pattern
# cat /proc/sys/kernel/core_pattern
core
```

Don't forget to set it back to whatever value it had after you're done fuzzing:
```bash
# echo '|/usr/lib/systemd/systemd-coredump %P %u %g %s %t %c %h' >/proc/sys/kernel/core_pattern
```