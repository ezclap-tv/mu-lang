<p align="center">
  <img
    alt="The greek lowercase letter mu inside of a gear shape"
    src="./assets/logo.svg"
    height="192px"
  >
</p>

# Mu

Mu is a statically typed, interpreted, embeddable programming language.

⚠ Currently heavily WIP ⚠

### Project structure

```
crates/
├─ diagnosis
├─ span
├─ syntax
```

- [`diagnosis`](./crates/diagnosis/) - error reporting
- [`span`](./crates/span/) - code spans
- [`syntax`](./crates/syntax/) - lexer and parser

### Fuzzing

1. Install cargo-fuzz with `cargo install --force cargo-fuzz`
2. List the available fuzz targets using `just fuzz-targets` and pick one.
3. Fuzz the picked target using `just fuzz <target>`. The fuzzer will run indefinitely, until it detects a crash or the program timeouts or runs out of memory. To stop the fuzzer, press Ctrl+C. Example:

   ```bash
   $ just fuzz lexer-from-bytes
   INFO: Running with entropic power schedule (0xFF, 100).
   INFO: Seed: 717794509
   INFO: Loaded 1 modules   (36225 inline 8-bit counters): 36225 [0x559cf4818810, 0x559cf4821591),
   INFO: Loaded 1 PC tables (36225 PCs): 36225 [0x559cf4821598,0x559cf48aeda8),
   INFO: -fork=6: fuzzing in separate process(s)
   ...
   ^C
   ==2686751== libFuzzer: a child was interrupted; exiting

   $ just fuzz parser-from-bytes
   [FUZZ] Reproduce this crash with:
   just repro parser /path/to/mu/fuzz/libfuzzer/artifacts/parser_from_bytes/crash-25f28287a3ad257fc30e266f12516a66c0ee3b1b --print-input
   ```

4. Reproduce a crash with `just repro <lexer | parser> <file>]`. Example:

   ```bash
   $ just repro parser /<...>/crash-25f28287a3ad257fc30e266f12516a66c0ee3b1b --print-input
   INPUT:`
   (((((((((0(((((((((((((((((((((((((((((((2((((((((((( (((((((((((((((((((((((((	((((((((((((8((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((	((((((((((((8((((((((((((((((((($((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((	((((((((((((8((((((((((((((((((($(((((((((((((((((((((((((((((((((((((((((((((((((((((((	(((((((((((((((((((((((((((	((((((((((((8((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((	((((((((((((8((((((((((((((((((($((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((	((((((((((((8((((((((((((((((((($(((((((((((((((((((((((((((((((((((((((((((((((((((((((	((((((((((((9((((((((411;n;1;n888844488883-311444448;2|||j|;n|||j|;n;1||88883-3((((((((((9((((((((411;n;1;n888844488883-311444448;2|||j|;n|||j|;n;1||88883-3114444411;n;1;n8888444888o888(((((((((((((((((((((((4
   `
   [REPRO] Reproducing Parser with /<...>/crash-25f28287a3ad257fc30e266f12516a66c0ee3b1b

   thread 'main' has overflowed its stack
   fatal runtime error: stack overflow

   $ just repro parser fuzz/afl/suite/errors.mu
   [REPRO] Reproducing Parser with fuzz/afl/suite/errors.mu
   [REPRO] Gracefully parsed the input module with errors:
   [
       NotImplemented(
           "class declarations",
       ),
       NotImplemented(
           "class declarations",
       ),
       NotImplemented(
           "class declarations",
       ),
       NotImplemented(
           "function declarations",
       ),
       Expected(
           Semicolon,
           Span {
               start: 74,
               end: 75,
           },
       ),
       Expected(
           Semicolon,
           Span {
               start: 142,
               end: 143,
           },
       ),
   ]
   ```
