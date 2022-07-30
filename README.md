<p align="center">
  <img
    alt="The greek lowercase letter mu inside of a gear shape"
    src="./assets/logo.svg"
    height="192px"
  >
</p>

# Mu

Mu is a statically typed, interpreted, embeddable programming language.

Currently undergoing design work. See the [tour](./tour.md) for current progress.


## Development
It is recommended to install [Just](https://github.com/casey/just) if you intend to modify the project, as it automates certain commands.

## Testing
At the moment, the mu test suite consists of library unit tests, library snapshot tests, and integration snapshot tests. The snapshot tests use a custom solution, which you can find under [./crates/testing](./crates/testing/README.md)

* Simply run `cargo test` or `just` in order to execute all tests in the project. 

* During development, you may also want to perform additional actions such as updating snapshot tests or disabling pretty assertions., which can be achieved with `just test`:

    ```bash
    $ just test --help
    mu-testing-cli 0.1.0
    Cargo test wrapper for running mu tests
    
    USAGE:
        mu-testing-cli [OPTIONS] [PACKAGE] [-- <CARGO_TEST_ARGS>...]
    
    ARGS:
        <PACKAGE>               The name of the package that should be tested
        <CARGO_TEST_ARGS>...    Any extra arguments that should be forwarded to `cargo test`
    
    OPTIONS:
            --force                     Forces the CLI to overwrite *all* snapshots
        -h, --help                      Print help information
        -p, --plain                     Disable pretty assertions
        -V, --version                   Print version information
        -w, --write [<TEST_NAME>...]    A single test name or a list of comma separated test names that
                                        should be over-written
    ```

    Here are some usage examples:
    ```bash
      $ just test mu-lexer                 # run all tests for mu-lexer
      $ just test -w test1,test2 -w test3  # overwrite test1, test2, and test3
      $ just test --force -w="*_lexing"    # overwrite the snapshot tests that end with _lexing
      $ just test --force -w               # overwrite *all* snapshot tests
      $ just test --plain                  # disable pretty_assertions
      $ just test -- --quiet               # forward arguments to cargo test, in this case it's --quiet
    ```
