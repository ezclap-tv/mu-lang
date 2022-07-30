# mu-testing

Simple snapshot testing library and proc macro.


### Usage
The library can be used in two modes:

1. Semi-manual. In this mode, you need to manually define each test with one of the testing macros (`test_eq`, `test_ok`, `test_eq`).

    ```rust
    static CRATE_ROOT: &str = env!("CARGO_MANIFEST_DIR");
    static TESTS_DIR: &str = "tests";

    // This macro generates a number of test macros that can be used to define snapshot tests.
    mu_testing::make_test_macros!(eq => CRATE_ROOT, TESTS_DIR, dump_tokens);

    // Afterwards, you can use the testing macros to define tests.
    // The following line generates a test for the file `$CRATE_ROOT/tests/t1_test_tour_lexing.test`.
    test_eq!(t1_test_tour_lexing); 
    ```

2. Automated. In this mode, you define a test suite a number of 
    
    ```rust
    #[cfg(test)]
    #[mu_testing::mu_test_suite]
    mod suite {
        use super::*;
    
        #[mu_tests = "tests/codegen"]
        mod codegen_tests {
            // Called for every input section that must succeed (everything that precedes the `%output` directive.
            // The `ok_callback` must return an `Ok` value for the snapshot comparison to be performed. Returning an `Err`
            // will lead to a failed test.
            #[ok_callback]
            use super::_impl::compile;
    
            // The `input_callback` is called for every expected output section (everything that follows the %output directive) of every test.
            // It can be used to prepare the output for comparison (like removing comments, for example).
            #[input_callback]
            use super::_impl::strip_comments;
        }

        #[mu_tests = "tests/vm"]
        mod vm_tests {
            #[ok_callback]
            use super::_impl::compile_and_run;
        }

        mod _impl { /* test runner & preprocessing implementation */ }
    }

    #[cfg(test)]
    #[mu_testing::mu_test_suite]
    mod tests {
        #[mu_tests = "tests"]
        mod parser {
            #[ok_callback]
            use super::parse as parse_ok;

            // If you specify an `err_callback` in addition to the mandatory `ok_callback`, the macro will call them
            // depending on the kind of the `%output` directive:
            // * For the `%output`, `%output ok`, and `%output eq` directives, `ok_callback` will be called.
            // * For the `%output err` directives, `err_callback` will be called.
            #[err_callback]
            use super::parse as parse_err;
        }
        
        fn parse(src: std::borrow::Cow<'a, str>) -> Result<String, String> { ... } 
    }
    ```


### Configuration

| Name                      | Default    | Description                                                              |
| ----                      | -------    | -----------                                                              |
| `MU_TEST_PLAIN_ASSERT`    | 0          | If set to `1`, disables pretty assertions.                               |
| `MU_TEST_WRITE_SNAPSHOTS` | None       | Accepts a comma-separated list of test names that should be overwritten. Supports basic prefix and postfix globs. |

Examples:

```bash
$ MU_TEST_PLAIN_ASSERT=1 cargo test
$ MU_TEST_WRITE_SNAPSHOTS=t1_test_tour_lexing cargo test -p mu-parser
$ MU_TEST_WRITE_SNAPSHOTS=t1_* cargo test -p mu-parser
$ MU_TEST_WRITE_SNAPSHOTS=*_lexing cargo test -p mu-parser
```
