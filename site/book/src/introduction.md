# Introduction

Mu is a statically typed, interpreted, embeddable programming language. Both the syntax and type system are heavily inspired by Rust.

The ultimate goal of Mu is to enable scripting in Rust programs, without allowing "obvious" errors, such as undefined variables, to exist. The design of Mu is heavily influenced by this goal. It is not meant to be a standalone, general-purpose programming language. There are already plenty to choose from in that category, Mu is only trying to fill this one niche.

What this means in practice is that the language was designed from the perspective of someone writing small, self-contained scripts. Here are some concrete examples of how that influenced the design of Mu:

- All memory is managed, and all non-primitives live on the heap
- The type system is not as rigorous
- The language has unchecked exceptions to reduce the verbosity of error handling
- It is single-threaded

Note that while these may be viewed as downsides, these design decisions generally result in much less verbose programs at the small scale. They make the language unsuitable for large codebases. Note that we still try to reduce the impact of these design decisions:

- Type system is not as rigorous, but still very expressive. For example, the language has GATs from the very beginning.
- The language has unchecked exceptions, but checked exceptions are planned for a future release. This will add some robustness back to the error handling approach of the language.
- It is single-threaded, but it also has concurrency baked in with `spawn` and the standard `task` module. The VM is built for easy integration with async runtimes such as Tokio.

Hopefully this introduction has provided some insight into the design principles that the language is built upon. The rest of the book focuses on the syntax, semantics, and type system of the language.

