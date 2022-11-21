# syntax

This crate contains everything required to transform source code into an AST.

- [`lexer`](./src/lexer.rs) transforms source code into a stream of tokens.
- [`parser`](./src/parser.rs) transforms a stream of tokens into a parsed module, or emits a list of syntax errors.
- [`ast`](./src/ast.rs) contains AST types.
