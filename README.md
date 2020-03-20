# monkers 🐒 + 🦀

[![Build Status](https://travis-ci.com/poteto/monkers.svg?branch=master)](https://travis-ci.com/poteto/monkers)

Short for `monkey-rs`. An implementation of [monkeylang](https://monkeylang.org/). Previously, I implemented the interpreter in [TypeScript](https://github.com/poteto/boba-js). I am re-implementing the interpreter and later compiler in Rust as a learning exercise.

## REPL

Start the REPL by running `cargo run`, then entering some Monkey:

```
🐒 >> let add = fn(x, y) { x + y; };
> Let
> Identifier("add")
> Assign
> Function
> Lparen
> Identifier("x")
> Comma
> Identifier("y")
> Rparen
> Lbrace
> Identifier("x")
> Plus
> Identifier("y")
> Semicolon
> Rbrace
> Semicolon
```

Command history is saved in `history.txt`.

## Developing

### Helpful crates

#### [`cargo-watch`](https://github.com/passcod/cargo-watch)

`cargo-watch` watches over your Cargo project's source. I use it to run my tests and `cargo check` whenever a file changes:

```
cargo watch -x check -x test
```

Optionally you can append the `RUST_BACKTRACE=1` flag to get backtraces.

### Helpful vscode extensions

-   [rls-vscode](https://github.com/rust-lang/rls-vscode) - RLS-based plugin for VSCode
-   [vscode-cargo](https://github.com/panicbit/vscode-cargo) - Integration with cargo and cargo tools

## Contributing

PRs are welcome! I am not a Rust expert, so I welcome any recommendations on more idiomatic Rust code.
