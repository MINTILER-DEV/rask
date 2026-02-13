# Rask

Rask is a scripting language prototype focused on predictability and zero-setup workflows.

## Current Status

Phase 1 scaffolding is in place:
- Rust project structure
- Lexer for core tokens (numbers, strings, identifiers, operators, punctuation, comments)
- Recursive-descent parser for expressions, variable declarations, and print statements
- REPL parser mode

Phase 2 foundations are implemented:
- Type model with nullable (`T?`), unions (`A | B`), generics (`List<T>`, `Map<K,V>`), and function types (`(A, B) -> R`)
- Type annotation parser and assignability rules
- Inference/checking pass for current AST
- REPL and CLI now run type checking after parse
- Phase 2 syntax coverage: `def`/`return`, `value or default`, `or return err`, `?.` optional chaining, postfix panic unwrap `!`, `match`, and destructuring patterns

Phase 3 foundations are implemented:
- Runtime interpreter for statements, expressions, functions, and module calls
- List/map literals and indexing
- List comprehensions (`[expr for item in iterable if condition]`)
- `use` imports for std modules (`use std.math as m`)
- String methods: `trim`, `split`, `replace`, `uppercase`, `lowercase`
- List methods: `push`, `pop`, `map`, `filter`, `reduce`, `sort`, `sorted`
- Map methods: `get`, `set`, `has`, `keys`, `values`
- Built-in modules: `math`, `fs`, `json`
- Grapheme-aware string length/index behavior
- `Path` runtime type and `std.path`

Phase 4 started:
- Permission model and CLI flags: `--allow-read`, `--allow-write`, `--allow-net`, `--allow-env`, `--allow-all`
- Runtime permission checks for filesystem and environment access

## Run (when Rust is installed)

```bash
cargo test
cargo run
```
