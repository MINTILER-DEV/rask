# SCALF

SCALF is a scripting language focused on predictability and zero-setup workflows. No dependency hell, no version fragmentation, secure by default.

## Current Status

Phase 1 foundations are implemented:
- Rust project structure
- Lexer for core tokens (numbers, strings, identifiers, operators, punctuation, comments)
- Recursive-descent parser for expressions and statements
- Control-flow statements: `if/else`, `while`, `for ... in`, C-style `for`
- Function definitions/returns and collection literals/indexing
- REPL read -> lex -> parse -> typecheck -> evaluate loop

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
- Built-in modules: `math`, `fs`, `json`, `path`, `env`, `http`, `time`, `crypto`
- Grapheme-aware string length/index behavior
- `Path` runtime type and `std.path`

Phase 4 foundations are implemented:
- Permission model and CLI flags: `--allow-read`, `--allow-write`, `--allow-net`, `--allow-env`, `--allow-all`, `--prompt-permissions`
- Runtime permission checks for filesystem, environment, and network access
- Optional interactive permission prompt mode
- Built-in `http` module: `get/post/put/delete` with optional headers and timeout
- HTTP response API: `.status`, `.url`, `.headers`, `.text()`, `.json()`
- HTTP calls start immediately and resolve lazily, enabling overlap for independent requests

Phase 5 foundations are implemented:
- URL import syntax: `use "https://example.com/lib.rask@v1" as lib`
- Import cache (content-addressed SHA-256): `$HOME/.rask/cache/modules/<sha>.rask`
- Lockfile generation and verification: `.rask.lock`
- Version pin metadata support via `@version` suffix in URL imports
- Namespace isolation for URL imports (module-bound exports)
- Stdlib organization includes `std.time` and `std.crypto`
- Auto-generated stdlib docs from `stdlib/std/*.rask` doc comments (`rask docs`)

Phase 6 foundations are implemented:
- Runtime diagnostics include error codes, hints, docs links, and stack traces
- Runtime errors now include concrete values in key failure paths (nil member access, assert failures, non-callable values)
- Member/method suggestions (`did you mean ...`) for common lookup failures
- Tooling commands:
  - `rask fmt [--check] <file>`
  - `rask check <file>` (typecheck + lint warnings)
  - `rask test <file>` with built-in `test`/`assert` syntax

Phase 7 foundations are implemented:
- Bytecode VM module with:
  - Stack-based instruction set
  - AST to bytecode compiler for core expressions/statements
  - Bytecode interpreter
  - Constant folding during compilation
- Optional VM execution path: `rask --vm <file>`
- Standalone build command:
  - `rask build <file>`
  - Embeds script source into the output binary
  - Auto-selects minimal compile features per script (e.g. excludes `net` when HTTP/URL imports are unused)
  - Target aliases: `linux-x64`, `windows-x64`, `macos-arm64`
  - Triple passthrough via `--target=<triple>`
  - Output control via `--out=<path>`
- Startup benchmark command with target budget support:
  - `rask startup <file>`
  - `--iterations=<n>`, `--budget-ms=<ms>`, `--no-budget`, `--vm`

Phase 8 partial foundations are implemented:
- Concurrency helpers:
  - `concurrency.await(value)` for pending async values
  - `concurrency.join(list)` to resolve lists of pending values
  - `concurrency.timeout(ms, value)` for bounded wait
  - `concurrency.channel()` with `send/recv/try_recv/recv_timeout/len`
- REPL improvements:
  - Multi-line input when blocks/expressions are incomplete
  - Syntax highlighting utilities and preview command
  - Completion command (`:complete <prefix>`)
  - Persistent history (`~/.rask/repl_history` by default)
  - Evaluation timing output (`:timing on|off`)
  - Inline docs (`:doc <symbol>`)

## Run (when Rust is installed)

```bash
cargo test
cargo run
cargo run -- docs
cargo run -- fmt --check path/to/file.rask
cargo run -- check path/to/file.rask
cargo run -- test path/to/file.rask
cargo run -- --vm path/to/file.rask
cargo run -- build path/to/file.rask --target=linux-x64
cargo run -- startup path/to/file.rask --iterations=25
cargo run
```
