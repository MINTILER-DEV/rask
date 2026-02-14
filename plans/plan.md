# SCALF Language Development Plan

**Vision**: A scripting language that actually works. No dependency hell, no version fragmentation, no surprises. Write it once, runs everywhere, secure by default.

**Killer Feature**: "One binary, zero dependencies, secure by default"

## Core Principles

1. **Batteries Included**: Standard library handles 90% of scripting needs
2. **Security First**: Explicit permissions for filesystem, network, environment access
3. **Zero Setup**: Single binary, no installation, no package manager needed for basics
4. **Predictable**: One obvious way to do things, consistent APIs, great error messages
5. **Fast Startup**: Scripts should start in <10ms
6. **Gradual Typing**: Dynamic when prototyping, static when shipping

---

## Implementation Roadmap

### Phase 1: REPL & Core Language (Weeks 1-4)

**Goal**: Build a working REPL that proves the syntax feels good

#### Week 1-2: Lexer & Parser
- [x] Set up Rust project structure
- [x] Implement lexer (tokenization)
  - Numbers (int, float)
  - Strings (with interpolation support)
  - Identifiers & keywords
  - Operators (+, -, *, /, ==, !=, <, >, etc.)
  - Punctuation ({}, [], (), comma, etc.)
- [x] Implement parser (recursive descent)
  - Expressions (binary ops, unary ops, literals)
  - Variable declarations
  - Print statements
- [x] Basic AST definitions
- [x] REPL loop
  - Read input
  - Lex → Parse → Evaluate
  - Print result
  - Handle errors gracefully

**Milestone**: `2 + 2` returns `4` in REPL

#### Week 3-4: Control Flow & Functions
- [x] If/else statements
- [x] While loops
- [x] For loops (both C-style and for-in)
- [x] Function definitions
  - Basic parameter passing
  - Return statements
  - Local scopes
- [x] Basic collections
  - Lists `[1, 2, 3]`
  - Maps `{key: value}`
  - Indexing and iteration

**Milestone**: FizzBuzz works in Rask

---

### Phase 2: Type System (Weeks 5-8)

**Goal**: Gradual typing with excellent inference and null safety

#### Week 5-6: Type Inference
- [x] Design type system
  - Primitives: int, float, string, bool
  - Collections: List<T>, Map<K,V>
  - Functions: (T1, T2) -> R
  - Null: T?
  - Errors: T | Error
- [x] Implement type inference algorithm (current AST coverage)
  - Hindley-Milner or simpler bidirectional
  - Infer variable types from usage
  - Infer function return types
- [x] Type annotations (optional, variable declarations)
  - `x: int = 42`
  - `def func(x: string) -> int`

#### Week 7-8: Null Safety & Error Handling
- [x] Nullable types with `?` (type-level support)
  - `user?.name` optional chaining
  - `value or default` null coalescing
- [x] Error types (type-level support)
  - `Result<T, E>` or similar
  - `or return err` error propagation
  - `or default` for fallback values
  - `!` operator for panic on error
- [x] Pattern matching (match expressions + basic destructuring patterns)
  - Match expressions
  - Destructuring
- [x] Type checking pass
  - Catch type errors before runtime
  - Helpful error messages

**Milestone**: Type system prevents common bugs, errors are clear

---

### Phase 3: Standard Library Basics (Weeks 9-12)

**Goal**: Built-in functionality for common scripting tasks

#### Week 9-10: Core APIs
- [x] String methods (core methods implemented)
  - `trim()`, `split()`, `replace()`, `uppercase()`, `lowercase()`
  - UTF-8 grapheme cluster handling
  - String interpolation: `"Hello {name}"`
- [x] List methods
  - `push()`, `pop()`, `map()`, `filter()`, `reduce()`
  - `sort()` (mutating) vs `sorted()` (new list)
  - List comprehensions: `[x * 2 for x in numbers]`
- [x] Map methods
  - `get()`, `set()`, `has()`, `keys()`, `values()`
- [x] Math module
  - Basic operations, constants (pi, e)
  - `min()`, `max()`, `abs()`, `round()`

#### Week 11-12: File & JSON
- [x] File system API (no permissions yet)
  - `fs.read(path)` - read file as string
  - `fs.write(path, content)` - write file
  - `fs.exists(path)`, `fs.delete(path)`
  - `Path` type for cross-platform paths
- [x] JSON module
  - `json.parse(string)` - parse JSON
  - `json.stringify(value)` - serialize to JSON
  - Pretty printing support

**Milestone**: Can read JSON files and process them

---

### Phase 4: Security & Permissions (Weeks 13-16)

**Goal**: Scripts are sandboxed by default, Deno-style permissions

#### Week 13-14: Permission System
- [x] Design permission model
  - `--allow-read=<path>`
  - `--allow-write=<path>`
  - `--allow-net=<domain>`
  - `--allow-env`
  - `--allow-all` (for trusted scripts)
- [x] Runtime permission checking (fs/env implemented, net hooks added)
  - Check before fs operations
  - Check before network operations
  - Check before env access
- [x] Permission prompts (optional interactive mode)
- [x] Graceful error messages when denied

#### Week 15-16: HTTP Client
- [x] Built-in HTTP module
  - `http.get(url)` - GET request
  - `http.post(url, body)` - POST request
  - `http.put()`, `http.delete()`
  - Response object with `.json()`, `.text()`, `.status`
- [x] Timeout handling
- [x] Basic headers support
- [x] Concurrent requests work automatically (lazy/pending response execution)

**Milestone**: Scripts can fetch data from APIs securely

---

### Phase 5: Package System (Weeks 17-20)

**Goal**: Import from URLs, no central registry, hermetic builds

#### Week 17-18: URL Imports
- [x] Import syntax: `use "https://example.com/lib.rask" as lib`
- [x] Download and cache imports
  - Hash-based caching (content-addressed)
  - Global cache location
  - Version pinning: `@v1.2.3`
- [x] Lockfile generation
  - Auto-generate `.rask.lock`
  - Lock exact hashes/versions
  - Verify on subsequent runs
- [x] Namespace isolation
  - Each import gets own namespace
  - No dependency conflicts

#### Week 19-20: Standard Library Organization
- [x] Organize std lib into modules
  - `std.fs` - filesystem
  - `std.http` - HTTP client
  - `std.json` - JSON parsing
  - `std.path` - path utilities
  - `std.env` - environment variables
  - `std.time` - time/date handling
  - `std.crypto` - hashing, etc.
- [x] Documentation system
  - Docstrings in code
  - Auto-generate docs

**Milestone**: Can import libraries from URLs, reproducible builds

---

### Phase 6: Error Messages & Developer Experience (Weeks 21-24)

**Goal**: Best-in-class error messages and tooling

#### Week 21-22: Error Messages
- [x] Show actual values in errors
  - "Cannot call .name on null, user = null (type: User?)"
- [x] Helpful hints and suggestions
  - Did you mean? suggestions
  - Common fix suggestions
- [x] Stack traces show user code first
  - Collapse framework/stdlib code
  - Clickable file paths with line numbers
- [x] Error codes and documentation links

#### Week 23-24: Tooling
- [x] Auto-formatter: `rask fmt`
  - One style, no configuration
  - Format on save integration
- [x] Linter: `rask check`
  - Catch common mistakes
  - Best practice enforcement
- [x] Built-in testing
  - `test "name" { assert ... }` syntax
  - `rask test` command
  - Test runner with good output

**Milestone**: Errors teach you how to fix them, tools work out of the box

---

### Phase 7: Performance & Compilation (Weeks 25-30)

**Goal**: Fast startup, good runtime performance, binary compilation

#### Week 25-27: Bytecode VM
- [x] Design bytecode format
- [x] Compile AST to bytecode
- [x] Bytecode interpreter
  - Stack-based VM
  - Register-based VM (evaluate which)
- [x] Optimize common patterns
- [x] Fast startup (<10ms target)

#### Week 28-30: Binary Compilation
- [x] `rask build` command
  - Compile script to standalone binary
  - Embed runtime
  - Cross-platform support
- [x] Cross-compilation
  - `--target linux-x64`
  - `--target windows-x64`
  - `--target macos-arm64`
- [x] Optimize binary size

**Milestone**: Scripts start instantly, can be distributed as binaries

---

### Phase 8: Advanced Features (Weeks 31-36)

**Goal**: Features that make Rask delightful

#### Week 31-32: Concurrency
- [x] Async runtime
  - Automatic concurrency for independent operations
  - Await/join operations when values needed
- [x] Timeout support
  - `timeout(5s) { ... }` blocks
  - Automatic cancellation
- [x] Channels/communication (if needed)

#### Week 33-34: LSP (Language Server Protocol)
- [ ] `rask lsp` command
- [ ] Autocomplete
- [ ] Go to definition
- [ ] Hover documentation
- [ ] Error highlighting
- [ ] VS Code extension
- [ ] Works with any LSP-compatible editor

#### Week 35-36: REPL Improvements
- [x] Multi-line editing
- [x] Syntax highlighting
- [x] Auto-completion
- [x] History
- [x] Performance timing display
- [x] Inline documentation

**Milestone**: Professional IDE experience

---

## Syntax Design Decisions

### Settled Decisions

**Statement terminators**: Newlines (with optional semicolons for multiple statements per line)
```rask
x = 10
y = 20
z = x + y; print(z)  # semicolon optional for multiline
```

**Braces vs indentation**: Braces `{ }` - easier to parse, copy-paste friendly
```rask
if condition {
  doThing()
}
```

**Function syntax**: `def` keyword
```rask
def greet(name: string) -> string {
  return "Hello, {name}!"
}
```

**Type annotations**: After variable (inference-friendly)
```rask
age: int = 25
name: string = "Alice"
items = [1, 2, 3]  # inferred as List<int>
```

**Comments**: `#` for single line, `/* */` for multiline
```rask
# This is a comment
/* This is a
   multiline comment */
```

---

## Example Rask Code

### Basic Script
```rask
#!/usr/bin/env rask

# Variables with inference
name = "Rask"
version = 1.0

# Functions
def greet(name: string) -> string {
  return "Hello, {name}!"
}

print(greet(name))
```

### With Types
```rask
# Explicit types when you want them
users: List<string> = []
count: int = 0

def processUser(user: string) -> Result<User, Error> {
  # might fail
  parsed = parseUser(user) or return err
  return Ok(parsed)
}
```

### HTTP Request
```rask
#!/usr/bin/env rask --allow-net=api.github.com

use std.http
use std.json

# Fetch data
response = http.get("https://api.github.com/users/rask")
data = response.json()

print("User: {data.name}")
print("Repos: {data.public_repos}")
```

### File Processing
```rask
#!/usr/bin/env rask --allow-read=. --allow-write=output.txt

use std.fs
use std.json

# Read and process
content = fs.read("data.json")
data = json.parse(content)

# Transform
results = [item.name for item in data if item.active]

# Write
fs.write("output.txt", results.join("\n"))
```

### Concurrent Operations
```rask
use std.http

# All three start immediately and run concurrently
users = http.get("https://api.com/users")
posts = http.get("https://api.com/posts")
comments = http.get("https://api.com/comments")

# Waits for all when accessed
print(users.json().length)
print(posts.json().length)
print(comments.json().length)
```

---

## Technical Stack

**Implementation Language**: Rust
- Memory safety
- Great performance
- Rich ecosystem for parsing, HTTP, etc.
- Cross-compilation support

**Parser Strategy**: Hand-written recursive descent
- Full control over error messages
- Easy to extend
- Good performance

**Runtime**: Bytecode VM (initially), JIT later
- Fast startup
- Predictable performance
- Easier to implement than native compilation

**Standard Library**: Wrap Rust crates
- `reqwest` for HTTP
- `serde_json` for JSON
- `tokio` for async runtime
- Don't reinvent wheels

---

## Project Structure

```
rask/
├── src/
│   ├── main.rs              # CLI entry point
│   ├── repl/
│   │   ├── mod.rs           # REPL implementation
│   │   └── highlighter.rs   # Syntax highlighting
│   ├── lexer/
│   │   ├── mod.rs           # Tokenization
│   │   └── token.rs         # Token types
│   ├── parser/
│   │   ├── mod.rs           # Parsing logic
│   │   ├── ast.rs           # AST definitions
│   │   └── expr.rs          # Expression parsing
│   ├── typechecker/
│   │   ├── mod.rs           # Type inference
│   │   └── types.rs         # Type definitions
│   ├── runtime/
│   │   ├── mod.rs           # VM or interpreter
│   │   ├── value.rs         # Runtime values
│   │   └── builtins.rs      # Built-in functions
│   ├── stdlib/
│   │   ├── mod.rs
│   │   ├── fs.rs            # Filesystem module
│   │   ├── http.rs          # HTTP client
│   │   ├── json.rs          # JSON parser
│   │   └── ...
│   └── errors/
│       ├── mod.rs           # Error handling
│       └── pretty.rs        # Pretty error printing
├── stdlib/                   # Standard library in Rask
│   ├── std/
│   │   ├── fs.rask
│   │   ├── http.rask
│   │   └── ...
├── tests/
│   ├── lexer_tests.rs
│   ├── parser_tests.rs
│   └── integration/
│       ├── basic.rask
│       ├── types.rask
│       └── ...
├── examples/
│   ├── hello.rask
│   ├── http_client.rask
│   ├── file_processor.rask
│   └── ...
├── docs/
│   ├── language_spec.md
│   ├── stdlib_reference.md
│   └── tutorial.md
├── Cargo.toml
├── plan.md                   # This file
└── README.md
```

---

## Marketing & Community

### Before Writing Code
1. ✅ Claim domain: rask-lang.org or getrask.dev
2. ✅ Set up GitHub org/repo
3. ✅ Create Discord/Slack for early adopters
4. ✅ Write manifesto: "Why Rask?"

### Launch Strategy
1. **Blog the journey**: Weekly dev updates
2. **Build in public**: Share progress, decisions, struggles
3. **Early access program**: Let people try it before 1.0
4. **Documentation first**: Great docs from day one
5. **Example gallery**: Show what's possible

### Marketing Angles
- "Python without the dependency hell"
- "Scripts that actually work on your coworker's machine"
- "Secure by default - no more accidental rm -rf /"
- "One binary, zero setup"
- "Write once, run anywhere (for real this time)"

---

## Success Metrics

### Phase 1 Success
- REPL is fun to use
- Can write basic scripts
- 10+ early testers giving feedback

### Phase 3 Success
- Can replace Python for 50% of common scripts
- 100+ stars on GitHub
- Someone writes a blog post about it

### Phase 6 Success
- Error messages are better than any other language
- VS Code extension works great
- 1000+ stars on GitHub

### Phase 8 Success
- Production use cases emerge
- Community contributions
- Other tools integrate with Rask
- Job postings mention Rask (the dream!)

---

## Risks & Mitigations

### Risk: "Just another language nobody uses"
**Mitigation**: Solve REAL problems (dependency hell, security). Focus on niche first (DevOps scripts, automation).

### Risk: Ecosystem too small
**Mitigation**: Batteries-included stdlib. FFI to Rust/C for escape hatch. URL imports mean no central registry bottleneck.

### Risk: Performance not good enough
**Mitigation**: Benchmark early and often. Profile and optimize hot paths. Users chose Python despite slowness, so "good enough" is achievable.

### Risk: Scope creep
**Mitigation**: Stay focused on scripting. Don't try to be systems language, web framework, database, etc. Do one thing well.

### Risk: Burnout
**Mitigation**: Build in public for accountability. Release early for motivation. Get help from community. It's okay to pause.

---

## Next Actions

### Immediate (This Week)
1. [ ] Set up Rust project: `cargo new rask`
2. [ ] Implement basic lexer for numbers and operators
3. [ ] Parse and evaluate `2 + 2`
4. [ ] Get REPL loop working
5. [ ] Write first blog post: "Why I'm Building Rask"

### Short Term (This Month)
1. [ ] Variables and basic expressions
2. [ ] Functions
3. [ ] If/else, loops
4. [ ] Lists and maps
5. [ ] File I/O

### Medium Term (3 Months)
1. [ ] Type system working
2. [ ] Standard library basics
3. [ ] Permission system
4. [ ] URL imports
5. [ ] First users writing real scripts

---

## Resources & References

### Language Design Inspiration
- **Python**: Syntax clarity, batteries included
- **Rust**: Type system, error messages, ownership
- **Go**: Simplicity, one way to do things, fast compile
- **Deno**: Security model, URL imports, TypeScript integration
- **Lua**: Minimalism, embeddability, tables
- **Ruby**: Everything is an expression, blocks
- **Elm**: Error messages, no runtime exceptions

### Technical References
- Crafting Interpreters (Bob Nystrom)
- Programming Language Pragmatics
- Types and Programming Languages (Pierce)
- LLVM/Cranelift documentation
- Rust async book

### Community Examples
- Zig: Building in public, clear vision
- Gleam: Great onboarding, friendly community
- Roc: Ambitious but practical
- V: What NOT to do (overpromise)

---

## Open Questions

- [ ] Should we have a `nil` or use Option types everywhere?
- [ ] Garbage collector or reference counting?
- [ ] Should strings be mutable?
- [ ] Do we need a `const` keyword?
- [ ] How do we handle circular imports?
- [ ] What's the story for C FFI?
- [ ] Should REPL save history? Where?
- [ ] Do we need a package.json equivalent?

---

**Last Updated**: 2026-02-13
**Next Review**: After Phase 1 completion

---

*Remember: The best language is the one people actually use. Focus on making Rask so easy and reliable that switching is a no-brainer.*
