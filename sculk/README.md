# Sculk - SCALF Native Compiler

**STATUS: EXPERIMENTAL - NOT PRODUCTION READY**

Sculk is the native code generation backend for SCALF. It compiles SCALF code directly to machine code for maximum performance.

## ⚠️ Important Notice

**This is an experimental component.** The main SCALF compiler uses bytecode (fast, stable, works today). Sculk is a long-term performance optimization project.

**Do NOT use Sculk for anything important yet.** Stick with bytecode mode for now.

## Architecture

```
SCALF Source Code
      ↓
SCALF AST (from main compiler)
      ↓
Sculk IR (intermediate representation)
      ↓
Optimization Passes
      ↓
Backend (Cranelift/QBE/Custom)
      ↓
Native Machine Code (x86-64, ARM64, etc.)
```

## Design Philosophy

1. **Use existing backends when possible** - Don't reinvent codegen
2. **Start simple** - Get it working with Cranelift first
3. **Optimize later** - Correctness before speed
4. **Keep it optional** - Bytecode is always the default

## Compilation Modes

### Mode 1: Cranelift (Recommended - Start Here)
- **Pros**: Production-ready, handles register allocation, multi-arch
- **Cons**: Larger dependency, less control
- **Target**: v0.6.0

### Mode 2: QBE (Simpler Alternative)
- **Pros**: Tiny, simple, good enough performance
- **Cons**: Less optimization, fewer targets
- **Target**: v0.7.0 (if Cranelift doesn't work out)

### Mode 3: Custom x86-64 Backend (Future)
- **Pros**: Maximum control, learning experience
- **Cons**: Months of work, maintenance burden
- **Target**: v1.0.0+ (maybe never)

## Current Status

**Phase 0: Foundation (Current)**
- [ ] Project structure
- [ ] IR design
- [ ] AST → IR lowering
- [ ] Basic Cranelift integration

**Phase 1: Basic Compilation (Next)**
- [ ] Compile simple expressions
- [ ] Compile functions
- [ ] Compile control flow
- [ ] Link into executable

**Phase 2: Feature Parity**
- [ ] All SCALF features supported
- [ ] Match bytecode correctness
- [ ] Basic optimization passes

**Phase 3: Performance**
- [ ] Benchmark vs bytecode
- [ ] Optimize hot paths
- [ ] Competitive with other native languages

**Phase 4: Production**
- [ ] Stable API
- [ ] Good error messages
- [ ] Cross-compilation
- [ ] Make it default (maybe)

## Directory Structure

```
sculk/
├── src/
│   ├── lib.rs              # Main entry point
│   ├── ir/                 # Intermediate Representation
│   │   ├── mod.rs          # IR data structures
│   │   ├── builder.rs      # IR construction helpers
│   │   └── printer.rs      # IR debug printing
│   ├── codegen/            # Code generation orchestration
│   │   ├── mod.rs          # Main codegen logic
│   │   └── lowering.rs     # AST → IR lowering
│   ├── backend/            # Backend implementations
│   │   ├── mod.rs          # Backend trait
│   │   ├── cranelift.rs    # Cranelift backend
│   │   ├── qbe.rs          # QBE backend (future)
│   │   └── x86_64.rs       # Custom backend (far future)
│   └── optimize/           # Optimization passes
│       ├── mod.rs          # Pass manager
│       ├── constant_fold.rs
│       ├── dead_code.rs
│       └── inline.rs
├── runtime/                # Minimal runtime library
│   ├── memory.rs           # Memory management (refcounting)
│   ├── builtins.rs         # Built-in functions in native code
│   └── ffi.rs              # Foreign function interface
├── tests/                  # Integration tests
│   ├── compile_tests.rs    # Test compilation
│   └── fixtures/           # Test SCALF files
└── examples/               # Example compiled programs
    └── hello_native.scalf
```

## Usage (When Ready)

```bash
# Compile with Cranelift (future)
scalf build --native script.scalf
# or
scalf build --backend=cranelift script.scalf

# Compare bytecode vs native
scalf build script.scalf              # Bytecode: 3MB, instant compile
scalf build --native script.scalf     # Native: 200KB, 5s compile

# Development: test Sculk
cd sculk
cargo test
cargo bench
```

## Development Roadmap

### Week 1-2: IR Design
- Define IR instruction set
- Design IR data structures
- AST → IR lowering for expressions

### Week 3-4: Cranelift Integration
- Add Cranelift dependency
- IR → Cranelift IR translation
- Generate first executable (hello world)

### Week 5-8: Language Features
- Functions and calls
- Control flow (if/while/for)
- Variables and scoping
- Basic types

### Week 9-12: Collections & Stdlib
- Lists and maps
- String operations
- Stdlib function calls

### Week 13-16: Optimization
- Constant folding
- Dead code elimination
- Inlining
- Benchmarking

### Month 5+: Production Readiness
- Error handling
- Debug info generation
- Cross-compilation
- Performance tuning

## Performance Goals

**Startup time:**
- Bytecode: <10ms (current)
- Native: <1ms (target)

**Runtime performance:**
- Match or beat Lua
- Within 2-3x of C for compute
- Beat Python by 10x+

**Binary size:**
- Bytecode: 2-5MB (current)
- Native: 100-500KB (target)

## Contributing Guidelines

**For now: Don't.** This is experimental and will change rapidly.

When Sculk is more stable (v0.6.0+), contributions welcome for:
- Backend implementations
- Optimization passes
- Platform support
- Benchmarks

## References

- [Cranelift Documentation](https://cranelift.dev/)
- [QBE Compiler](https://c9x.me/compile/)
- [LLVM IR Reference](https://llvm.org/docs/LangRef.html)
- [Writing a Compiler Backend](https://www.sigbus.info/compilerbook)

## Questions?

Ask in the main SCALF repo: https://github.com/yourusername/scalf

---

**Remember: Bytecode is the default. Sculk is the future. Don't block releases on Sculk.**

## Advanced Example Scripts

The `sculk/examples/advanced/` directory includes larger scripts that run on the current experimental native subset:

- `arithmetic_pipeline.scl` - chained function calls and arithmetic flow
- `function_chain_logic.scl` - call graph + comparisons + boolean logic
- `top_level_script.scl` - top-level script mode without explicit `def main`
- `use_local_module.scl` - local file import via `use "math_utils.scl"`

`use` support in Sculk currently resolves local string imports (`use "file.scl"`). Module-path imports, URL imports, and `as` aliases are not implemented yet.

Run one with:

```bash
cd sculk
cargo run -- examples/advanced/arithmetic_pipeline.scl
```



### Windows .exe Output

Sculk can emit standalone Windows executables by linking generated object code through `rustc`:

```bash
cd sculk
cargo run -- examples/advanced/use_local_module.scl --emit-exe
# or choose output path
cargo run -- examples/advanced/use_local_module.scl --emit-exe target/use_local_module.exe
```
