//! Sculk - Native code generation backend for SCALF
//!
//! **STATUS: EXPERIMENTAL - DO NOT USE IN PRODUCTION**
//!
//! Sculk compiles SCALF code to native machine code for maximum performance.
//! This is a long-term project - the main SCALF compiler uses bytecode and
//! is stable and production-ready.

#![warn(missing_docs)]

pub mod backend;
pub mod codegen;
mod frontend;
pub mod ir;
pub mod optimize;
pub mod runtime;

use std::path::Path;

use scalf::parser::ast::Program;

/// Sculk compiler version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Main compiler interface
pub struct Compiler {
    /// Optimization level (0-3)
    pub opt_level: u8,
    /// Target architecture
    pub target: String,
}

impl Compiler {
    /// Create a new compiler with default settings
    pub fn new() -> Self {
        Self {
            opt_level: 0,
            target: std::env::consts::ARCH.to_string(),
        }
    }

    /// Parse SCALF source text and expand supported imports.
    pub fn parse_and_expand_source(
        &self,
        source: &str,
        origin_path: Option<&Path>,
    ) -> Result<Program, CompileError> {
        let mut frontend = frontend::Frontend::new();
        frontend.parse_and_expand(source, origin_path)
    }

    /// Parse a SCALF file and expand supported imports.
    pub fn parse_and_expand_file(&self, path: &Path) -> Result<Program, CompileError> {
        let source = std::fs::read_to_string(path).map_err(|err| {
            CompileError::FrontendError(format!("failed to read '{}': {}", path.display(), err))
        })?;
        self.parse_and_expand_source(&source, Some(path))
    }

    /// Compile SCALF source text into Sculk IR.
    pub fn compile_source(
        &self,
        source: &str,
        module_name: &str,
    ) -> Result<ir::Module, CompileError> {
        self.compile_source_with_origin(source, module_name, None)
    }

    /// Compile a SCALF source file into Sculk IR.
    pub fn compile_file(&self, path: &Path) -> Result<ir::Module, CompileError> {
        let source = std::fs::read_to_string(path).map_err(|err| {
            CompileError::FrontendError(format!("failed to read '{}': {}", path.display(), err))
        })?;
        let module_name = path
            .file_stem()
            .and_then(|value| value.to_str())
            .unwrap_or("main");
        self.compile_source_with_origin(&source, module_name, Some(path))
    }

    fn compile_source_with_origin(
        &self,
        source: &str,
        module_name: &str,
        origin_path: Option<&Path>,
    ) -> Result<ir::Module, CompileError> {
        let program = self.parse_and_expand_source(source, origin_path)?;
        self.compile_program(&program, module_name)
    }

    /// Compile parsed SCALF program into Sculk IR.
    pub fn compile_program(
        &self,
        program: &Program,
        module_name: &str,
    ) -> Result<ir::Module, CompileError> {
        let mut lowering = codegen::lowering::Lowering::new();
        lowering.lower_program(program, module_name)
    }

    /// Compile SCALF AST to IR.
    pub fn compile_ast(&self, ast: &Program) -> Result<ir::Module, CompileError> {
        self.compile_program(ast, "main")
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Compilation errors
#[derive(Debug)]
pub enum CompileError {
    /// Frontend (lex/parse/import) error
    FrontendError(String),
    /// Feature not yet implemented
    NotImplemented(&'static str),
    /// IR validation failed
    InvalidIR(String),
    /// Backend error
    BackendError(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::FrontendError(msg) => write!(f, "Frontend error: {}", msg),
            CompileError::NotImplemented(msg) => write!(f, "Not implemented: {}", msg),
            CompileError::InvalidIR(msg) => write!(f, "Invalid IR: {}", msg),
            CompileError::BackendError(msg) => write!(f, "Backend error: {}", msg),
        }
    }
}

impl std::error::Error for CompileError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_creation() {
        let compiler = Compiler::new();
        assert_eq!(compiler.opt_level, 0);
    }
}
