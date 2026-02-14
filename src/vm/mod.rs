use std::collections::HashMap;
use std::fmt;

use crate::parser::ast::{BinaryOp, Expr, Program, Stmt, UnaryOp};
use crate::runtime::value::Value;

#[derive(Debug, Clone)]
pub enum Instruction {
    LoadConst(usize),
    LoadGlobal(String),
    StoreGlobal(String),
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Negate,
    Not,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Print,
    Pop,
    Return,
}

#[derive(Debug, Clone)]
pub struct BytecodeChunk {
    pub constants: Vec<Value>,
    pub instructions: Vec<Instruction>,
}

impl BytecodeChunk {
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct VmCompileError {
    pub message: String,
}

impl VmCompileError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for VmCompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vm compile error: {}", self.message)
    }
}

impl std::error::Error for VmCompileError {}

#[derive(Debug, Clone)]
pub struct VmRuntimeError {
    pub message: String,
}

impl VmRuntimeError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for VmRuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "vm runtime error: {}", self.message)
    }
}

impl std::error::Error for VmRuntimeError {}

pub fn compile_program(program: &Program) -> Result<BytecodeChunk, VmCompileError> {
    let mut compiler = Compiler::new();
    compiler.compile_program(program)?;
    Ok(compiler.chunk)
}

pub fn run_program(program: &Program) -> Result<Value, VmRuntimeError> {
    let chunk = compile_program(program).map_err(|err| VmRuntimeError::new(err.message))?;
    execute_chunk(&chunk)
}

pub fn execute_chunk(chunk: &BytecodeChunk) -> Result<Value, VmRuntimeError> {
    let mut vm = Vm::new();
    vm.execute(chunk)
}

struct Compiler {
    chunk: BytecodeChunk,
}

impl Compiler {
    fn new() -> Self {
        Self {
            chunk: BytecodeChunk::new(),
        }
    }

    fn compile_program(&mut self, program: &Program) -> Result<(), VmCompileError> {
        let statement_count = program.statements.len();
        for (index, statement) in program.statements.iter().enumerate() {
            let is_last = index + 1 == statement_count;
            self.compile_statement(statement)?;
            if !is_last && statement_produces_value(statement) {
                self.emit(Instruction::Pop);
            }
        }
        self.emit(Instruction::Return);
        Ok(())
    }

    fn compile_statement(&mut self, statement: &Stmt) -> Result<(), VmCompileError> {
        match statement {
            Stmt::VarDecl {
                name, initializer, ..
            } => {
                self.compile_expr(initializer)?;
                self.emit(Instruction::StoreGlobal(name.clone()));
                Ok(())
            }
            Stmt::Print { expr } => {
                self.compile_expr(expr)?;
                self.emit(Instruction::Print);
                Ok(())
            }
            Stmt::Expr(expr) => self.compile_expr(expr),
            Stmt::If { .. } => Err(VmCompileError::new(
                "if statements are not yet supported by the VM",
            )),
            Stmt::While { .. } => Err(VmCompileError::new(
                "while statements are not yet supported by the VM",
            )),
            Stmt::For { .. } => Err(VmCompileError::new(
                "for loops are not yet supported by the VM",
            )),
            Stmt::ForIn { .. } => Err(VmCompileError::new(
                "for-in loops are not yet supported by the VM",
            )),
            Stmt::Use { .. } => Err(VmCompileError::new(
                "use statements are not yet supported by the VM",
            )),
            Stmt::DestructureDecl { .. } => Err(VmCompileError::new(
                "destructuring declarations are not yet supported by the VM",
            )),
            Stmt::FunctionDef { .. } => Err(VmCompileError::new(
                "function definitions are not yet supported by the VM",
            )),
            Stmt::Return { .. } => Err(VmCompileError::new(
                "return statements are not yet supported by the VM",
            )),
            Stmt::Test { .. } => Err(VmCompileError::new(
                "test blocks are not yet supported by the VM",
            )),
            Stmt::Assert { .. } => Err(VmCompileError::new(
                "assert statements are not yet supported by the VM",
            )),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), VmCompileError> {
        if let Some(value) = const_eval(expr)? {
            self.emit_constant(value);
            return Ok(());
        }

        match expr {
            Expr::Int(value) => self.emit_constant(Value::Int(*value)),
            Expr::Float(value) => self.emit_constant(Value::Float(*value)),
            Expr::String { value, .. } => self.emit_constant(Value::String(value.clone())),
            Expr::Bool(value) => self.emit_constant(Value::Bool(*value)),
            Expr::Nil => self.emit_constant(Value::Nil),
            Expr::Variable(name) => self.emit(Instruction::LoadGlobal(name.clone())),
            Expr::Assign { name, value } => {
                self.compile_expr(value)?;
                self.emit(Instruction::StoreGlobal(name.clone()));
            }
            Expr::Grouping(inner) => self.compile_expr(inner)?,
            Expr::Unary { op, rhs } => {
                self.compile_expr(rhs)?;
                self.emit(match op {
                    UnaryOp::Negate => Instruction::Negate,
                    UnaryOp::Not => Instruction::Not,
                });
            }
            Expr::Binary { lhs, op, rhs } => {
                self.compile_expr(lhs)?;
                self.compile_expr(rhs)?;
                self.emit(match op {
                    BinaryOp::Add => Instruction::Add,
                    BinaryOp::Subtract => Instruction::Subtract,
                    BinaryOp::Multiply => Instruction::Multiply,
                    BinaryOp::Divide => Instruction::Divide,
                    BinaryOp::Modulo => Instruction::Modulo,
                    BinaryOp::And => Instruction::And,
                    BinaryOp::Equal => Instruction::Equal,
                    BinaryOp::NotEqual => Instruction::NotEqual,
                    BinaryOp::Less => Instruction::Less,
                    BinaryOp::LessEqual => Instruction::LessEqual,
                    BinaryOp::Greater => Instruction::Greater,
                    BinaryOp::GreaterEqual => Instruction::GreaterEqual,
                });
            }
            Expr::Call { .. } => {
                return Err(VmCompileError::new(
                    "function calls are not yet supported by the VM",
                ))
            }
            Expr::Member { .. } => {
                return Err(VmCompileError::new(
                    "member access is not yet supported by the VM",
                ))
            }
            Expr::Coalesce { .. } => {
                return Err(VmCompileError::new(
                    "coalesce expressions are not yet supported by the VM",
                ))
            }
            Expr::OrReturn { .. } => {
                return Err(VmCompileError::new(
                    "or-return expressions are not yet supported by the VM",
                ))
            }
            Expr::PanicUnwrap(_) => {
                return Err(VmCompileError::new(
                    "panic unwrap is not yet supported by the VM",
                ))
            }
            Expr::Match { .. } => {
                return Err(VmCompileError::new(
                    "match expressions are not yet supported by the VM",
                ))
            }
            Expr::ListLiteral(_) => {
                return Err(VmCompileError::new(
                    "list literals are not yet supported by the VM",
                ))
            }
            Expr::ListComprehension { .. } => {
                return Err(VmCompileError::new(
                    "list comprehensions are not yet supported by the VM",
                ))
            }
            Expr::MapLiteral(_) => {
                return Err(VmCompileError::new(
                    "map literals are not yet supported by the VM",
                ))
            }
            Expr::Index { .. } => {
                return Err(VmCompileError::new(
                    "index expressions are not yet supported by the VM",
                ))
            }
            Expr::Function { .. } => {
                return Err(VmCompileError::new(
                    "inline function expressions are not yet supported by the VM",
                ))
            }
        }
        Ok(())
    }

    fn emit_constant(&mut self, value: Value) {
        let index = self.chunk.constants.len();
        self.chunk.constants.push(value);
        self.chunk.instructions.push(Instruction::LoadConst(index));
    }

    fn emit(&mut self, instruction: Instruction) {
        self.chunk.instructions.push(instruction);
    }
}

struct Vm {
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
    ip: usize,
}

impl Vm {
    fn new() -> Self {
        Self {
            stack: Vec::new(),
            globals: HashMap::new(),
            ip: 0,
        }
    }

    fn execute(&mut self, chunk: &BytecodeChunk) -> Result<Value, VmRuntimeError> {
        self.ip = 0;
        while let Some(instruction) = chunk.instructions.get(self.ip) {
            self.ip += 1;
            match instruction {
                Instruction::LoadConst(index) => {
                    let constant = chunk
                        .constants
                        .get(*index)
                        .ok_or_else(|| VmRuntimeError::new("constant index out of bounds"))?;
                    self.stack.push(constant.clone());
                }
                Instruction::LoadGlobal(name) => {
                    let value = self.globals.get(name).cloned().ok_or_else(|| {
                        VmRuntimeError::new(format!("unknown variable '{}'", name))
                    })?;
                    self.stack.push(value);
                }
                Instruction::StoreGlobal(name) => {
                    let value = self
                        .stack
                        .last()
                        .cloned()
                        .ok_or_else(|| VmRuntimeError::new("stack underflow on store"))?;
                    self.globals.insert(name.clone(), value);
                }
                Instruction::Add => self.binary_numeric_or_string(|a, b| a + b)?,
                Instruction::Subtract => self.binary_numeric(|a, b| a - b)?,
                Instruction::Multiply => self.binary_numeric(|a, b| a * b)?,
                Instruction::Divide => self.binary_numeric(|a, b| a / b)?,
                Instruction::Modulo => self.binary_numeric(|a, b| a % b)?,
                Instruction::And => {
                    let rhs = self.pop()?;
                    let lhs = self.pop()?;
                    self.stack
                        .push(Value::Bool(lhs.is_truthy() && rhs.is_truthy()));
                }
                Instruction::Negate => {
                    let value = self.pop()?;
                    match value {
                        Value::Int(v) => self.stack.push(Value::Int(-v)),
                        Value::Float(v) => self.stack.push(Value::Float(-v)),
                        other => {
                            return Err(VmRuntimeError::new(format!(
                                "cannot negate '{}'",
                                other.type_name()
                            )))
                        }
                    }
                }
                Instruction::Not => {
                    let value = self.pop()?;
                    self.stack.push(Value::Bool(!value.is_truthy()));
                }
                Instruction::Equal => self.binary_compare(|a, b| a == b)?,
                Instruction::NotEqual => self.binary_compare(|a, b| a != b)?,
                Instruction::Less => self.binary_order(|a, b| a < b)?,
                Instruction::LessEqual => self.binary_order(|a, b| a <= b)?,
                Instruction::Greater => self.binary_order(|a, b| a > b)?,
                Instruction::GreaterEqual => self.binary_order(|a, b| a >= b)?,
                Instruction::Print => {
                    let value = self
                        .stack
                        .last()
                        .ok_or_else(|| VmRuntimeError::new("stack underflow on print"))?;
                    println!("{}", value);
                }
                Instruction::Pop => {
                    let _ = self.pop()?;
                }
                Instruction::Return => {
                    return Ok(self.stack.pop().unwrap_or(Value::Nil));
                }
            }
        }
        Ok(self.stack.pop().unwrap_or(Value::Nil))
    }

    fn pop(&mut self) -> Result<Value, VmRuntimeError> {
        self.stack
            .pop()
            .ok_or_else(|| VmRuntimeError::new("stack underflow"))
    }

    fn binary_numeric(&mut self, op: impl Fn(f64, f64) -> f64) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let a = to_f64(&lhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric lhs, got '{}'", lhs.type_name()))
        })?;
        let b = to_f64(&rhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric rhs, got '{}'", rhs.type_name()))
        })?;
        let result = op(a, b);
        if matches!(lhs, Value::Int(_)) && matches!(rhs, Value::Int(_)) && result.fract() == 0.0 {
            self.stack.push(Value::Int(result as i64));
        } else {
            self.stack.push(Value::Float(result));
        }
        Ok(())
    }

    fn binary_numeric_or_string(
        &mut self,
        op: impl Fn(f64, f64) -> f64,
    ) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        match (&lhs, &rhs) {
            (Value::String(a), Value::String(b)) => {
                self.stack.push(Value::String(format!("{}{}", a, b)));
                return Ok(());
            }
            _ => {}
        }

        let a = to_f64(&lhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric lhs, got '{}'", lhs.type_name()))
        })?;
        let b = to_f64(&rhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric rhs, got '{}'", rhs.type_name()))
        })?;
        let result = op(a, b);
        if matches!(lhs, Value::Int(_)) && matches!(rhs, Value::Int(_)) && result.fract() == 0.0 {
            self.stack.push(Value::Int(result as i64));
        } else {
            self.stack.push(Value::Float(result));
        }
        Ok(())
    }

    fn binary_compare(
        &mut self,
        compare: impl Fn(&Value, &Value) -> bool,
    ) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        self.stack.push(Value::Bool(compare(&lhs, &rhs)));
        Ok(())
    }

    fn binary_order(&mut self, compare: impl Fn(f64, f64) -> bool) -> Result<(), VmRuntimeError> {
        let rhs = self.pop()?;
        let lhs = self.pop()?;
        let a = to_f64(&lhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric lhs, got '{}'", lhs.type_name()))
        })?;
        let b = to_f64(&rhs).ok_or_else(|| {
            VmRuntimeError::new(format!("expected numeric rhs, got '{}'", rhs.type_name()))
        })?;
        self.stack.push(Value::Bool(compare(a, b)));
        Ok(())
    }
}

fn statement_produces_value(statement: &Stmt) -> bool {
    matches!(
        statement,
        Stmt::VarDecl { .. } | Stmt::Expr(_) | Stmt::Print { .. }
    )
}

fn const_eval(expr: &Expr) -> Result<Option<Value>, VmCompileError> {
    match expr {
        Expr::Int(v) => Ok(Some(Value::Int(*v))),
        Expr::Float(v) => Ok(Some(Value::Float(*v))),
        Expr::String { value, .. } => Ok(Some(Value::String(value.clone()))),
        Expr::Bool(v) => Ok(Some(Value::Bool(*v))),
        Expr::Nil => Ok(Some(Value::Nil)),
        Expr::Grouping(inner) => const_eval(inner),
        Expr::Unary { op, rhs } => {
            let Some(rhs) = const_eval(rhs)? else {
                return Ok(None);
            };
            match op {
                UnaryOp::Negate => match rhs {
                    Value::Int(v) => Ok(Some(Value::Int(-v))),
                    Value::Float(v) => Ok(Some(Value::Float(-v))),
                    _ => Ok(None),
                },
                UnaryOp::Not => Ok(Some(Value::Bool(!rhs.is_truthy()))),
            }
        }
        Expr::Binary { lhs, op, rhs } => {
            let Some(lhs) = const_eval(lhs)? else {
                return Ok(None);
            };
            let Some(rhs) = const_eval(rhs)? else {
                return Ok(None);
            };
            eval_const_binary(op, lhs, rhs)
        }
        _ => Ok(None),
    }
}

fn eval_const_binary(
    op: &BinaryOp,
    lhs: Value,
    rhs: Value,
) -> Result<Option<Value>, VmCompileError> {
    let value = match op {
        BinaryOp::Add => match (lhs, rhs) {
            (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
            (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
            (Value::Int(a), Value::Float(b)) => Value::Float(a as f64 + b),
            (Value::Float(a), Value::Int(b)) => Value::Float(a + b as f64),
            (Value::String(a), Value::String(b)) => Value::String(format!("{}{}", a, b)),
            _ => return Ok(None),
        },
        BinaryOp::Subtract => const_numeric_binary(lhs, rhs, |a, b| a - b)?,
        BinaryOp::Multiply => const_numeric_binary(lhs, rhs, |a, b| a * b)?,
        BinaryOp::Divide => const_numeric_binary(lhs, rhs, |a, b| a / b)?,
        BinaryOp::Modulo => const_numeric_binary(lhs, rhs, |a, b| a % b)?,
        BinaryOp::And => Value::Bool(lhs.is_truthy() && rhs.is_truthy()),
        BinaryOp::Equal => Value::Bool(lhs == rhs),
        BinaryOp::NotEqual => Value::Bool(lhs != rhs),
        BinaryOp::Less => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a < b)?),
        BinaryOp::LessEqual => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a <= b)?),
        BinaryOp::Greater => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a > b)?),
        BinaryOp::GreaterEqual => Value::Bool(const_numeric_order(lhs, rhs, |a, b| a >= b)?),
    };
    Ok(Some(value))
}

fn const_numeric_binary(
    lhs: Value,
    rhs: Value,
    op: impl Fn(f64, f64) -> f64,
) -> Result<Value, VmCompileError> {
    let a =
        to_f64(&lhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric lhs"))?;
    let b =
        to_f64(&rhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric rhs"))?;
    let result = op(a, b);
    if matches!(lhs, Value::Int(_)) && matches!(rhs, Value::Int(_)) && result.fract() == 0.0 {
        Ok(Value::Int(result as i64))
    } else {
        Ok(Value::Float(result))
    }
}

fn const_numeric_order(
    lhs: Value,
    rhs: Value,
    compare: impl Fn(f64, f64) -> bool,
) -> Result<bool, VmCompileError> {
    let a =
        to_f64(&lhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric lhs"))?;
    let b =
        to_f64(&rhs).ok_or_else(|| VmCompileError::new("constant folding expected numeric rhs"))?;
    Ok(compare(a, b))
}

fn to_f64(value: &Value) -> Option<f64> {
    match value {
        Value::Int(v) => Some(*v as f64),
        Value::Float(v) => Some(*v),
        _ => None,
    }
}
