//! Intermediate Representation for Sculk
//!
//! The IR is a low-level, typed representation of SCALF programs
//! that is easier to optimize and compile than the AST.

pub mod builder;
pub mod printer;

/// A compiled module.
#[derive(Debug, Clone)]
pub struct Module {
    /// Module name.
    pub name: String,
    /// Functions defined in this module.
    pub functions: Vec<Function>,
    /// Global constants defined in this module.
    pub constants: Vec<Constant>,
}

/// A function definition.
#[derive(Debug, Clone)]
pub struct Function {
    /// Function name.
    pub name: String,
    /// Function parameters.
    pub params: Vec<Parameter>,
    /// Function return type.
    pub return_type: Type,
    /// Basic blocks in function body.
    pub blocks: Vec<BasicBlock>,
}

/// Function parameter.
#[derive(Debug, Clone)]
pub struct Parameter {
    /// Parameter name.
    pub name: String,
    /// Parameter type.
    pub ty: Type,
}

/// A basic block (straight-line code with no branches except at the end).
#[derive(Debug, Clone)]
pub struct BasicBlock {
    /// Block label.
    pub label: String,
    /// Instructions in this block.
    pub instructions: Vec<Instruction>,
    /// Block terminator.
    pub terminator: Terminator,
}

/// IR instruction.
#[derive(Debug, Clone)]
pub enum Instruction {
    /// Assign a value to a destination variable.
    Assign {
        /// Destination variable name.
        dest: String,
        /// Value to assign.
        value: Value,
    },
    /// Evaluate a binary operation and store the result.
    BinOp {
        /// Destination variable name.
        dest: String,
        /// Binary operator.
        op: BinOp,
        /// Left operand.
        left: Value,
        /// Right operand.
        right: Value,
    },
    /// Call a function.
    Call {
        /// Optional destination variable for the call result.
        dest: Option<String>,
        /// Function name.
        func: String,
        /// Positional arguments.
        args: Vec<Value>,
    },
    /// Load a value from memory.
    Load {
        /// Destination variable name.
        dest: String,
        /// Address expression.
        addr: Value,
    },
    /// Store a value to memory.
    Store {
        /// Address expression.
        addr: Value,
        /// Value expression.
        value: Value,
    },
}

/// Block terminator (control flow).
#[derive(Debug, Clone)]
pub enum Terminator {
    /// Return from function with an optional value.
    Return(Option<Value>),
    /// Unconditional branch to a target block label.
    Branch {
        /// Target block label.
        target: String,
    },
    /// Conditional branch to one of two target block labels.
    CondBranch {
        /// Branch condition expression.
        cond: Value,
        /// Target label when condition is truthy.
        then_block: String,
        /// Target label when condition is falsy.
        else_block: String,
    },
    /// Marks unreachable code.
    Unreachable,
}

/// Binary operation.
#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    /// Integer/float addition.
    Add,
    /// Integer/float subtraction.
    Sub,
    /// Integer/float multiplication.
    Mul,
    /// Integer/float division.
    Div,
    /// Integer modulo.
    Mod,
    /// Equality comparison.
    Eq,
    /// Inequality comparison.
    Ne,
    /// Less-than comparison.
    Lt,
    /// Less-than-or-equal comparison.
    Le,
    /// Greater-than comparison.
    Gt,
    /// Greater-than-or-equal comparison.
    Ge,
    /// Logical conjunction.
    And,
    /// Logical disjunction.
    Or,
}

/// IR value.
#[derive(Debug, Clone)]
pub enum Value {
    /// Variable reference.
    Var(String),
    /// Integer constant.
    Int(i64),
    /// Float constant.
    Float(f64),
    /// String constant.
    String(String),
    /// Boolean constant.
    Bool(bool),
    /// Null value.
    Null,
}

/// IR type.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Void (no value).
    Void,
    /// Integer.
    Int,
    /// Float.
    Float,
    /// Boolean.
    Bool,
    /// String.
    String,
    /// Pointer to another type.
    Ptr(Box<Type>),
    /// Function type signature.
    Func {
        /// Parameter types.
        params: Vec<Type>,
        /// Return type.
        ret: Box<Type>,
    },
}

/// Global constant.
#[derive(Debug, Clone)]
pub struct Constant {
    /// Constant name.
    pub name: String,
    /// Constant type.
    pub ty: Type,
    /// Constant value.
    pub value: Value,
}

impl Module {
    /// Create a new empty module.
    pub fn new(name: String) -> Self {
        Self {
            name,
            functions: Vec::new(),
            constants: Vec::new(),
        }
    }
}

impl Function {
    /// Create a new function.
    pub fn new(name: String, params: Vec<Parameter>, return_type: Type) -> Self {
        Self {
            name,
            params,
            return_type,
            blocks: Vec::new(),
        }
    }
}

impl BasicBlock {
    /// Create a new basic block.
    pub fn new(label: String) -> Self {
        Self {
            label,
            instructions: Vec::new(),
            terminator: Terminator::Unreachable,
        }
    }
}
