use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;

use crate::parser::ast::Stmt;

#[derive(Debug, Clone)]
pub struct UserFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
    Path(PathBuf),
    List(Rc<RefCell<Vec<Value>>>),
    Map(Rc<RefCell<HashMap<String, Value>>>),
    UserFunction(UserFunction),
    NativeFunction(String),
    Module(Rc<HashMap<String, Value>>),
    BoundMethod {
        receiver: Box<Value>,
        method: String,
    },
    Error(String),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Path(a), Value::Path(b)) => a == b,
            (Value::Error(a), Value::Error(b)) => a == b,
            (Value::List(a), Value::List(b)) => *a.borrow() == *b.borrow(),
            (Value::Map(a), Value::Map(b)) => *a.borrow() == *b.borrow(),
            _ => false,
        }
    }
}

impl Value {
    pub fn list(values: Vec<Value>) -> Self {
        Value::List(Rc::new(RefCell::new(values)))
    }

    pub fn map(values: HashMap<String, Value>) -> Self {
        Value::Map(Rc::new(RefCell::new(values)))
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Bool(value) => *value,
            Value::Nil => false,
            Value::Int(value) => *value != 0,
            Value::Float(value) => *value != 0.0,
            Value::String(value) => !value.is_empty(),
            Value::Path(value) => !value.as_os_str().is_empty(),
            Value::List(values) => !values.borrow().is_empty(),
            Value::Map(values) => !values.borrow().is_empty(),
            Value::Error(_) => false,
            Value::UserFunction(_)
            | Value::NativeFunction(_)
            | Value::Module(_)
            | Value::BoundMethod { .. } => true,
        }
    }

    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
            Value::Path(_) => "path",
            Value::List(_) => "list",
            Value::Map(_) => "map",
            Value::UserFunction(_) => "function",
            Value::NativeFunction(_) => "native_function",
            Value::Module(_) => "module",
            Value::BoundMethod { .. } => "bound_method",
            Value::Error(_) => "error",
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::Nil => write!(f, "nil"),
            Value::Path(value) => write!(f, "{}", value.display()),
            Value::List(values) => {
                let rendered = values
                    .borrow()
                    .iter()
                    .map(|value| value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", rendered)
            }
            Value::Map(values) => {
                let rendered = values
                    .borrow()
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{{{}}}", rendered)
            }
            Value::UserFunction(func) => write!(f, "<fn {}>", func.name),
            Value::NativeFunction(name) => write!(f, "<native {}>", name),
            Value::Module(_) => write!(f, "<module>"),
            Value::BoundMethod { method, .. } => write!(f, "<method {}>", method),
            Value::Error(message) => write!(f, "Error({})", message),
        }
    }
}
