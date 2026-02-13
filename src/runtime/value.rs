use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;

use crate::parser::ast::Stmt;

#[derive(Debug, Clone)]
pub struct UserFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HttpResponseData {
    pub status: i64,
    pub body: String,
    pub headers: HashMap<String, String>,
    pub url: String,
}

#[derive(Clone)]
pub struct PendingHttp {
    inner: Arc<Mutex<PendingHttpState>>,
}

struct PendingHttpState {
    handle: Option<JoinHandle<Result<HttpResponseData, String>>>,
    result: Option<Result<HttpResponseData, String>>,
}

impl PendingHttp {
    pub fn spawn(task: impl FnOnce() -> Result<HttpResponseData, String> + Send + 'static) -> Self {
        let handle = std::thread::spawn(task);
        Self {
            inner: Arc::new(Mutex::new(PendingHttpState {
                handle: Some(handle),
                result: None,
            })),
        }
    }

    pub fn resolve(&self) -> Result<HttpResponseData, String> {
        let maybe_cached = {
            let state = self
                .inner
                .lock()
                .map_err(|_| "http request state lock poisoned".to_string())?;
            state.result.clone()
        };
        if let Some(result) = maybe_cached {
            return result;
        }

        let handle = {
            let mut state = self
                .inner
                .lock()
                .map_err(|_| "http request state lock poisoned".to_string())?;
            state.handle.take()
        };
        let Some(handle) = handle else {
            return Err("http request missing execution handle".to_string());
        };

        let result = handle
            .join()
            .map_err(|_| "http request worker panicked".to_string())?;

        let mut state = self
            .inner
            .lock()
            .map_err(|_| "http request state lock poisoned".to_string())?;
        state.result = Some(result.clone());
        result
    }
}

impl fmt::Debug for PendingHttp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PendingHttp")
    }
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
    HttpResponse(HttpResponseData),
    HttpPending(PendingHttp),
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
            (Value::HttpResponse(a), Value::HttpResponse(b)) => a == b,
            (Value::HttpPending(a), Value::HttpPending(b)) => Arc::ptr_eq(&a.inner, &b.inner),
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
            Value::HttpResponse(_) | Value::HttpPending(_) => true,
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
            Value::HttpResponse(_) => "http_response",
            Value::HttpPending(_) => "http_pending",
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
            Value::HttpResponse(response) => {
                write!(f, "<http {} {}>", response.status, response.url)
            }
            Value::HttpPending(_) => write!(f, "<http pending>"),
        }
    }
}
