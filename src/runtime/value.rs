use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::{mpsc, Arc, Mutex};
use std::time::Duration;

use crate::parser::ast::Stmt;

#[derive(Debug, Clone)]
pub struct UserFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub captured: HashMap<String, Value>,
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
    receiver: Option<mpsc::Receiver<Result<HttpResponseData, String>>>,
    result: Option<Result<HttpResponseData, String>>,
}

impl PendingHttp {
    pub fn spawn(task: impl FnOnce() -> Result<HttpResponseData, String> + Send + 'static) -> Self {
        let (tx, rx) = mpsc::channel::<Result<HttpResponseData, String>>();
        std::thread::spawn(move || {
            let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(task))
                .map_err(|_| "http request worker panicked".to_string())
                .and_then(|res| res);
            let _ = tx.send(result);
        });
        Self {
            inner: Arc::new(Mutex::new(PendingHttpState {
                receiver: Some(rx),
                result: None,
            })),
        }
    }

    pub fn resolve(&self) -> Result<HttpResponseData, String> {
        self.resolve_with_timeout(None)
    }

    pub fn resolve_with_timeout(
        &self,
        timeout: Option<Duration>,
    ) -> Result<HttpResponseData, String> {
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

        let mut state = self
            .inner
            .lock()
            .map_err(|_| "http request state lock poisoned".to_string())?;
        let Some(receiver) = state.receiver.as_ref() else {
            return Err("http request missing execution handle".to_string());
        };

        let result = match timeout {
            Some(duration) => match receiver.recv_timeout(duration) {
                Ok(result) => result,
                Err(mpsc::RecvTimeoutError::Timeout) => {
                    return Err(format!(
                        "http request timed out after {}ms",
                        duration.as_millis()
                    ));
                }
                Err(mpsc::RecvTimeoutError::Disconnected) => {
                    return Err("http request worker disconnected".to_string());
                }
            },
            None => receiver
                .recv()
                .map_err(|_| "http request worker disconnected".to_string())?,
        };

        state.receiver = None;
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
pub struct Channel {
    queue: Rc<RefCell<VecDeque<Value>>>,
}

impl Channel {
    pub fn new() -> Self {
        Self {
            queue: Rc::new(RefCell::new(VecDeque::new())),
        }
    }

    pub fn send(&self, value: Value) {
        self.queue.borrow_mut().push_back(value);
    }

    pub fn recv(&self) -> Option<Value> {
        self.queue.borrow_mut().pop_front()
    }

    pub fn recv_timeout(&self, timeout: Duration) -> Option<Value> {
        let started = std::time::Instant::now();
        loop {
            if let Some(value) = self.recv() {
                return Some(value);
            }
            if started.elapsed() >= timeout {
                return None;
            }
            std::thread::sleep(Duration::from_millis(1));
        }
    }

    pub fn len(&self) -> usize {
        self.queue.borrow().len()
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
    Channel(Channel),
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
            (Value::Channel(a), Value::Channel(b)) => Rc::ptr_eq(&a.queue, &b.queue),
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
            Value::HttpResponse(_) | Value::HttpPending(_) | Value::Channel(_) => true,
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
            Value::Channel(_) => "channel",
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
            Value::Channel(channel) => write!(f, "<channel len={}>", channel.len()),
        }
    }
}
