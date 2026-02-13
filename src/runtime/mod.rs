pub mod builtins;
pub mod value;

use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use crate::parser::ast::{
    BinaryOp, Expr, MapEntryExpr, MatchArm, Pattern, Program, Stmt, UnaryOp,
};

use self::value::{UserFunction, Value};
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, Clone, Default)]
pub struct Permissions {
    pub allow_all: bool,
    pub allow_read: Vec<PathBuf>,
    pub allow_write: Vec<PathBuf>,
    pub allow_net: Vec<String>,
    pub allow_env: bool,
}

impl Permissions {
    pub fn allow_all() -> Self {
        Self {
            allow_all: true,
            allow_read: Vec::new(),
            allow_write: Vec::new(),
            allow_net: Vec::new(),
            allow_env: true,
        }
    }

    pub fn check_read(&self, path: &Path) -> Result<(), RuntimeError> {
        if self.allow_all || self.is_allowed(path, &self.allow_read) {
            Ok(())
        } else {
            Err(RuntimeError::new(format!(
                "read access denied for '{}'; pass --allow-read=<path> or --allow-all",
                path.display()
            )))
        }
    }

    pub fn check_write(&self, path: &Path) -> Result<(), RuntimeError> {
        if self.allow_all || self.is_allowed(path, &self.allow_write) {
            Ok(())
        } else {
            Err(RuntimeError::new(format!(
                "write access denied for '{}'; pass --allow-write=<path> or --allow-all",
                path.display()
            )))
        }
    }

    pub fn check_env(&self, key: &str) -> Result<(), RuntimeError> {
        if self.allow_all || self.allow_env {
            Ok(())
        } else {
            Err(RuntimeError::new(format!(
                "environment access denied for '{}'; pass --allow-env or --allow-all",
                key
            )))
        }
    }

    fn is_allowed(&self, path: &Path, allowed_roots: &[PathBuf]) -> bool {
        if allowed_roots.is_empty() {
            return false;
        }
        let normalized_path = normalize_path(path);
        allowed_roots.iter().any(|root| normalized_path.starts_with(root))
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
}

impl RuntimeError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "runtime error: {}", self.message)
    }
}

impl Error for RuntimeError {}

enum StmtFlow {
    Continue(Value),
    Return(Value),
}

#[derive(Debug)]
pub struct Runtime {
    scopes: Vec<HashMap<String, Value>>,
    pending_return: Option<Value>,
    permissions: Permissions,
}

impl Runtime {
    pub fn new() -> Self {
        Self::with_permissions(Permissions::default())
    }

    pub fn with_permissions(permissions: Permissions) -> Self {
        Self {
            scopes: vec![builtins::core_globals()],
            pending_return: None,
            permissions,
        }
    }

    pub fn run_program(&mut self, program: &Program) -> Result<Value, RuntimeError> {
        let mut last_value = Value::Nil;
        for statement in &program.statements {
            match self.execute_statement(statement)? {
                StmtFlow::Continue(value) => last_value = value,
                StmtFlow::Return(_) => {
                    return Err(RuntimeError::new("'return' is only valid inside a function"))
                }
            }
        }

        if self.pending_return.is_some() {
            return Err(RuntimeError::new("'return' is only valid inside a function"));
        }

        Ok(last_value)
    }

    fn execute_statement(&mut self, statement: &Stmt) -> Result<StmtFlow, RuntimeError> {
        match statement {
            Stmt::Use { path, alias } => {
                self.execute_use(path, alias.as_deref())?;
                Ok(StmtFlow::Continue(Value::Nil))
            }
            Stmt::VarDecl {
                name, initializer, ..
            } => {
                let value = self.eval_expr(initializer)?;
                if let Some(ret) = self.take_pending_return() {
                    return Ok(StmtFlow::Return(ret));
                }
                self.define(name.clone(), value.clone());
                Ok(StmtFlow::Continue(value))
            }
            Stmt::DestructureDecl {
                pattern,
                initializer,
            } => {
                let value = self.eval_expr(initializer)?;
                if let Some(ret) = self.take_pending_return() {
                    return Ok(StmtFlow::Return(ret));
                }
                self.bind_pattern(pattern, &value)?;
                Ok(StmtFlow::Continue(Value::Nil))
            }
            Stmt::FunctionDef {
                name,
                params,
                body,
                ..
            } => {
                let params = params.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
                let func = UserFunction {
                    name: name.clone(),
                    params,
                    body: body.clone(),
                };
                self.define(name.clone(), Value::UserFunction(func));
                Ok(StmtFlow::Continue(Value::Nil))
            }
            Stmt::Return { value } => {
                let return_value = if let Some(expr) = value {
                    self.eval_expr(expr)?
                } else {
                    Value::Nil
                };
                self.signal_return(return_value.clone());
                Ok(StmtFlow::Return(return_value))
            }
            Stmt::Print { expr } => {
                let value = self.eval_expr(expr)?;
                if let Some(ret) = self.take_pending_return() {
                    return Ok(StmtFlow::Return(ret));
                }
                println!("{}", value);
                Ok(StmtFlow::Continue(value))
            }
            Stmt::Expr(expr) => {
                let value = self.eval_expr(expr)?;
                if let Some(ret) = self.take_pending_return() {
                    return Ok(StmtFlow::Return(ret));
                }
                Ok(StmtFlow::Continue(value))
            }
        }
    }

    fn execute_use(&mut self, path: &[String], alias: Option<&str>) -> Result<(), RuntimeError> {
        if path.is_empty() {
            return Err(RuntimeError::new("use path cannot be empty"));
        }

        let module_name = if path.len() >= 2 && path[0] == "std" {
            path[1].clone()
        } else {
            path[path.len() - 1].clone()
        };

        let module = self
            .lookup(&module_name)
            .ok_or_else(|| RuntimeError::new(format!("unknown module '{}'", module_name)))?;

        let bind_name = alias.unwrap_or(&module_name).to_string();
        self.define(bind_name, module);
        Ok(())
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Int(value) => Ok(Value::Int(*value)),
            Expr::Float(value) => Ok(Value::Float(*value)),
            Expr::String { value, .. } => Ok(Value::String(value.clone())),
            Expr::Bool(value) => Ok(Value::Bool(*value)),
            Expr::Nil => Ok(Value::Nil),
            Expr::Variable(name) => self
                .lookup(name)
                .ok_or_else(|| RuntimeError::new(format!("unknown variable '{}'", name))),
            Expr::Unary { op, rhs } => {
                let rhs_value = self.eval_expr(rhs)?;
                match op {
                    UnaryOp::Negate => match rhs_value {
                        Value::Int(value) => Ok(Value::Int(-value)),
                        Value::Float(value) => Ok(Value::Float(-value)),
                        _ => Err(RuntimeError::new(format!(
                            "operator '-' does not support '{}'",
                            rhs_value.type_name()
                        ))),
                    },
                    UnaryOp::Not => Ok(Value::Bool(!rhs_value.is_truthy())),
                }
            }
            Expr::Binary { lhs, op, rhs } => {
                let lhs_value = self.eval_expr(lhs)?;
                let rhs_value = self.eval_expr(rhs)?;
                self.eval_binary(op, lhs_value, rhs_value)
            }
            Expr::Assign { name, value } => {
                let rhs = self.eval_expr(value)?;
                self.assign(name, rhs.clone())?;
                Ok(rhs)
            }
            Expr::Grouping(inner) => self.eval_expr(inner),
            Expr::Call { callee, args } => {
                let callee_value = self.eval_expr(callee)?;
                let evaluated_args = args
                    .iter()
                    .map(|arg| self.eval_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;
                self.call_value(callee_value, evaluated_args)
            }
            Expr::Member {
                object,
                property,
                optional,
            } => {
                let object_value = self.eval_expr(object)?;
                self.read_member(object_value, property, *optional)
            }
            Expr::Coalesce { lhs, rhs } => {
                let lhs_value = self.eval_expr(lhs)?;
                if matches!(lhs_value, Value::Nil | Value::Error(_)) {
                    self.eval_expr(rhs)
                } else {
                    Ok(lhs_value)
                }
            }
            Expr::OrReturn { lhs, return_value } => {
                let lhs_value = self.eval_expr(lhs)?;
                if matches!(lhs_value, Value::Nil | Value::Error(_)) {
                    let value = self.eval_expr(return_value)?;
                    self.signal_return(value);
                    Ok(Value::Nil)
                } else {
                    Ok(lhs_value)
                }
            }
            Expr::PanicUnwrap(inner) => {
                let value = self.eval_expr(inner)?;
                match value {
                    Value::Nil => Err(RuntimeError::new("panic unwrap failed on nil")),
                    Value::Error(message) => {
                        Err(RuntimeError::new(format!("panic unwrap failed: {}", message)))
                    }
                    other => Ok(other),
                }
            }
            Expr::Match { subject, arms } => {
                let subject_value = self.eval_expr(subject)?;
                self.eval_match(&subject_value, arms)
            }
            Expr::ListLiteral(items) => {
                let values = items
                    .iter()
                    .map(|item| self.eval_expr(item))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(Value::list(values))
            }
            Expr::ListComprehension {
                expr,
                item_name,
                iterable,
                condition,
            } => {
                let iterable_value = self.eval_expr(iterable)?;
                let items = match iterable_value {
                    Value::List(values) => values.borrow().clone(),
                    Value::String(text) => UnicodeSegmentation::graphemes(text.as_str(), true)
                        .map(|g| Value::String(g.to_string()))
                        .collect::<Vec<_>>(),
                    other => {
                        return Err(RuntimeError::new(format!(
                            "list comprehension requires list or string iterable, found '{}'",
                            other.type_name()
                        )))
                    }
                };

                let mut result = Vec::new();
                self.push_scope();
                for item in items {
                    self.define(item_name.clone(), item);
                    if let Some(condition_expr) = condition {
                        let keep = self.eval_expr(condition_expr)?;
                        if !keep.is_truthy() {
                            continue;
                        }
                    }
                    result.push(self.eval_expr(expr)?);
                    if self.pending_return.is_some() {
                        break;
                    }
                }
                self.pop_scope();
                Ok(Value::list(result))
            }
            Expr::MapLiteral(entries) => self.eval_map_literal(entries),
            Expr::Index { object, index } => {
                let object_value = self.eval_expr(object)?;
                let index_value = self.eval_expr(index)?;
                self.eval_index(object_value, index_value)
            }
        }
    }

    fn eval_binary(
        &self,
        op: &BinaryOp,
        lhs_value: Value,
        rhs_value: Value,
    ) -> Result<Value, RuntimeError> {
        match op {
            BinaryOp::Add => match (lhs_value, rhs_value) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                (a, b) => Err(RuntimeError::new(format!(
                    "operator '+' does not support '{}' and '{}'",
                    a.type_name(),
                    b.type_name()
                ))),
            },
            BinaryOp::Subtract => numeric_binary(lhs_value, rhs_value, |a, b| a - b),
            BinaryOp::Multiply => numeric_binary(lhs_value, rhs_value, |a, b| a * b),
            BinaryOp::Divide => numeric_binary(lhs_value, rhs_value, |a, b| a / b),
            BinaryOp::Modulo => numeric_binary(lhs_value, rhs_value, |a, b| a % b),
            BinaryOp::Equal => Ok(Value::Bool(lhs_value == rhs_value)),
            BinaryOp::NotEqual => Ok(Value::Bool(lhs_value != rhs_value)),
            BinaryOp::Less => numeric_compare(lhs_value, rhs_value, |a, b| a < b),
            BinaryOp::LessEqual => numeric_compare(lhs_value, rhs_value, |a, b| a <= b),
            BinaryOp::Greater => numeric_compare(lhs_value, rhs_value, |a, b| a > b),
            BinaryOp::GreaterEqual => numeric_compare(lhs_value, rhs_value, |a, b| a >= b),
        }
    }

    fn eval_match(&mut self, subject_value: &Value, arms: &[MatchArm]) -> Result<Value, RuntimeError> {
        for arm in arms {
            if self.pattern_matches(&arm.pattern, subject_value)? {
                self.push_scope();
                self.bind_pattern(&arm.pattern, subject_value)?;
                let value = self.eval_expr(&arm.value);
                self.pop_scope();
                return value;
            }
        }
        Ok(Value::Nil)
    }

    fn eval_map_literal(&mut self, entries: &[MapEntryExpr]) -> Result<Value, RuntimeError> {
        let mut map = HashMap::new();
        for entry in entries {
            map.insert(entry.key.clone(), self.eval_expr(&entry.value)?);
        }
        Ok(Value::map(map))
    }

    fn eval_index(&self, object: Value, index: Value) -> Result<Value, RuntimeError> {
        match (object, index) {
            (Value::List(list), Value::Int(index)) => {
                if index < 0 {
                    return Ok(Value::Nil);
                }
                Ok(list
                    .borrow()
                    .get(index as usize)
                    .cloned()
                    .unwrap_or(Value::Nil))
            }
            (Value::Map(map), Value::String(key)) => {
                Ok(map.borrow().get(&key).cloned().unwrap_or(Value::Nil))
            }
            (Value::String(text), Value::Int(index)) => {
                if index < 0 {
                    return Ok(Value::Nil);
                }
                Ok(UnicodeSegmentation::graphemes(text.as_str(), true)
                    .nth(index as usize)
                    .map(|g| Value::String(g.to_string()))
                    .unwrap_or(Value::Nil))
            }
            (object, index) => Err(RuntimeError::new(format!(
                "cannot index '{}' with '{}'",
                object.type_name(),
                index.type_name()
            ))),
        }
    }

    fn call_value(&mut self, callee: Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match callee {
            Value::UserFunction(func) => self.call_user_function(func, args),
            Value::NativeFunction(name) => self.call_native(&name, args),
            Value::BoundMethod { receiver, method } => {
                self.call_bound_method(*receiver, &method, args)
            }
            _ => Err(RuntimeError::new(format!(
                "value '{}' is not callable",
                callee.type_name()
            ))),
        }
    }

    fn call_user_function(
        &mut self,
        function: UserFunction,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if function.params.len() != args.len() {
            return Err(RuntimeError::new(format!(
                "function '{}' expected {} argument(s), got {}",
                function.name,
                function.params.len(),
                args.len()
            )));
        }

        self.push_scope();
        for (name, value) in function.params.iter().zip(args) {
            self.define(name.clone(), value);
        }

        let mut return_value = Value::Nil;
        for statement in &function.body {
            match self.execute_statement(statement)? {
                StmtFlow::Continue(_) => {}
                StmtFlow::Return(value) => {
                    return_value = value;
                    self.pending_return = None;
                    self.pop_scope();
                    return Ok(return_value);
                }
            }
        }

        self.pending_return = None;
        self.pop_scope();
        Ok(return_value)
    }

    fn call_native(&mut self, name: &str, args: Vec<Value>) -> Result<Value, RuntimeError> {
        match name {
            "core.list" => Ok(Value::list(args)),
            "core.map" => self.native_map_constructor(args),
            "core.Path" => self.native_path_constructor(args),
            "core.len" => {
                expect_arity(name, &args, 1)?;
                match &args[0] {
                    Value::String(text) => Ok(Value::Int(
                        UnicodeSegmentation::graphemes(text.as_str(), true).count() as i64,
                    )),
                    Value::Path(path) => Ok(Value::Int(
                        path.as_os_str().to_string_lossy().chars().count() as i64,
                    )),
                    Value::List(values) => Ok(Value::Int(values.borrow().len() as i64)),
                    Value::Map(values) => Ok(Value::Int(values.borrow().len() as i64)),
                    other => Err(RuntimeError::new(format!(
                        "len() does not support '{}'",
                        other.type_name()
                    ))),
                }
            }
            "math.min" => native_math_min(args),
            "math.max" => native_math_max(args),
            "math.abs" => native_math_abs(args),
            "math.round" => native_math_round(args),
            "fs.read" => self.native_fs_read(args),
            "fs.write" => self.native_fs_write(args),
            "fs.exists" => self.native_fs_exists(args),
            "fs.delete" => self.native_fs_delete(args),
            "json.parse" => native_json_parse(args),
            "json.stringify" => native_json_stringify(args),
            "path.join" => self.native_path_join(args),
            "path.normalize" => self.native_path_normalize(args),
            "path.basename" => self.native_path_basename(args),
            "path.dirname" => self.native_path_dirname(args),
            "path.cwd" => self.native_path_cwd(args),
            "path.to_string" => self.native_path_to_string(args),
            "env.get" => self.native_env_get(args),
            other => Err(RuntimeError::new(format!(
                "unknown native function '{}'",
                other
            ))),
        }
    }

    fn call_bound_method(
        &mut self,
        receiver: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match receiver {
            Value::String(text) => self.call_string_method(text, method, args),
            Value::List(values) => self.call_list_method(values, method, args),
            Value::Map(values) => self.call_map_method(values, method, args),
            Value::Path(path) => self.call_path_method(path, method, args),
            other => Err(RuntimeError::new(format!(
                "type '{}' has no methods",
                other.type_name()
            ))),
        }
    }

    fn call_string_method(
        &mut self,
        text: String,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "trim" => {
                expect_arity("string.trim", &args, 0)?;
                Ok(Value::String(text.trim().to_string()))
            }
            "split" => {
                expect_arity("string.split", &args, 1)?;
                let delimiter = expect_string(&args[0], "string.split delimiter")?;
                let parts = text
                    .split(&delimiter)
                    .map(|part| Value::String(part.to_string()))
                    .collect::<Vec<_>>();
                Ok(Value::list(parts))
            }
            "replace" => {
                expect_arity("string.replace", &args, 2)?;
                let from = expect_string(&args[0], "string.replace from")?;
                let to = expect_string(&args[1], "string.replace to")?;
                Ok(Value::String(text.replace(&from, &to)))
            }
            "uppercase" => {
                expect_arity("string.uppercase", &args, 0)?;
                Ok(Value::String(text.to_uppercase()))
            }
            "lowercase" => {
                expect_arity("string.lowercase", &args, 0)?;
                Ok(Value::String(text.to_lowercase()))
            }
            other => Err(RuntimeError::new(format!(
                "unknown string method '{}'",
                other
            ))),
        }
    }

    fn call_list_method(
        &mut self,
        values: std::rc::Rc<std::cell::RefCell<Vec<Value>>>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "push" => {
                expect_arity("list.push", &args, 1)?;
                values.borrow_mut().push(args[0].clone());
                Ok(Value::Nil)
            }
            "pop" => {
                expect_arity("list.pop", &args, 0)?;
                Ok(values.borrow_mut().pop().unwrap_or(Value::Nil))
            }
            "map" => {
                expect_arity("list.map", &args, 1)?;
                let callback = args[0].clone();
                let snapshot = values.borrow().clone();
                let mut result = Vec::new();
                for item in snapshot {
                    result.push(self.call_value(callback.clone(), vec![item])?);
                }
                Ok(Value::list(result))
            }
            "filter" => {
                expect_arity("list.filter", &args, 1)?;
                let callback = args[0].clone();
                let snapshot = values.borrow().clone();
                let mut result = Vec::new();
                for item in snapshot {
                    let keep = self.call_value(callback.clone(), vec![item.clone()])?;
                    if keep.is_truthy() {
                        result.push(item);
                    }
                }
                Ok(Value::list(result))
            }
            "reduce" => {
                if args.len() != 2 {
                    return Err(RuntimeError::new(
                        "list.reduce expects 2 arguments: callback, initial",
                    ));
                }
                let callback = args[0].clone();
                let mut accumulator = args[1].clone();
                for item in values.borrow().iter().cloned() {
                    accumulator = self.call_value(callback.clone(), vec![accumulator, item])?;
                }
                Ok(accumulator)
            }
            "sort" => {
                expect_arity("list.sort", &args, 0)?;
                let mut borrowed = values.borrow_mut();
                sort_values(&mut borrowed)?;
                Ok(Value::Nil)
            }
            "sorted" => {
                expect_arity("list.sorted", &args, 0)?;
                let mut cloned = values.borrow().clone();
                sort_values(&mut cloned)?;
                Ok(Value::list(cloned))
            }
            other => Err(RuntimeError::new(format!("unknown list method '{}'", other))),
        }
    }

    fn call_map_method(
        &mut self,
        values: std::rc::Rc<std::cell::RefCell<HashMap<String, Value>>>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "get" => {
                expect_arity("map.get", &args, 1)?;
                let key = expect_string(&args[0], "map.get key")?;
                Ok(values.borrow().get(&key).cloned().unwrap_or(Value::Nil))
            }
            "set" => {
                expect_arity("map.set", &args, 2)?;
                let key = expect_string(&args[0], "map.set key")?;
                values.borrow_mut().insert(key, args[1].clone());
                Ok(Value::Nil)
            }
            "has" => {
                expect_arity("map.has", &args, 1)?;
                let key = expect_string(&args[0], "map.has key")?;
                Ok(Value::Bool(values.borrow().contains_key(&key)))
            }
            "keys" => {
                expect_arity("map.keys", &args, 0)?;
                let keys = values
                    .borrow()
                    .keys()
                    .cloned()
                    .map(Value::String)
                    .collect::<Vec<_>>();
                Ok(Value::list(keys))
            }
            "values" => {
                expect_arity("map.values", &args, 0)?;
                let all_values = values.borrow().values().cloned().collect::<Vec<_>>();
                Ok(Value::list(all_values))
            }
            other => Err(RuntimeError::new(format!("unknown map method '{}'", other))),
        }
    }

    fn call_path_method(
        &mut self,
        path: PathBuf,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "join" => {
                if args.is_empty() {
                    return Err(RuntimeError::new("path.join expects at least 1 argument"));
                }
                let mut out = path;
                for value in args {
                    out = out.join(value_to_path(&value, "path.join segment")?);
                }
                Ok(Value::Path(normalize_path(&out)))
            }
            "normalize" => {
                expect_arity("path.normalize", &args, 0)?;
                Ok(Value::Path(normalize_path(&path)))
            }
            "basename" => {
                expect_arity("path.basename", &args, 0)?;
                let name = path
                    .file_name()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_default();
                Ok(Value::String(name))
            }
            "dirname" => {
                expect_arity("path.dirname", &args, 0)?;
                let parent = path.parent().map(Path::to_path_buf).unwrap_or_default();
                Ok(Value::Path(parent))
            }
            "to_string" => {
                expect_arity("path.to_string", &args, 0)?;
                Ok(Value::String(path.to_string_lossy().to_string()))
            }
            other => Err(RuntimeError::new(format!("unknown path method '{}'", other))),
        }
    }

    fn native_map_constructor(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() % 2 != 0 {
            return Err(RuntimeError::new(
                "map() expects an even number of arguments: key, value, ...",
            ));
        }
        let mut map = HashMap::new();
        let mut index = 0;
        while index < args.len() {
            let key = expect_string(&args[index], "map key")?;
            map.insert(key, args[index + 1].clone());
            index += 2;
        }
        Ok(Value::map(map))
    }

    fn native_path_constructor(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("Path", &args, 1)?;
        let path = value_to_path(&args[0], "Path argument")?;
        Ok(Value::Path(normalize_path(&path)))
    }

    fn native_path_join(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Err(RuntimeError::new(
                "path.join expects at least 2 arguments: base, segment...",
            ));
        }
        let mut base = value_to_path(&args[0], "path.join base")?;
        for value in args.iter().skip(1) {
            base = base.join(value_to_path(value, "path.join segment")?);
        }
        Ok(Value::Path(normalize_path(&base)))
    }

    fn native_path_normalize(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("path.normalize", &args, 1)?;
        let path = value_to_path(&args[0], "path.normalize path")?;
        Ok(Value::Path(normalize_path(&path)))
    }

    fn native_path_basename(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("path.basename", &args, 1)?;
        let path = value_to_path(&args[0], "path.basename path")?;
        let name = path
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_default();
        Ok(Value::String(name))
    }

    fn native_path_dirname(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("path.dirname", &args, 1)?;
        let path = value_to_path(&args[0], "path.dirname path")?;
        let parent = path.parent().map(Path::to_path_buf).unwrap_or_default();
        Ok(Value::Path(parent))
    }

    fn native_path_cwd(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("path.cwd", &args, 0)?;
        let cwd = std::env::current_dir()
            .map_err(|err| RuntimeError::new(format!("path.cwd failed: {}", err)))?;
        Ok(Value::Path(cwd))
    }

    fn native_path_to_string(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("path.to_string", &args, 1)?;
        let path = value_to_path(&args[0], "path.to_string path")?;
        Ok(Value::String(path.to_string_lossy().to_string()))
    }

    fn native_env_get(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("env.get", &args, 1)?;
        let key = expect_string(&args[0], "env.get key")?;
        self.permissions.check_env(&key)?;
        match std::env::var(&key) {
            Ok(value) => Ok(Value::String(value)),
            Err(std::env::VarError::NotPresent) => Ok(Value::Nil),
            Err(err) => Err(RuntimeError::new(format!("env.get failed: {}", err))),
        }
    }

    fn native_fs_read(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("fs.read", &args, 1)?;
        let path = value_to_path(&args[0], "fs.read path")?;
        self.permissions.check_read(&path)?;
        std::fs::read_to_string(&path)
            .map(Value::String)
            .map_err(|err| RuntimeError::new(format!("fs.read failed: {}", err)))
    }

    fn native_fs_write(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("fs.write", &args, 2)?;
        let path = value_to_path(&args[0], "fs.write path")?;
        let content = expect_string(&args[1], "fs.write content")?;
        self.permissions.check_write(&path)?;
        std::fs::write(&path, content)
            .map(|_| Value::Nil)
            .map_err(|err| RuntimeError::new(format!("fs.write failed: {}", err)))
    }

    fn native_fs_exists(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("fs.exists", &args, 1)?;
        let path = value_to_path(&args[0], "fs.exists path")?;
        self.permissions.check_read(&path)?;
        Ok(Value::Bool(path.exists()))
    }

    fn native_fs_delete(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("fs.delete", &args, 1)?;
        let path = value_to_path(&args[0], "fs.delete path")?;
        self.permissions.check_write(&path)?;
        if !path.exists() {
            return Ok(Value::Nil);
        }
        if path.is_dir() {
            std::fs::remove_dir_all(&path)
                .map(|_| Value::Nil)
                .map_err(|err| RuntimeError::new(format!("fs.delete failed: {}", err)))
        } else {
            std::fs::remove_file(&path)
                .map(|_| Value::Nil)
                .map_err(|err| RuntimeError::new(format!("fs.delete failed: {}", err)))
        }
    }

    fn read_member(
        &self,
        object: Value,
        property: &str,
        optional: bool,
    ) -> Result<Value, RuntimeError> {
        if optional && matches!(object, Value::Nil) {
            return Ok(Value::Nil);
        }

        match object {
            Value::Module(module) => module
                .get(property)
                .cloned()
                .ok_or_else(|| RuntimeError::new(format!("module has no member '{}'", property))),
            Value::Map(values) => {
                let existing = {
                    let borrowed = values.borrow();
                    borrowed.get(property).cloned()
                };
                if let Some(value) = existing {
                    Ok(value)
                } else {
                    Ok(Value::BoundMethod {
                        receiver: Box::new(Value::Map(values)),
                        method: property.to_string(),
                    })
                }
            }
            Value::String(text) => Ok(Value::BoundMethod {
                receiver: Box::new(Value::String(text)),
                method: property.to_string(),
            }),
            Value::List(values) => Ok(Value::BoundMethod {
                receiver: Box::new(Value::List(values)),
                method: property.to_string(),
            }),
            Value::Path(path) => Ok(Value::BoundMethod {
                receiver: Box::new(Value::Path(path)),
                method: property.to_string(),
            }),
            Value::Nil if optional => Ok(Value::Nil),
            other => Err(RuntimeError::new(format!(
                "type '{}' has no member '{}'",
                other.type_name(),
                property
            ))),
        }
    }

    fn pattern_matches(&self, pattern: &Pattern, value: &Value) -> Result<bool, RuntimeError> {
        match pattern {
            Pattern::Wildcard | Pattern::Identifier(_) => Ok(true),
            Pattern::Int(expected) => Ok(matches!(value, Value::Int(v) if v == expected)),
            Pattern::Float(expected) => Ok(matches!(value, Value::Float(v) if v == expected)),
            Pattern::String(expected) => Ok(matches!(value, Value::String(v) if v == expected)),
            Pattern::Bool(expected) => Ok(matches!(value, Value::Bool(v) if v == expected)),
            Pattern::Nil => Ok(matches!(value, Value::Nil)),
            Pattern::List(patterns) => {
                let Value::List(values) = value else {
                    return Ok(false);
                };
                let borrowed = values.borrow();
                if borrowed.len() != patterns.len() {
                    return Ok(false);
                }
                for (pattern, item) in patterns.iter().zip(borrowed.iter()) {
                    if !self.pattern_matches(pattern, item)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            Pattern::Map(entries) => {
                let Value::Map(values) = value else {
                    return Ok(false);
                };
                let borrowed = values.borrow();
                for entry in entries {
                    let Some(item_value) = borrowed.get(&entry.key) else {
                        return Ok(false);
                    };
                    if !self.pattern_matches(&entry.pattern, item_value)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern, value: &Value) -> Result<(), RuntimeError> {
        match pattern {
            Pattern::Wildcard => Ok(()),
            Pattern::Identifier(name) => {
                self.define(name.clone(), value.clone());
                Ok(())
            }
            Pattern::Int(expected) => {
                if matches!(value, Value::Int(v) if v == expected) {
                    Ok(())
                } else {
                    Err(RuntimeError::new("pattern binding failed for int"))
                }
            }
            Pattern::Float(expected) => {
                if matches!(value, Value::Float(v) if v == expected) {
                    Ok(())
                } else {
                    Err(RuntimeError::new("pattern binding failed for float"))
                }
            }
            Pattern::String(expected) => {
                if matches!(value, Value::String(v) if v == expected) {
                    Ok(())
                } else {
                    Err(RuntimeError::new("pattern binding failed for string"))
                }
            }
            Pattern::Bool(expected) => {
                if matches!(value, Value::Bool(v) if v == expected) {
                    Ok(())
                } else {
                    Err(RuntimeError::new("pattern binding failed for bool"))
                }
            }
            Pattern::Nil => {
                if matches!(value, Value::Nil) {
                    Ok(())
                } else {
                    Err(RuntimeError::new("pattern binding failed for nil"))
                }
            }
            Pattern::List(patterns) => {
                let Value::List(values) = value else {
                    return Err(RuntimeError::new("cannot bind list pattern to non-list value"));
                };
                let borrowed = values.borrow();
                if borrowed.len() != patterns.len() {
                    return Err(RuntimeError::new("list pattern length mismatch"));
                }
                for (pattern, item) in patterns.iter().zip(borrowed.iter()) {
                    self.bind_pattern(pattern, item)?;
                }
                Ok(())
            }
            Pattern::Map(entries) => {
                let Value::Map(values) = value else {
                    return Err(RuntimeError::new("cannot bind map pattern to non-map value"));
                };
                let borrowed = values.borrow();
                for entry in entries {
                    let item = borrowed.get(&entry.key).ok_or_else(|| {
                        RuntimeError::new(format!(
                            "map pattern key '{}' missing in value",
                            entry.key
                        ))
                    })?;
                    self.bind_pattern(&entry.pattern, item)?;
                }
                Ok(())
            }
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }

    fn define(&mut self, name: String, value: Value) {
        self.scopes
            .last_mut()
            .expect("scope exists")
            .insert(name, value);
    }

    fn assign(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            }
        }
        Err(RuntimeError::new(format!("unknown variable '{}'", name)))
    }

    fn lookup(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }
        None
    }

    fn signal_return(&mut self, value: Value) {
        self.pending_return = Some(value);
    }

    fn take_pending_return(&mut self) -> Option<Value> {
        self.pending_return.take()
    }
}

fn expect_arity(name: &str, args: &[Value], expected: usize) -> Result<(), RuntimeError> {
    if args.len() == expected {
        Ok(())
    } else {
        Err(RuntimeError::new(format!(
            "{} expected {} argument(s), got {}",
            name,
            expected,
            args.len()
        )))
    }
}

fn expect_string(value: &Value, label: &str) -> Result<String, RuntimeError> {
    if let Value::String(text) = value {
        Ok(text.clone())
    } else {
        Err(RuntimeError::new(format!(
            "{} expected string, got {}",
            label,
            value.type_name()
        )))
    }
}

fn value_to_path(value: &Value, label: &str) -> Result<PathBuf, RuntimeError> {
    match value {
        Value::String(text) => Ok(PathBuf::from(text)),
        Value::Path(path) => Ok(path.clone()),
        _ => Err(RuntimeError::new(format!(
            "{} expected string/path, got {}",
            label,
            value.type_name()
        ))),
    }
}

fn normalize_path(path: &Path) -> PathBuf {
    let input = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()
            .unwrap_or_else(|_| PathBuf::from("."))
            .join(path)
    };

    let mut normalized = PathBuf::new();
    for component in input.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                normalized.pop();
            }
            _ => normalized.push(component.as_os_str()),
        }
    }
    normalized
}

fn to_f64(value: &Value) -> Option<f64> {
    match value {
        Value::Int(v) => Some(*v as f64),
        Value::Float(v) => Some(*v),
        _ => None,
    }
}

fn numeric_binary(
    lhs: Value,
    rhs: Value,
    op: impl Fn(f64, f64) -> f64,
) -> Result<Value, RuntimeError> {
    let Some(lhs_num) = to_f64(&lhs) else {
        return Err(RuntimeError::new(format!(
            "expected numeric lhs, got {}",
            lhs.type_name()
        )));
    };
    let Some(rhs_num) = to_f64(&rhs) else {
        return Err(RuntimeError::new(format!(
            "expected numeric rhs, got {}",
            rhs.type_name()
        )));
    };

    let result = op(lhs_num, rhs_num);
    if matches!(lhs, Value::Int(_)) && matches!(rhs, Value::Int(_)) && result.fract() == 0.0 {
        Ok(Value::Int(result as i64))
    } else {
        Ok(Value::Float(result))
    }
}

fn numeric_compare(
    lhs: Value,
    rhs: Value,
    compare: impl Fn(f64, f64) -> bool,
) -> Result<Value, RuntimeError> {
    let Some(lhs_num) = to_f64(&lhs) else {
        return Err(RuntimeError::new(format!(
            "expected numeric lhs, got {}",
            lhs.type_name()
        )));
    };
    let Some(rhs_num) = to_f64(&rhs) else {
        return Err(RuntimeError::new(format!(
            "expected numeric rhs, got {}",
            rhs.type_name()
        )));
    };
    Ok(Value::Bool(compare(lhs_num, rhs_num)))
}

fn sort_values(values: &mut [Value]) -> Result<(), RuntimeError> {
    if values.is_empty() {
        return Ok(());
    }

    if values
        .iter()
        .all(|value| matches!(value, Value::Int(_) | Value::Float(_)))
    {
        values.sort_by(|a, b| {
            to_f64(a)
                .expect("numeric")
                .partial_cmp(&to_f64(b).expect("numeric"))
                .expect("valid float comparison")
        });
        return Ok(());
    }

    if values.iter().all(|value| matches!(value, Value::String(_))) {
        values.sort_by(|a, b| {
            let Value::String(a) = a else { unreachable!() };
            let Value::String(b) = b else { unreachable!() };
            a.cmp(b)
        });
        return Ok(());
    }

    Err(RuntimeError::new(
        "list.sort/sorted only supports uniform numeric or string lists",
    ))
}

fn native_math_min(args: Vec<Value>) -> Result<Value, RuntimeError> {
    expect_arity("math.min", &args, 2)?;
    let a = to_f64(&args[0]).ok_or_else(|| RuntimeError::new("math.min expected numeric arg 1"))?;
    let b = to_f64(&args[1]).ok_or_else(|| RuntimeError::new("math.min expected numeric arg 2"))?;
    Ok(Value::Float(a.min(b)))
}

fn native_math_max(args: Vec<Value>) -> Result<Value, RuntimeError> {
    expect_arity("math.max", &args, 2)?;
    let a = to_f64(&args[0]).ok_or_else(|| RuntimeError::new("math.max expected numeric arg 1"))?;
    let b = to_f64(&args[1]).ok_or_else(|| RuntimeError::new("math.max expected numeric arg 2"))?;
    Ok(Value::Float(a.max(b)))
}

fn native_math_abs(args: Vec<Value>) -> Result<Value, RuntimeError> {
    expect_arity("math.abs", &args, 1)?;
    match &args[0] {
        Value::Int(value) => Ok(Value::Int(value.abs())),
        Value::Float(value) => Ok(Value::Float(value.abs())),
        _ => Err(RuntimeError::new("math.abs expected numeric argument")),
    }
}

fn native_math_round(args: Vec<Value>) -> Result<Value, RuntimeError> {
    expect_arity("math.round", &args, 1)?;
    let value =
        to_f64(&args[0]).ok_or_else(|| RuntimeError::new("math.round expected numeric argument"))?;
    Ok(Value::Int(value.round() as i64))
}

fn native_json_parse(args: Vec<Value>) -> Result<Value, RuntimeError> {
    expect_arity("json.parse", &args, 1)?;
    let text = expect_string(&args[0], "json.parse input")?;
    let parsed: serde_json::Value = serde_json::from_str(&text)
        .map_err(|err| RuntimeError::new(format!("json.parse failed: {}", err)))?;
    Ok(json_to_value(parsed))
}

fn native_json_stringify(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.is_empty() || args.len() > 2 {
        return Err(RuntimeError::new(
            "json.stringify expects 1 or 2 arguments: value, pretty(bool?)",
        ));
    }
    let json_value = value_to_json(&args[0])?;
    let pretty = if args.len() == 2 {
        matches!(args[1], Value::Bool(true))
    } else {
        false
    };

    let encoded = if pretty {
        serde_json::to_string_pretty(&json_value)
    } else {
        serde_json::to_string(&json_value)
    }
    .map_err(|err| RuntimeError::new(format!("json.stringify failed: {}", err)))?;
    Ok(Value::String(encoded))
}

fn json_to_value(value: serde_json::Value) -> Value {
    match value {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(v) => Value::Bool(v),
        serde_json::Value::Number(num) => {
            if let Some(i) = num.as_i64() {
                Value::Int(i)
            } else if let Some(f) = num.as_f64() {
                Value::Float(f)
            } else {
                Value::Nil
            }
        }
        serde_json::Value::String(v) => Value::String(v),
        serde_json::Value::Array(items) => Value::list(items.into_iter().map(json_to_value).collect()),
        serde_json::Value::Object(entries) => {
            let mut map = HashMap::new();
            for (k, v) in entries {
                map.insert(k, json_to_value(v));
            }
            Value::map(map)
        }
    }
}

fn value_to_json(value: &Value) -> Result<serde_json::Value, RuntimeError> {
    match value {
        Value::Nil => Ok(serde_json::Value::Null),
        Value::Bool(v) => Ok(serde_json::Value::Bool(*v)),
        Value::Int(v) => Ok(serde_json::Value::Number((*v).into())),
        Value::Float(v) => {
            let number = serde_json::Number::from_f64(*v)
                .ok_or_else(|| RuntimeError::new("json.stringify cannot encode NaN/Infinity"))?;
            Ok(serde_json::Value::Number(number))
        }
        Value::String(v) => Ok(serde_json::Value::String(v.clone())),
        Value::Path(v) => Ok(serde_json::Value::String(v.to_string_lossy().to_string())),
        Value::List(values) => {
            let converted = values
                .borrow()
                .iter()
                .map(value_to_json)
                .collect::<Result<Vec<_>, _>>()?;
            Ok(serde_json::Value::Array(converted))
        }
        Value::Map(values) => {
            let mut object = serde_json::Map::new();
            for (k, v) in values.borrow().iter() {
                object.insert(k.clone(), value_to_json(v)?);
            }
            Ok(serde_json::Value::Object(object))
        }
        Value::Error(message) => Ok(serde_json::Value::String(format!("Error({})", message))),
        Value::UserFunction(_)
        | Value::NativeFunction(_)
        | Value::Module(_)
        | Value::BoundMethod { .. } => Err(RuntimeError::new(format!(
            "json.stringify cannot encode '{}'",
            value.type_name()
        ))),
    }
}
