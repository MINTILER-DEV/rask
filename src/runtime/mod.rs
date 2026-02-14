pub mod builtins;
pub mod value;

use std::collections::HashMap;
use std::error::Error;
use std::fmt;
#[cfg(feature = "net")]
use std::fs;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::Duration;

use crate::parser::ast::{
    BinaryOp, Expr, MapEntryExpr, MatchArm, Pattern, Program, Stmt, UnaryOp, UseTarget,
};

use self::value::{Channel, HttpResponseData, PendingHttp, UserFunction, Value};
#[cfg(feature = "crypto")]
use sha2::{Digest, Sha256};
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug, Clone, Default)]
pub struct Permissions {
    pub allow_all: bool,
    pub allow_read: Vec<PathBuf>,
    pub allow_write: Vec<PathBuf>,
    pub allow_net: Vec<String>,
    pub allow_env: bool,
    pub prompt_permissions: bool,
}

impl Permissions {
    pub fn allow_all() -> Self {
        Self {
            allow_all: true,
            allow_read: Vec::new(),
            allow_write: Vec::new(),
            allow_net: Vec::new(),
            allow_env: true,
            prompt_permissions: false,
        }
    }

    pub fn check_read(&self, path: &Path) -> Result<(), RuntimeError> {
        if self.allow_all || self.is_allowed(path, &self.allow_read) {
            Ok(())
        } else if self.prompt_once("read", &path.display().to_string()) {
            Ok(())
        } else {
            Err(
                RuntimeError::new(format!("read access denied for '{}'", path.display()))
                    .with_code("RUNTIME0401")
                    .with_hint("pass --allow-read=<path>, --prompt-permissions, or --allow-all"),
            )
        }
    }

    pub fn check_write(&self, path: &Path) -> Result<(), RuntimeError> {
        if self.allow_all || self.is_allowed(path, &self.allow_write) {
            Ok(())
        } else if self.prompt_once("write", &path.display().to_string()) {
            Ok(())
        } else {
            Err(
                RuntimeError::new(format!("write access denied for '{}'", path.display()))
                    .with_code("RUNTIME0402")
                    .with_hint("pass --allow-write=<path>, --prompt-permissions, or --allow-all"),
            )
        }
    }

    pub fn check_env(&self, key: &str) -> Result<(), RuntimeError> {
        if self.allow_all || self.allow_env {
            Ok(())
        } else if self.prompt_once("env", key) {
            Ok(())
        } else {
            Err(
                RuntimeError::new(format!("environment access denied for '{}'", key))
                    .with_code("RUNTIME0403")
                    .with_hint("pass --allow-env, --prompt-permissions, or --allow-all"),
            )
        }
    }

    pub fn check_net(&self, url: &str) -> Result<(), RuntimeError> {
        if self.allow_all {
            return Ok(());
        }

        if self.allow_net.is_empty() {
            if self.prompt_once("net", url) {
                return Ok(());
            }
            return Err(
                RuntimeError::new(format!("network access denied for '{}'", url))
                    .with_code("RUNTIME0404")
                    .with_hint("pass --allow-net=<domain>, --prompt-permissions, or --allow-all"),
            );
        }

        let (host, port) = extract_host_and_port(url)
            .ok_or_else(|| RuntimeError::new(format!("invalid URL '{}'", url)))?;
        let host_port = if let Some(port) = port {
            format!("{}:{}", host, port)
        } else {
            host.clone()
        };

        let allowed = self
            .allow_net
            .iter()
            .map(|entry| normalize_allowed_net_entry(entry))
            .any(|entry| {
                if entry == "*" {
                    return true;
                }
                if entry.contains(':') {
                    return entry == host_port;
                }
                host == entry || host.ends_with(&format!(".{}", entry))
            });

        if allowed {
            Ok(())
        } else if self.prompt_once("net", &host) {
            Ok(())
        } else {
            Err(RuntimeError::new(format!(
                "network access denied for host '{}'; allowed: {}",
                host,
                self.allow_net.join(",")
            ))
            .with_code("RUNTIME0404")
            .with_hint(
                "allow this host with --allow-net=<domain>, --prompt-permissions, or --allow-all",
            ))
        }
    }

    fn is_allowed(&self, path: &Path, allowed_roots: &[PathBuf]) -> bool {
        if allowed_roots.is_empty() {
            return false;
        }
        let normalized_path = normalize_path(path);
        allowed_roots
            .iter()
            .any(|root| normalized_path.starts_with(root))
    }

    fn prompt_once(&self, access_kind: &str, target: &str) -> bool {
        if !self.prompt_permissions {
            return false;
        }
        prompt_permission(access_kind, target)
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub message: String,
    pub code: &'static str,
    pub hint: Option<String>,
    pub docs_url: Option<String>,
    pub stack_frames: Vec<String>,
}

impl RuntimeError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            code: "RUNTIME0001",
            hint: None,
            docs_url: Some("https://scalf-lang.dev/errors/RUNTIME0001".to_string()),
            stack_frames: Vec::new(),
        }
    }

    fn with_code(mut self, code: &'static str) -> Self {
        self.code = code;
        self.docs_url = Some(format!("https://scalf-lang.dev/errors/{}", code));
        self
    }

    fn with_hint(mut self, hint: impl Into<String>) -> Self {
        self.hint = Some(hint.into());
        self
    }

    fn with_stack_frames(mut self, frames: Vec<String>) -> Self {
        if self.stack_frames.is_empty() {
            self.stack_frames = frames;
        }
        self
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "runtime error [{}]: {}", self.code, self.message)?;
        if let Some(hint) = &self.hint {
            writeln!(f, "help: {}", hint)?;
        }
        if !self.stack_frames.is_empty() {
            writeln!(f, "stack trace (most recent call first):")?;
            for frame in &self.stack_frames {
                writeln!(f, "  at {}", frame)?;
            }
        }
        if let Some(url) = &self.docs_url {
            write!(f, "docs: {}", url)?;
        }
        Ok(())
    }
}

impl Error for RuntimeError {}

enum StmtFlow {
    Continue(Value),
    Return(Value),
}

#[derive(Debug, Clone)]
pub struct TestCaseResult {
    pub name: String,
    pub passed: bool,
    pub error: Option<RuntimeError>,
}

#[derive(Debug, Clone)]
pub struct TestReport {
    pub passed: usize,
    pub failed: usize,
    pub results: Vec<TestCaseResult>,
}

#[derive(Debug)]
pub struct Runtime {
    scopes: Vec<HashMap<String, Value>>,
    pending_return: Option<Value>,
    permissions: Permissions,
    implicit_nil_for_unknown_variables: bool,
    #[cfg(feature = "net")]
    loaded_url_modules: HashMap<String, Value>,
    #[cfg(feature = "net")]
    import_stack: Vec<String>,
    call_stack: Vec<String>,
    source_label: String,
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
            implicit_nil_for_unknown_variables: false,
            #[cfg(feature = "net")]
            loaded_url_modules: HashMap::new(),
            #[cfg(feature = "net")]
            import_stack: Vec::new(),
            call_stack: Vec::new(),
            source_label: "<script>".to_string(),
        }
    }

    pub fn with_source_label(mut self, label: impl Into<String>) -> Self {
        self.source_label = label.into();
        self
    }

    pub fn with_implicit_nil_for_unknown_variables(mut self, enabled: bool) -> Self {
        self.implicit_nil_for_unknown_variables = enabled;
        self
    }

    pub fn run_program(&mut self, program: &Program) -> Result<Value, RuntimeError> {
        let mut last_value = Value::Nil;
        for statement in &program.statements {
            let flow = self
                .execute_statement(statement)
                .map_err(|err| self.decorate_error(err))?;
            match flow {
                StmtFlow::Continue(value) => last_value = value,
                StmtFlow::Return(_) => {
                    return Err(RuntimeError::new(
                        "'return' is only valid inside a function",
                    ))
                }
            }
        }

        if self.pending_return.is_some() {
            return Err(RuntimeError::new(
                "'return' is only valid inside a function",
            ));
        }

        Ok(last_value)
    }

    pub fn run_tests(&mut self, program: &Program) -> TestReport {
        let mut tests = Vec::new();
        let mut setup_error = None;

        for statement in &program.statements {
            match statement {
                Stmt::Test { name, body } => tests.push((name.clone(), body.clone())),
                _ => {
                    if let Err(err) = self
                        .execute_statement(statement)
                        .map_err(|e| self.decorate_error(e))
                    {
                        setup_error = Some(err);
                        break;
                    }
                }
            }
        }

        if let Some(error) = setup_error {
            return TestReport {
                passed: 0,
                failed: tests.len(),
                results: tests
                    .into_iter()
                    .map(|(name, _)| TestCaseResult {
                        name,
                        passed: false,
                        error: Some(error.clone()),
                    })
                    .collect(),
            };
        }

        let mut results = Vec::new();
        for (name, body) in tests {
            self.call_stack
                .push(format!("{}:1: test \"{}\"", self.source_label, name));
            self.push_scope();
            let mut case_error = None;
            for statement in &body {
                match self
                    .execute_statement(statement)
                    .map_err(|e| self.decorate_error(e))
                {
                    Ok(StmtFlow::Continue(_)) => {}
                    Ok(StmtFlow::Return(_)) => {
                        case_error = Some(
                            RuntimeError::new("'return' is not allowed inside test blocks")
                                .with_code("RUNTIME0301")
                                .with_hint("remove 'return' or wrap logic in a function"),
                        );
                        break;
                    }
                    Err(err) => {
                        case_error = Some(err);
                        break;
                    }
                }
            }
            self.pop_scope();
            self.call_stack.pop();

            results.push(TestCaseResult {
                name,
                passed: case_error.is_none(),
                error: case_error,
            });
        }

        let passed = results.iter().filter(|r| r.passed).count();
        let failed = results.len().saturating_sub(passed);
        TestReport {
            passed,
            failed,
            results,
        }
    }

    fn execute_statement(&mut self, statement: &Stmt) -> Result<StmtFlow, RuntimeError> {
        match statement {
            Stmt::Use { target, alias } => {
                self.execute_use(target, alias.as_deref())?;
                Ok(StmtFlow::Continue(Value::Nil))
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_value = self.eval_expr(condition)?;
                if let Some(ret) = self.take_pending_return() {
                    return Ok(StmtFlow::Return(ret));
                }
                if condition_value.is_truthy() {
                    self.execute_scoped_block(then_branch)
                } else if let Some(else_branch) = else_branch {
                    self.execute_scoped_block(else_branch)
                } else {
                    Ok(StmtFlow::Continue(Value::Nil))
                }
            }
            Stmt::While { condition, body } => {
                let mut last_value = Value::Nil;
                loop {
                    let condition_value = self.eval_expr(condition)?;
                    if let Some(ret) = self.take_pending_return() {
                        return Ok(StmtFlow::Return(ret));
                    }
                    if !condition_value.is_truthy() {
                        break;
                    }

                    match self.execute_scoped_block(body)? {
                        StmtFlow::Continue(value) => last_value = value,
                        StmtFlow::Return(value) => return Ok(StmtFlow::Return(value)),
                    }
                }
                Ok(StmtFlow::Continue(last_value))
            }
            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                self.push_scope();
                let result = (|| -> Result<StmtFlow, RuntimeError> {
                    if let Some(initializer) = initializer {
                        match self.execute_statement(initializer)? {
                            StmtFlow::Continue(_) => {}
                            StmtFlow::Return(value) => return Ok(StmtFlow::Return(value)),
                        }
                    }

                    let mut last_value = Value::Nil;
                    loop {
                        if let Some(condition) = condition {
                            let condition_value = self.eval_expr(condition)?;
                            if let Some(ret) = self.take_pending_return() {
                                return Ok(StmtFlow::Return(ret));
                            }
                            if !condition_value.is_truthy() {
                                break;
                            }
                        }

                        match self.execute_scoped_block(body)? {
                            StmtFlow::Continue(value) => last_value = value,
                            StmtFlow::Return(value) => return Ok(StmtFlow::Return(value)),
                        }

                        if let Some(increment) = increment {
                            let value = self.eval_expr(increment)?;
                            if let Some(ret) = self.take_pending_return() {
                                return Ok(StmtFlow::Return(ret));
                            }
                            last_value = value;
                        }
                    }

                    Ok(StmtFlow::Continue(last_value))
                })();
                self.pop_scope();
                result
            }
            Stmt::ForIn {
                item_name,
                iterable,
                body,
            } => {
                let iterable_value = self.eval_expr(iterable)?;
                if let Some(ret) = self.take_pending_return() {
                    return Ok(StmtFlow::Return(ret));
                }

                let items = iterable_values(iterable_value)?;
                self.push_scope();
                let result = (|| -> Result<StmtFlow, RuntimeError> {
                    let mut last_value = Value::Nil;
                    let mut initialized = false;
                    for item in items {
                        if initialized {
                            self.assign(item_name, item)?;
                        } else {
                            self.define(item_name.clone(), item);
                            initialized = true;
                        }

                        match self.execute_scoped_block(body)? {
                            StmtFlow::Continue(value) => last_value = value,
                            StmtFlow::Return(value) => return Ok(StmtFlow::Return(value)),
                        }
                    }

                    Ok(StmtFlow::Continue(last_value))
                })();
                self.pop_scope();
                result
            }
            Stmt::Test { .. } => Ok(StmtFlow::Continue(Value::Nil)),
            Stmt::Assert { condition, message } => {
                let result = self.eval_expr(condition)?;
                if !result.is_truthy() {
                    let message_text = if let Some(msg_expr) = message {
                        let message_value = self.eval_expr(msg_expr)?;
                        expect_string(&message_value, "assert message")?
                    } else {
                        "assertion failed".to_string()
                    };
                    return Err(RuntimeError::new(format!(
                        "{}: condition evaluated to {} (type: {})",
                        message_text,
                        result,
                        result.type_name()
                    ))
                    .with_code("RUNTIME0300")
                    .with_hint("assert a boolean expression that evaluates to true"));
                }
                Ok(StmtFlow::Continue(Value::Nil))
            }
            Stmt::VarDecl {
                name, initializer, ..
            } => {
                let value = self.eval_expr(initializer)?;
                if let Some(ret) = self.take_pending_return() {
                    return Ok(StmtFlow::Return(ret));
                }
                self.assign_or_define(name.clone(), value.clone());
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
                name, params, body, ..
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

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<StmtFlow, RuntimeError> {
        let mut last_value = Value::Nil;
        for statement in statements {
            match self.execute_statement(statement)? {
                StmtFlow::Continue(value) => last_value = value,
                StmtFlow::Return(value) => return Ok(StmtFlow::Return(value)),
            }
        }
        Ok(StmtFlow::Continue(last_value))
    }

    fn execute_scoped_block(&mut self, statements: &[Stmt]) -> Result<StmtFlow, RuntimeError> {
        self.push_scope();
        let result = self.execute_block(statements);
        self.pop_scope();
        result
    }

    fn execute_use(&mut self, target: &UseTarget, alias: Option<&str>) -> Result<(), RuntimeError> {
        match target {
            UseTarget::ModulePath(path) => self.execute_module_use(path, alias),
            UseTarget::Url(spec) => {
                if is_http_import_spec(spec) {
                    self.execute_url_use(spec, alias)
                } else {
                    self.execute_file_use(spec, alias)
                }
            }
        }
    }

    fn execute_module_use(
        &mut self,
        path: &[String],
        alias: Option<&str>,
    ) -> Result<(), RuntimeError> {
        if path.is_empty() {
            return Err(RuntimeError::new("use path cannot be empty"));
        }

        let module_name = if path.len() >= 2 && path[0] == "std" {
            path[1].clone()
        } else {
            path[path.len() - 1].clone()
        };

        let module = self.lookup(&module_name).ok_or_else(|| {
            let builtins = builtins::core_globals().keys().cloned().collect::<Vec<_>>();
            let mut err = RuntimeError::new(format!("unknown module '{}'", module_name))
                .with_code("RUNTIME0208");
            if let Some(suggestion) = suggest_name(&module_name, &builtins) {
                err = err.with_hint(format!("did you mean '{}'?", suggestion));
            }
            err
        })?;

        let bind_name = alias.unwrap_or(&module_name).to_string();
        self.define(bind_name, module);
        Ok(())
    }

    fn execute_url_use(&mut self, spec: &str, alias: Option<&str>) -> Result<(), RuntimeError> {
        #[cfg(not(feature = "net"))]
        {
            let _ = (spec, alias);
            return Err(RuntimeError::new(
                "URL imports require a net-enabled build; rebuild with feature 'net'",
            )
            .with_code("RUNTIME0501")
            .with_hint("run `scalf build <file>` without disabling network feature"));
        }

        #[cfg(feature = "net")]
        {
            let cache_key = spec.to_string();
            if let Some(module) = self.loaded_url_modules.get(&cache_key).cloned() {
                let bind_name = alias
                    .map(ToString::to_string)
                    .unwrap_or_else(|| UseTarget::Url(cache_key.clone()).default_binding_name());
                self.define(bind_name, module);
                return Ok(());
            }

            if self.import_stack.contains(&cache_key) {
                return Err(RuntimeError::new(format!(
                    "circular URL import detected: {}",
                    self.import_stack.join(" -> ")
                )));
            }

            self.import_stack.push(cache_key.clone());
            let loaded = (|| {
                let resolved = resolve_url_import(spec, &self.permissions)?;
                let mut module_runtime = Runtime::with_permissions(self.permissions.clone())
                    .with_implicit_nil_for_unknown_variables(
                        self.implicit_nil_for_unknown_variables,
                    );
                module_runtime.import_stack = self.import_stack.clone();
                module_runtime.loaded_url_modules = self.loaded_url_modules.clone();
                let module = module_runtime.evaluate_imported_module(&resolved.source, spec)?;
                Ok(module)
            })();
            self.import_stack.pop();

            let module = loaded?;
            self.loaded_url_modules
                .insert(cache_key.clone(), module.clone());
            let bind_name = alias
                .map(ToString::to_string)
                .unwrap_or_else(|| UseTarget::Url(cache_key).default_binding_name());
            self.define(bind_name, module);
            Ok(())
        }
    }

    fn execute_file_use(&mut self, spec: &str, alias: Option<&str>) -> Result<(), RuntimeError> {
        let resolved_path = self.resolve_file_import_path(spec);
        self.permissions.check_read(&resolved_path)?;

        let source = std::fs::read_to_string(&resolved_path).map_err(|err| {
            RuntimeError::new(format!(
                "failed to read imported module '{}': {}",
                resolved_path.display(),
                err
            ))
        })?;
        let origin = resolved_path.display().to_string();

        let mut module_runtime = Runtime::with_permissions(self.permissions.clone())
            .with_implicit_nil_for_unknown_variables(self.implicit_nil_for_unknown_variables)
            .with_source_label(origin.clone());
        #[cfg(feature = "net")]
        {
            module_runtime.import_stack = self.import_stack.clone();
            module_runtime.loaded_url_modules = self.loaded_url_modules.clone();
        }
        let module = module_runtime.evaluate_imported_module(&source, &origin)?;

        let bind_name = alias
            .map(ToString::to_string)
            .unwrap_or_else(|| UseTarget::Url(spec.to_string()).default_binding_name());
        self.define(bind_name, module);
        Ok(())
    }

    fn resolve_file_import_path(&self, spec: &str) -> PathBuf {
        let candidate = PathBuf::from(spec);
        if candidate.is_absolute() {
            return normalize_path(&candidate);
        }

        let base_dir = if self.source_label.starts_with('<') {
            std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
        } else {
            let source_path = PathBuf::from(&self.source_label);
            if source_path.is_absolute() {
                source_path
                    .parent()
                    .map(PathBuf::from)
                    .unwrap_or_else(|| PathBuf::from("."))
            } else {
                let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
                match source_path.parent() {
                    Some(parent) if !parent.as_os_str().is_empty() => cwd.join(parent),
                    _ => cwd,
                }
            }
        };

        normalize_path(&base_dir.join(candidate))
    }

    fn evaluate_imported_module(
        &mut self,
        source: &str,
        origin: &str,
    ) -> Result<Value, RuntimeError> {
        let tokens = crate::lexer::lex(source).map_err(|err| {
            RuntimeError::new(format!(
                "failed to lex imported module '{}': {}",
                origin, err
            ))
        })?;
        let mut parser = crate::parser::Parser::new(tokens);
        let program = parser.parse_program().map_err(|err| {
            RuntimeError::new(format!(
                "failed to parse imported module '{}': {}",
                origin, err
            ))
        })?;

        let mut checker = crate::typechecker::TypeChecker::new()
            .with_implicit_nil_for_unknown_variables(self.implicit_nil_for_unknown_variables);
        if let Err(errors) = checker.check_program(&program) {
            let message = errors
                .into_iter()
                .map(|err| err.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            return Err(RuntimeError::new(format!(
                "type check failed for imported module '{}': {}",
                origin, message
            )));
        }

        self.run_program(&program)?;
        let builtins = builtins::core_globals();
        let mut exports = HashMap::new();
        for (name, value) in self.scopes.first().expect("global scope exists") {
            if builtins.contains_key(name) {
                continue;
            }
            exports.insert(name.clone(), value.clone());
        }
        Ok(Value::Module(Rc::new(exports)))
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match expr {
            Expr::Int(value) => Ok(Value::Int(*value)),
            Expr::Float(value) => Ok(Value::Float(*value)),
            Expr::String {
                value,
                has_interpolation,
            } => {
                if *has_interpolation {
                    Ok(Value::String(self.interpolate_string(value)?))
                } else {
                    Ok(Value::String(value.clone()))
                }
            }
            Expr::Bool(value) => Ok(Value::Bool(*value)),
            Expr::Nil => Ok(Value::Nil),
            Expr::Variable(name) => {
                if let Some(value) = self.lookup(name) {
                    Ok(value)
                } else if self.implicit_nil_for_unknown_variables {
                    Ok(Value::Nil)
                } else {
                    Err(RuntimeError::new(format!("unknown variable '{}'", name)))
                }
            }
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
                let lhs_value = self
                    .eval_expr(lhs)
                    .unwrap_or_else(|err| Value::Error(err.message));
                if matches!(lhs_value, Value::Nil | Value::Error(_)) {
                    self.eval_expr(rhs)
                } else {
                    Ok(lhs_value)
                }
            }
            Expr::OrReturn { lhs, return_value } => {
                let lhs_value = self
                    .eval_expr(lhs)
                    .unwrap_or_else(|err| Value::Error(err.message));
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
                    Value::Error(message) => Err(RuntimeError::new(format!(
                        "panic unwrap failed: {}",
                        message
                    ))),
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
            Expr::Function { params, body, .. } => {
                let params = params.iter().map(|p| p.name.clone()).collect::<Vec<_>>();
                Ok(Value::UserFunction(UserFunction {
                    name: "<anonymous>".to_string(),
                    params,
                    body: body.clone(),
                }))
            }
        }
    }

    fn interpolate_string(&mut self, template: &str) -> Result<String, RuntimeError> {
        let mut rendered = String::with_capacity(template.len());
        let mut cursor = 0usize;

        while let Some(open_rel) = template[cursor..].find('{') {
            let open = cursor + open_rel;
            rendered.push_str(&template[cursor..open]);

            let close = find_interpolation_end(template, open + 1)?;
            let expr_source = template[open + 1..close].trim();
            if expr_source.is_empty() {
                return Err(RuntimeError::new(
                    "string interpolation expression cannot be empty",
                ));
            }

            let value = self.eval_interpolation_expression(expr_source)?;
            rendered.push_str(&value.to_string());
            cursor = close + 1;
        }

        if cursor < template.len() {
            rendered.push_str(&template[cursor..]);
        }

        Ok(rendered)
    }

    fn eval_interpolation_expression(&mut self, source: &str) -> Result<Value, RuntimeError> {
        let tokens = crate::lexer::lex(source).map_err(|err| {
            RuntimeError::new(format!(
                "invalid interpolation expression: lex error at {}:{}: {}",
                err.line, err.column, err.message
            ))
        })?;
        let mut parser = crate::parser::Parser::new(tokens);
        let expr = parser.expression().map_err(|err| {
            RuntimeError::new(format!("invalid interpolation expression: {}", err))
        })?;
        parser.skip_statement_breaks();
        if !parser.is_at_end() {
            return Err(RuntimeError::new(
                "invalid interpolation expression: unexpected trailing tokens",
            ));
        }

        self.eval_expr(&expr)
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

    fn eval_match(
        &mut self,
        subject_value: &Value,
        arms: &[MatchArm],
    ) -> Result<Value, RuntimeError> {
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
                "value '{}' is not callable (value: {}, type: {})",
                callee.type_name(),
                callee,
                callee.type_name()
            ))
            .with_code("RUNTIME0102")
            .with_hint("call a function, method, or native module member")),
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
        self.call_stack
            .push(format!("{}:1: fn {}", self.source_label, function.name));
        for (name, value) in function.params.iter().zip(args) {
            self.define(name.clone(), value);
        }

        let mut return_value = Value::Nil;
        let mut function_error = None;
        for statement in &function.body {
            match self
                .execute_statement(statement)
                .map_err(|err| self.decorate_error(err))
            {
                Ok(StmtFlow::Continue(_)) => {}
                Ok(StmtFlow::Return(value)) => {
                    return_value = value;
                    self.pending_return = None;
                    break;
                }
                Err(err) => {
                    function_error = Some(err);
                    break;
                }
            }
        }

        self.pending_return = None;
        self.pop_scope();
        self.call_stack.pop();
        if let Some(err) = function_error {
            Err(err)
        } else {
            Ok(return_value)
        }
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
            "time.now_ms" => self.native_time_now_ms(args),
            "time.now_s" => self.native_time_now_s(args),
            "time.sleep" => self.native_time_sleep(args),
            "crypto.sha256" => self.native_crypto_sha256(args),
            "concurrency.await" => self.native_concurrency_await(args),
            "concurrency.join" => self.native_concurrency_join(args),
            "concurrency.timeout" => self.native_concurrency_timeout(args),
            "concurrency.channel" => self.native_concurrency_channel(args),
            "http.get" => self.native_http_get(args),
            "http.post" => self.native_http_post(args),
            "http.put" => self.native_http_put(args),
            "http.delete" => self.native_http_delete(args),
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
            Value::HttpResponse(response) => self.call_http_response_method(response, method, args),
            Value::HttpPending(pending) => {
                let response = self.resolve_http_pending(&pending)?;
                self.call_http_response_method(response, method, args)
            }
            Value::Channel(channel) => self.call_channel_method(channel, method, args),
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
            other => {
                let candidates = vec![
                    "trim".to_string(),
                    "split".to_string(),
                    "replace".to_string(),
                    "uppercase".to_string(),
                    "lowercase".to_string(),
                ];
                let mut err = RuntimeError::new(format!("unknown string method '{}'", other))
                    .with_code("RUNTIME0203");
                if let Some(suggestion) = suggest_name(other, &candidates) {
                    err = err.with_hint(format!("did you mean '{}'?", suggestion));
                }
                Err(err)
            }
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
            other => {
                let candidates = vec![
                    "push".to_string(),
                    "pop".to_string(),
                    "map".to_string(),
                    "filter".to_string(),
                    "reduce".to_string(),
                    "sort".to_string(),
                    "sorted".to_string(),
                ];
                let mut err = RuntimeError::new(format!("unknown list method '{}'", other))
                    .with_code("RUNTIME0204");
                if let Some(suggestion) = suggest_name(other, &candidates) {
                    err = err.with_hint(format!("did you mean '{}'?", suggestion));
                }
                Err(err)
            }
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
            other => {
                let candidates = vec![
                    "get".to_string(),
                    "set".to_string(),
                    "has".to_string(),
                    "keys".to_string(),
                    "values".to_string(),
                ];
                let mut err = RuntimeError::new(format!("unknown map method '{}'", other))
                    .with_code("RUNTIME0205");
                if let Some(suggestion) = suggest_name(other, &candidates) {
                    err = err.with_hint(format!("did you mean '{}'?", suggestion));
                }
                Err(err)
            }
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
            other => {
                let candidates = vec![
                    "join".to_string(),
                    "normalize".to_string(),
                    "basename".to_string(),
                    "dirname".to_string(),
                    "to_string".to_string(),
                ];
                let mut err = RuntimeError::new(format!("unknown path method '{}'", other))
                    .with_code("RUNTIME0206");
                if let Some(suggestion) = suggest_name(other, &candidates) {
                    err = err.with_hint(format!("did you mean '{}'?", suggestion));
                }
                Err(err)
            }
        }
    }

    fn call_http_response_method(
        &mut self,
        response: HttpResponseData,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "text" => {
                expect_arity("http.response.text", &args, 0)?;
                Ok(Value::String(response.body))
            }
            "json" => {
                expect_arity("http.response.json", &args, 0)?;
                parse_json_text(
                    &response.body,
                    "http.response.json failed to parse response body",
                )
            }
            other => {
                let candidates = vec!["text".to_string(), "json".to_string()];
                let mut err =
                    RuntimeError::new(format!("unknown http response method '{}'", other))
                        .with_code("RUNTIME0207");
                if let Some(suggestion) = suggest_name(other, &candidates) {
                    err = err.with_hint(format!("did you mean '{}'?", suggestion));
                }
                Err(err)
            }
        }
    }

    fn call_channel_method(
        &self,
        channel: Channel,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "send" => {
                expect_arity("channel.send", &args, 1)?;
                channel.send(args[0].clone());
                Ok(Value::Nil)
            }
            "recv" => {
                expect_arity("channel.recv", &args, 0)?;
                Ok(channel.recv().unwrap_or(Value::Nil))
            }
            "try_recv" => {
                expect_arity("channel.try_recv", &args, 0)?;
                Ok(channel.recv().unwrap_or(Value::Nil))
            }
            "recv_timeout" => {
                expect_arity("channel.recv_timeout", &args, 1)?;
                let Value::Int(ms) = args[0] else {
                    return Err(RuntimeError::new(format!(
                        "channel.recv_timeout expected int milliseconds, got '{}'",
                        args[0].type_name()
                    )));
                };
                if ms <= 0 {
                    return Err(RuntimeError::new(
                        "channel.recv_timeout timeout must be > 0 milliseconds",
                    ));
                }
                let timeout = Duration::from_millis(ms as u64);
                Ok(channel.recv_timeout(timeout).unwrap_or(Value::Nil))
            }
            "len" => {
                expect_arity("channel.len", &args, 0)?;
                Ok(Value::Int(channel.len() as i64))
            }
            other => Err(RuntimeError::new(format!(
                "channel has no method '{}'",
                other
            ))),
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

    fn native_http_get(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.native_http_request("get", args, false)
    }

    fn native_http_post(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.native_http_request("post", args, true)
    }

    fn native_http_put(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.native_http_request("put", args, true)
    }

    fn native_http_delete(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        self.native_http_request("delete", args, false)
    }

    #[cfg(feature = "net")]
    fn native_http_request(
        &self,
        method_name: &str,
        args: Vec<Value>,
        with_body: bool,
    ) -> Result<Value, RuntimeError> {
        let method = match method_name {
            "get" => reqwest::Method::GET,
            "post" => reqwest::Method::POST,
            "put" => reqwest::Method::PUT,
            "delete" => reqwest::Method::DELETE,
            other => {
                return Err(RuntimeError::new(format!(
                    "unsupported http method '{}'",
                    other
                )))
            }
        };
        if args.is_empty() {
            return Err(RuntimeError::new(format!(
                "http.{} expects at least a URL argument",
                method_name
            )));
        }

        let url = expect_string(&args[0], &format!("http.{} url", method_name))?;
        self.permissions.check_net(&url)?;

        let mut cursor = 1;
        let body = if with_body {
            if args.len() < 2 {
                return Err(RuntimeError::new(format!(
                    "http.{} expects a request body argument",
                    method_name
                )));
            }
            cursor = 2;
            Some(http_body_to_string(&args[1])?)
        } else {
            None
        };

        let mut headers = HashMap::new();
        let mut timeout_ms: u64 = 30_000;

        if args.len() > cursor {
            match &args[cursor] {
                Value::Map(_) => {
                    headers = headers_from_value(&args[cursor])?;
                    cursor += 1;
                }
                Value::Int(value) if *value > 0 => {
                    timeout_ms = *value as u64;
                    cursor += 1;
                }
                Value::Int(_) => {
                    return Err(RuntimeError::new(format!(
                        "http.{} timeout must be > 0 milliseconds",
                        method_name
                    )));
                }
                other => {
                    return Err(RuntimeError::new(format!(
                        "http.{} optional argument must be headers map or timeout int, got '{}'",
                        method_name,
                        other.type_name()
                    )));
                }
            }
        }

        if args.len() > cursor {
            match &args[cursor] {
                Value::Int(value) if *value > 0 => {
                    timeout_ms = *value as u64;
                    cursor += 1;
                }
                Value::Int(_) => {
                    return Err(RuntimeError::new(format!(
                        "http.{} timeout must be > 0 milliseconds",
                        method_name
                    )));
                }
                other => {
                    return Err(RuntimeError::new(format!(
                        "http.{} timeout must be int milliseconds, got '{}'",
                        method_name,
                        other.type_name()
                    )));
                }
            }
        }

        if args.len() != cursor {
            return Err(RuntimeError::new(format!(
                "http.{} received too many arguments",
                method_name
            )));
        }

        let lower_method = method_name.to_string();
        let pending = PendingHttp::spawn(move || {
            let client = reqwest::blocking::Client::builder()
                .timeout(Duration::from_millis(timeout_ms))
                .build()
                .map_err(|err| format!("http client init failed: {}", err))?;

            let mut request = client.request(method.clone(), &url);
            for (name, value) in &headers {
                request = request.header(name, value);
            }

            if let Some(body) = body {
                request = request.body(body);
            }

            let response = request.send().map_err(|err| {
                if err.is_timeout() {
                    format!("http.{} request timed out: {}", lower_method, err)
                } else {
                    format!("http.{} request failed: {}", lower_method, err)
                }
            })?;

            let status = response.status().as_u16() as i64;
            let final_url = response.url().to_string();
            let mut header_values = HashMap::new();
            for (name, value) in response.headers() {
                let rendered = value.to_str().unwrap_or_default().to_string();
                header_values.insert(name.to_string(), rendered);
            }

            let body = response
                .text()
                .map_err(|err| format!("http response body read failed: {}", err))?;

            Ok(HttpResponseData {
                status,
                body,
                headers: header_values,
                url: final_url,
            })
        });
        Ok(Value::HttpPending(pending))
    }

    #[cfg(not(feature = "net"))]
    fn native_http_request(
        &self,
        method_name: &str,
        args: Vec<Value>,
        _with_body: bool,
    ) -> Result<Value, RuntimeError> {
        let _ = args;
        Err(RuntimeError::new(format!(
            "http.{} is unavailable in this build (compiled without 'net' feature)",
            method_name
        ))
        .with_code("RUNTIME0502")
        .with_hint("rebuild with network support enabled"))
    }

    fn resolve_http_pending(
        &self,
        pending: &PendingHttp,
    ) -> Result<HttpResponseData, RuntimeError> {
        pending.resolve().map_err(RuntimeError::new)
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

    fn native_time_now_ms(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("time.now_ms", &args, 0)?;
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|err| RuntimeError::new(format!("time.now_ms failed: {}", err)))?;
        Ok(Value::Int(now.as_millis() as i64))
    }

    fn native_time_now_s(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("time.now_s", &args, 0)?;
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|err| RuntimeError::new(format!("time.now_s failed: {}", err)))?;
        Ok(Value::Int(now.as_secs() as i64))
    }

    fn native_time_sleep(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("time.sleep", &args, 1)?;
        let Value::Int(ms) = args[0] else {
            return Err(RuntimeError::new(format!(
                "time.sleep milliseconds expected int, got {}",
                args[0].type_name()
            )));
        };
        if ms < 0 {
            return Err(RuntimeError::new("time.sleep milliseconds must be >= 0"));
        }
        std::thread::sleep(Duration::from_millis(ms as u64));
        Ok(Value::Nil)
    }

    #[cfg(feature = "crypto")]
    fn native_crypto_sha256(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("crypto.sha256", &args, 1)?;
        let text = expect_string(&args[0], "crypto.sha256 input")?;
        Ok(Value::String(sha256_hex(text.as_bytes())))
    }

    #[cfg(not(feature = "crypto"))]
    fn native_crypto_sha256(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("crypto.sha256", &args, 1)?;
        let _ = args;
        Err(RuntimeError::new(
            "crypto.sha256 is unavailable in this build (compiled without 'crypto' feature)",
        )
        .with_code("RUNTIME0504")
        .with_hint("rebuild with crypto support enabled"))
    }

    fn native_concurrency_await(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("concurrency.await", &args, 1)?;
        self.await_value(args.into_iter().next().expect("arity checked"))
    }

    fn native_concurrency_join(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("concurrency.join", &args, 1)?;
        let Value::List(items) = &args[0] else {
            return Err(RuntimeError::new(format!(
                "concurrency.join expects list, got '{}'",
                args[0].type_name()
            )));
        };

        let mut resolved = Vec::new();
        for item in items.borrow().iter() {
            resolved.push(self.await_value(item.clone())?);
        }
        Ok(Value::list(resolved))
    }

    fn native_concurrency_timeout(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        expect_arity("concurrency.timeout", &args, 2)?;
        let Value::Int(ms) = args[0] else {
            return Err(RuntimeError::new(format!(
                "concurrency.timeout timeout expected int milliseconds, got '{}'",
                args[0].type_name()
            )));
        };
        if ms <= 0 {
            return Err(RuntimeError::new(
                "concurrency.timeout timeout must be > 0 milliseconds",
            ));
        }

        match &args[1] {
            Value::HttpPending(pending) => {
                match pending.resolve_with_timeout(Some(Duration::from_millis(ms as u64))) {
                    Ok(response) => Ok(Value::HttpResponse(response)),
                    Err(message) => Ok(Value::Error(message)),
                }
            }
            other => Ok(other.clone()),
        }
    }

    fn native_concurrency_channel(&self, args: Vec<Value>) -> Result<Value, RuntimeError> {
        if !args.is_empty() {
            return Err(RuntimeError::new(
                "concurrency.channel expects no arguments",
            ));
        }
        Ok(Value::Channel(Channel::new()))
    }

    fn await_value(&self, value: Value) -> Result<Value, RuntimeError> {
        match value {
            Value::HttpPending(pending) => {
                Ok(Value::HttpResponse(self.resolve_http_pending(&pending)?))
            }
            other => Ok(other),
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
            Value::Module(module) => module.get(property).cloned().ok_or_else(|| {
                let mut err = RuntimeError::new(format!("module has no member '{}'", property))
                    .with_code("RUNTIME0201");
                let candidates = module.keys().cloned().collect::<Vec<_>>();
                if let Some(suggestion) = suggest_name(property, &candidates) {
                    err = err.with_hint(format!("did you mean '{}'?", suggestion));
                }
                err
            }),
            Value::HttpResponse(response) => self.read_http_member(response, property),
            Value::HttpPending(pending) => {
                if property == "text" || property == "json" {
                    return Ok(Value::BoundMethod {
                        receiver: Box::new(Value::HttpPending(pending)),
                        method: property.to_string(),
                    });
                }
                let response = self.resolve_http_pending(&pending)?;
                self.read_http_member(response, property)
            }
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
            Value::Channel(channel) => Ok(Value::BoundMethod {
                receiver: Box::new(Value::Channel(channel)),
                method: property.to_string(),
            }),
            Value::Nil if optional => Ok(Value::Nil),
            Value::Nil => Err(RuntimeError::new(format!(
                "cannot access member '{}' on nil (value: nil, type: nil)",
                property
            ))
            .with_code("RUNTIME0200")
            .with_hint("use optional chaining with '?.' or provide a default with 'or'")),
            other => Err(RuntimeError::new(format!(
                "type '{}' has no member '{}' (value: {})",
                other.type_name(),
                property,
                other
            ))
            .with_code("RUNTIME0202")
            .with_hint("check the value type before accessing members")),
        }
    }

    fn read_http_member(
        &self,
        response: HttpResponseData,
        property: &str,
    ) -> Result<Value, RuntimeError> {
        match property {
            "status" => Ok(Value::Int(response.status)),
            "url" => Ok(Value::String(response.url)),
            "headers" => {
                let mut map = HashMap::new();
                for (name, value) in response.headers {
                    map.insert(name, Value::String(value));
                }
                Ok(Value::map(map))
            }
            "text" | "json" => Ok(Value::BoundMethod {
                receiver: Box::new(Value::HttpResponse(response)),
                method: property.to_string(),
            }),
            other => Err(RuntimeError::new(format!(
                "http response has no member '{}'",
                other
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
                    return Err(RuntimeError::new(
                        "cannot bind list pattern to non-list value",
                    ));
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
                    return Err(RuntimeError::new(
                        "cannot bind map pattern to non-map value",
                    ));
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

    fn assign_or_define(&mut self, name: String, value: Value) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(&name) {
                scope.insert(name, value);
                return;
            }
        }
        self.define(name, value);
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

    fn decorate_error(&self, err: RuntimeError) -> RuntimeError {
        let mut frames = Vec::new();
        for frame in self.call_stack.iter().rev() {
            frames.push(frame.clone());
        }
        if !self.source_label.is_empty() {
            frames.push(format!("{}:1", self.source_label));
        }
        err.with_stack_frames(frames)
    }
}

#[cfg(feature = "net")]
#[derive(Debug, Clone)]
struct ImportLockEntry {
    url: String,
    sha256: String,
    version: Option<String>,
}

#[cfg(feature = "net")]
#[derive(Debug, Clone)]
struct ResolvedImport {
    source: String,
}

#[cfg(feature = "net")]
fn resolve_url_import(
    spec: &str,
    permissions: &Permissions,
) -> Result<ResolvedImport, RuntimeError> {
    let (fetch_url, version) = parse_url_import_spec(spec)?;
    let cache_root = import_cache_root();
    let modules_dir = cache_root.join("modules");
    fs::create_dir_all(&modules_dir).map_err(|err| {
        RuntimeError::new(format!(
            "failed to create import cache directory '{}': {}",
            modules_dir.display(),
            err
        ))
    })?;

    let lockfile = import_lockfile_path();
    let mut lock_entries = read_import_lockfile(&lockfile)?;

    if let Some(entry) = lock_entries.get(spec) {
        if let Some(source) = read_cached_import(&modules_dir, &entry.sha256)? {
            return Ok(ResolvedImport { source });
        }
        let source = fetch_url_import_source(&fetch_url, permissions)?;
        let fetched_hash = sha256_hex(source.as_bytes());
        if fetched_hash != entry.sha256 {
            return Err(RuntimeError::new(format!(
                "lockfile hash mismatch for '{}': expected {}, got {}",
                spec, entry.sha256, fetched_hash
            )));
        }
        write_cached_import(&modules_dir, &fetched_hash, &source)?;
        return Ok(ResolvedImport { source });
    }

    let source = fetch_url_import_source(&fetch_url, permissions)?;
    let sha256 = sha256_hex(source.as_bytes());
    write_cached_import(&modules_dir, &sha256, &source)?;

    lock_entries.insert(
        spec.to_string(),
        ImportLockEntry {
            url: fetch_url,
            sha256,
            version,
        },
    );
    write_import_lockfile(&lockfile, &lock_entries)?;
    Ok(ResolvedImport { source })
}

#[cfg(feature = "net")]
fn parse_url_import_spec(spec: &str) -> Result<(String, Option<String>), RuntimeError> {
    let is_http = spec.starts_with("http://") || spec.starts_with("https://");
    if !is_http {
        return Err(RuntimeError::new(format!(
            "URL imports must start with http:// or https://, got '{}'",
            spec
        )));
    }

    if let Some((base, pin)) = spec.rsplit_once('@') {
        if !pin.is_empty() && !pin.contains('/') && !pin.contains('?') && !pin.contains('#') {
            return Ok((base.to_string(), Some(pin.to_string())));
        }
    }
    Ok((spec.to_string(), None))
}

#[cfg(feature = "net")]
fn fetch_url_import_source(url: &str, permissions: &Permissions) -> Result<String, RuntimeError> {
    #[cfg(not(feature = "net"))]
    {
        let _ = (url, permissions);
        return Err(RuntimeError::new(
            "URL imports are unavailable in this build (compiled without 'net' feature)",
        )
        .with_code("RUNTIME0501")
        .with_hint("rebuild with network support enabled"));
    }

    #[cfg(feature = "net")]
    {
        permissions.check_net(url)?;
        let response = reqwest::blocking::get(url).map_err(|err| {
            RuntimeError::new(format!("failed to fetch import '{}': {}", url, err))
        })?;
        if !response.status().is_success() {
            return Err(RuntimeError::new(format!(
                "failed to fetch import '{}': HTTP {}",
                url,
                response.status()
            )));
        }
        response.text().map_err(|err| {
            RuntimeError::new(format!(
                "failed to read response body for import '{}': {}",
                url, err
            ))
        })
    }
}

#[cfg(feature = "net")]
fn read_cached_import(modules_dir: &Path, sha256: &str) -> Result<Option<String>, RuntimeError> {
    let file_path = modules_dir.join(format!("{}.scl", sha256));
    if !file_path.exists() {
        return Ok(None);
    }
    let source = fs::read_to_string(&file_path).map_err(|err| {
        RuntimeError::new(format!(
            "failed to read cached import '{}': {}",
            file_path.display(),
            err
        ))
    })?;
    let actual = sha256_hex(source.as_bytes());
    if actual != sha256 {
        return Err(RuntimeError::new(format!(
            "cached import hash mismatch at '{}': expected {}, got {}",
            file_path.display(),
            sha256,
            actual
        )));
    }
    Ok(Some(source))
}

#[cfg(feature = "net")]
fn write_cached_import(modules_dir: &Path, sha256: &str, source: &str) -> Result<(), RuntimeError> {
    let file_path = modules_dir.join(format!("{}.scl", sha256));
    fs::write(&file_path, source).map_err(|err| {
        RuntimeError::new(format!(
            "failed to write cached import '{}': {}",
            file_path.display(),
            err
        ))
    })
}

#[cfg(feature = "net")]
fn read_import_lockfile(path: &Path) -> Result<HashMap<String, ImportLockEntry>, RuntimeError> {
    if !path.exists() {
        return Ok(HashMap::new());
    }
    let text = fs::read_to_string(path).map_err(|err| {
        RuntimeError::new(format!(
            "failed to read lockfile '{}': {}",
            path.display(),
            err
        ))
    })?;
    let json: serde_json::Value = serde_json::from_str(&text).map_err(|err| {
        RuntimeError::new(format!(
            "failed to parse lockfile '{}': {}",
            path.display(),
            err
        ))
    })?;
    let mut entries = HashMap::new();
    let Some(imports) = json.get("imports").and_then(serde_json::Value::as_object) else {
        return Ok(entries);
    };

    for (spec, entry) in imports {
        let Some(url) = entry.get("url").and_then(serde_json::Value::as_str) else {
            continue;
        };
        let Some(sha256) = entry.get("sha256").and_then(serde_json::Value::as_str) else {
            continue;
        };
        let version = entry
            .get("version")
            .and_then(serde_json::Value::as_str)
            .map(ToString::to_string);
        entries.insert(
            spec.clone(),
            ImportLockEntry {
                url: url.to_string(),
                sha256: sha256.to_string(),
                version,
            },
        );
    }

    Ok(entries)
}

#[cfg(feature = "net")]
fn write_import_lockfile(
    path: &Path,
    entries: &HashMap<String, ImportLockEntry>,
) -> Result<(), RuntimeError> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent).map_err(|err| {
        RuntimeError::new(format!(
            "failed to create lockfile directory '{}': {}",
            parent.display(),
            err
        ))
    })?;

    let mut imports = serde_json::Map::new();
    let mut keys = entries.keys().cloned().collect::<Vec<_>>();
    keys.sort();
    for spec in keys {
        let entry = entries.get(&spec).expect("entry exists");
        let mut json_entry = serde_json::Map::new();
        json_entry.insert(
            "url".to_string(),
            serde_json::Value::String(entry.url.clone()),
        );
        json_entry.insert(
            "sha256".to_string(),
            serde_json::Value::String(entry.sha256.clone()),
        );
        if let Some(version) = &entry.version {
            json_entry.insert(
                "version".to_string(),
                serde_json::Value::String(version.clone()),
            );
        }
        imports.insert(spec, serde_json::Value::Object(json_entry));
    }

    let mut root = serde_json::Map::new();
    root.insert(
        "version".to_string(),
        serde_json::Value::Number(serde_json::Number::from(1)),
    );
    root.insert("imports".to_string(), serde_json::Value::Object(imports));
    let encoded =
        serde_json::to_string_pretty(&serde_json::Value::Object(root)).map_err(|err| {
            RuntimeError::new(format!(
                "failed to serialize lockfile '{}': {}",
                path.display(),
                err
            ))
        })?;
    fs::write(path, encoded).map_err(|err| {
        RuntimeError::new(format!(
            "failed to write lockfile '{}': {}",
            path.display(),
            err
        ))
    })
}

#[cfg(feature = "net")]
fn import_lockfile_path() -> PathBuf {
    if let Ok(path) = std::env::var("scalf_LOCKFILE") {
        return PathBuf::from(path);
    }
    std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(".scl.lock")
}

#[cfg(feature = "net")]
fn import_cache_root() -> PathBuf {
    if let Ok(path) = std::env::var("scalf_CACHE_DIR") {
        return PathBuf::from(path);
    }

    if let Ok(home) = std::env::var("HOME") {
        return PathBuf::from(home).join(".scl").join("cache");
    }
    if let Ok(home) = std::env::var("USERPROFILE") {
        return PathBuf::from(home).join(".scl").join("cache");
    }
    std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(".scl")
        .join("cache")
}

#[cfg(feature = "crypto")]
fn sha256_hex(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    let digest = hasher.finalize();
    let mut out = String::with_capacity(digest.len() * 2);
    for byte in digest {
        out.push(hex_char(byte >> 4));
        out.push(hex_char(byte & 0x0f));
    }
    out
}

#[cfg(feature = "crypto")]
fn hex_char(nibble: u8) -> char {
    match nibble {
        0..=9 => (b'0' + nibble) as char,
        10..=15 => (b'a' + (nibble - 10)) as char,
        _ => unreachable!("invalid nibble"),
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

fn suggest_name(input: &str, candidates: &[String]) -> Option<String> {
    let mut best: Option<(usize, &String)> = None;
    for candidate in candidates {
        let distance = levenshtein(input, candidate);
        if distance <= 3 || candidate.starts_with(input) || input.starts_with(candidate) {
            match best {
                Some((best_distance, _)) if distance >= best_distance => {}
                _ => best = Some((distance, candidate)),
            }
        }
    }
    best.map(|(_, value)| value.clone())
}

fn levenshtein(a: &str, b: &str) -> usize {
    if a == b {
        return 0;
    }
    if a.is_empty() {
        return b.chars().count();
    }
    if b.is_empty() {
        return a.chars().count();
    }

    let b_chars = b.chars().collect::<Vec<_>>();
    let mut prev = (0..=b_chars.len()).collect::<Vec<_>>();
    let mut curr = vec![0; b_chars.len() + 1];

    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b_chars.iter().enumerate() {
            let cost = if ca == *cb { 0 } else { 1 };
            curr[j + 1] =
                std::cmp::min(std::cmp::min(curr[j] + 1, prev[j + 1] + 1), prev[j] + cost);
        }
        prev.clone_from(&curr);
    }

    prev[b_chars.len()]
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

#[cfg(feature = "net")]
fn headers_from_value(value: &Value) -> Result<HashMap<String, String>, RuntimeError> {
    let Value::Map(entries) = value else {
        return Err(RuntimeError::new(format!(
            "http headers expected map, got '{}'",
            value.type_name()
        )));
    };

    let mut headers = HashMap::new();
    for (key, value) in entries.borrow().iter() {
        let rendered = match value {
            Value::String(text) => text.clone(),
            Value::Int(number) => number.to_string(),
            Value::Float(number) => number.to_string(),
            Value::Bool(boolean) => boolean.to_string(),
            _ => {
                return Err(RuntimeError::new(format!(
                    "http header '{}' must be string/number/bool, got '{}'",
                    key,
                    value.type_name()
                )))
            }
        };
        headers.insert(key.clone(), rendered);
    }
    Ok(headers)
}

#[cfg(feature = "net")]
fn http_body_to_string(value: &Value) -> Result<String, RuntimeError> {
    if let Value::String(text) = value {
        return Ok(text.clone());
    }
    let encoded = serde_json::to_string(&value_to_json(value)?)
        .map_err(|err| RuntimeError::new(format!("http body encode failed: {}", err)))?;
    Ok(encoded)
}

fn normalize_allowed_net_entry(entry: &str) -> String {
    let trimmed = entry.trim().trim_matches('/');
    if trimmed == "*" {
        return "*".to_string();
    }
    if let Some((host, port)) = extract_host_and_port(trimmed) {
        if let Some(port) = port {
            return format!("{}:{}", host, port);
        }
        return host;
    }
    trimmed.to_ascii_lowercase()
}

fn is_http_import_spec(spec: &str) -> bool {
    let lowered = spec.trim().to_ascii_lowercase();
    lowered.starts_with("http://") || lowered.starts_with("https://")
}

fn prompt_permission(access_kind: &str, target: &str) -> bool {
    let mut stderr = io::stderr();
    let prompt = format!(
        "permission prompt: allow {} access to '{}' once? [y/N]: ",
        access_kind, target
    );
    if stderr.write_all(prompt.as_bytes()).is_err() {
        return false;
    }
    if stderr.flush().is_err() {
        return false;
    }

    let mut input = String::new();
    if io::stdin().read_line(&mut input).is_err() {
        return false;
    }

    matches!(input.trim().to_ascii_lowercase().as_str(), "y" | "yes")
}

fn find_interpolation_end(template: &str, start: usize) -> Result<usize, RuntimeError> {
    let mut depth: usize = 0;
    let mut in_string = false;
    let mut escaped = false;

    for (offset, ch) in template[start..].char_indices() {
        if in_string {
            if escaped {
                escaped = false;
                continue;
            }
            match ch {
                '\\' => escaped = true,
                '"' => in_string = false,
                _ => {}
            }
            continue;
        }

        match ch {
            '"' => in_string = true,
            '{' => depth += 1,
            '}' => {
                if depth == 0 {
                    return Ok(start + offset);
                }
                depth -= 1;
            }
            _ => {}
        }
    }

    Err(RuntimeError::new(
        "unterminated string interpolation expression (missing '}')",
    ))
}

fn extract_host_and_port(input: &str) -> Option<(String, Option<u16>)> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return None;
    }

    let without_scheme = if let Some((_, rest)) = trimmed.split_once("://") {
        rest
    } else {
        trimmed
    };

    let authority = without_scheme.split('/').next().unwrap_or_default().trim();
    if authority.is_empty() {
        return None;
    }

    let authority = authority.rsplit('@').next().unwrap_or(authority).trim();
    if authority.is_empty() {
        return None;
    }

    if authority.starts_with('[') {
        let close_idx = authority.find(']')?;
        let host = authority[1..close_idx].to_ascii_lowercase();
        let remainder = authority[close_idx + 1..].trim();
        if let Some(port_str) = remainder.strip_prefix(':') {
            let port = port_str.parse::<u16>().ok()?;
            return Some((host, Some(port)));
        }
        return Some((host, None));
    }

    if let Some((host, port_str)) = authority.rsplit_once(':') {
        if !host.is_empty() && !port_str.is_empty() {
            if let Ok(port) = port_str.parse::<u16>() {
                return Some((host.to_ascii_lowercase(), Some(port)));
            }
        }
    }

    Some((authority.to_ascii_lowercase(), None))
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

fn iterable_values(value: Value) -> Result<Vec<Value>, RuntimeError> {
    match value {
        Value::List(values) => Ok(values.borrow().iter().cloned().collect()),
        Value::String(text) => Ok(UnicodeSegmentation::graphemes(text.as_str(), true)
            .map(|g| Value::String(g.to_string()))
            .collect()),
        Value::Map(entries) => Ok(entries
            .borrow()
            .keys()
            .cloned()
            .map(Value::String)
            .collect()),
        other => Err(RuntimeError::new(format!(
            "for-in requires list, map, or string iterable, got '{}'",
            other.type_name()
        ))),
    }
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
    let value = to_f64(&args[0])
        .ok_or_else(|| RuntimeError::new("math.round expected numeric argument"))?;
    Ok(Value::Int(value.round() as i64))
}

#[cfg(feature = "json")]
fn native_json_parse(args: Vec<Value>) -> Result<Value, RuntimeError> {
    expect_arity("json.parse", &args, 1)?;
    let text = expect_string(&args[0], "json.parse input")?;
    parse_json_text(&text, "json.parse failed")
}

#[cfg(not(feature = "json"))]
fn native_json_parse(args: Vec<Value>) -> Result<Value, RuntimeError> {
    expect_arity("json.parse", &args, 1)?;
    let _ = args;
    Err(RuntimeError::new(
        "json.parse is unavailable in this build (compiled without 'json' feature)",
    )
    .with_code("RUNTIME0503")
    .with_hint("rebuild with json support enabled"))
}

#[cfg(feature = "json")]
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

#[cfg(not(feature = "json"))]
fn native_json_stringify(args: Vec<Value>) -> Result<Value, RuntimeError> {
    if args.is_empty() || args.len() > 2 {
        return Err(RuntimeError::new(
            "json.stringify expects 1 or 2 arguments: value, pretty(bool?)",
        ));
    }
    Err(RuntimeError::new(
        "json.stringify is unavailable in this build (compiled without 'json' feature)",
    )
    .with_code("RUNTIME0503")
    .with_hint("rebuild with json support enabled"))
}

#[cfg(feature = "json")]
fn parse_json_text(text: &str, context: &str) -> Result<Value, RuntimeError> {
    let parsed: serde_json::Value = serde_json::from_str(text)
        .map_err(|err| RuntimeError::new(format!("{}: {}", context, err)))?;
    Ok(json_to_value(parsed))
}

#[cfg(not(feature = "json"))]
fn parse_json_text(_text: &str, _context: &str) -> Result<Value, RuntimeError> {
    Err(RuntimeError::new(
        "json support is unavailable in this build (compiled without 'json' feature)",
    )
    .with_code("RUNTIME0503")
    .with_hint("rebuild with json support enabled"))
}

#[cfg(feature = "json")]
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
        serde_json::Value::Array(items) => {
            Value::list(items.into_iter().map(json_to_value).collect())
        }
        serde_json::Value::Object(entries) => {
            let mut map = HashMap::new();
            for (k, v) in entries {
                map.insert(k, json_to_value(v));
            }
            Value::map(map)
        }
    }
}

#[cfg(feature = "json")]
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
        Value::HttpResponse(response) => {
            let mut object = serde_json::Map::new();
            object.insert(
                "status".to_string(),
                serde_json::Value::Number((response.status).into()),
            );
            object.insert(
                "body".to_string(),
                serde_json::Value::String(response.body.clone()),
            );
            object.insert(
                "url".to_string(),
                serde_json::Value::String(response.url.clone()),
            );
            let mut headers = serde_json::Map::new();
            for (key, value) in &response.headers {
                headers.insert(key.clone(), serde_json::Value::String(value.clone()));
            }
            object.insert("headers".to_string(), serde_json::Value::Object(headers));
            Ok(serde_json::Value::Object(object))
        }
        Value::UserFunction(_)
        | Value::NativeFunction(_)
        | Value::Module(_)
        | Value::BoundMethod { .. }
        | Value::HttpPending(_)
        | Value::Channel(_) => Err(RuntimeError::new(format!(
            "json.stringify cannot encode '{}'",
            value.type_name()
        ))),
    }
}
