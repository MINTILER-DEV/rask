pub mod types;

use std::collections::HashMap;
use std::error::Error;
use std::fmt;

use crate::parser::ast::{BinaryOp, Expr, MatchArm, Pattern, Program, Stmt, UnaryOp};
use types::{common_numeric_type, is_assignable, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct TypeError {
    pub message: String,
}

impl TypeError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "type error: {}", self.message)
    }
}

impl Error for TypeError {}

#[derive(Debug, Clone)]
pub struct TypeCheckOutput {
    pub inferred_types: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
struct FunctionContext {
    declared_return: Type,
    inferred_returns: Vec<Type>,
}

#[derive(Debug, Default)]
pub struct TypeChecker {
    scopes: Vec<HashMap<String, Type>>,
    function_stack: Vec<FunctionContext>,
    implicit_nil_for_unknown_variables: bool,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut global = HashMap::new();
        global.insert(
            "str".to_string(),
            Type::Function {
                params: vec![Type::Unknown],
                ret: Box::new(Type::String),
            },
        );
        global.insert(
            "int".to_string(),
            Type::Function {
                params: vec![Type::Unknown],
                ret: Box::new(Type::Int),
            },
        );
        global.insert(
            "float".to_string(),
            Type::Function {
                params: vec![Type::Unknown],
                ret: Box::new(Type::Float),
            },
        );
        global.insert(
            "list".to_string(),
            Type::Function {
                params: vec![Type::Unknown],
                ret: Box::new(Type::List(Box::new(Type::Unknown))),
            },
        );
        global.insert(
            "map".to_string(),
            Type::Function {
                params: vec![Type::Unknown],
                ret: Box::new(Type::Map(Box::new(Type::String), Box::new(Type::Unknown))),
            },
        );
        global.insert(
            "len".to_string(),
            Type::Function {
                params: vec![Type::Unknown],
                ret: Box::new(Type::Int),
            },
        );
        global.insert("math".to_string(), Type::Unknown);
        global.insert("fs".to_string(), Type::Unknown);
        global.insert("json".to_string(), Type::Unknown);
        global.insert("path".to_string(), Type::Unknown);
        global.insert("env".to_string(), Type::Unknown);
        global.insert("http".to_string(), Type::Unknown);
        global.insert("time".to_string(), Type::Unknown);
        global.insert("crypto".to_string(), Type::Unknown);
        global.insert("concurrency".to_string(), Type::Unknown);
        global.insert(
            "Path".to_string(),
            Type::Function {
                params: vec![Type::Unknown],
                ret: Box::new(Type::Named {
                    name: "Path".to_string(),
                    args: vec![],
                }),
            },
        );
        Self {
            scopes: vec![global],
            function_stack: Vec::new(),
            implicit_nil_for_unknown_variables: false,
        }
    }

    pub fn with_implicit_nil_for_unknown_variables(mut self, enabled: bool) -> Self {
        self.implicit_nil_for_unknown_variables = enabled;
        self
    }

    pub fn inferred_types(&self) -> &HashMap<String, Type> {
        &self.scopes[0]
    }

    pub fn check_program(&mut self, program: &Program) -> Result<TypeCheckOutput, Vec<TypeError>> {
        let mut errors = Vec::new();
        for statement in &program.statements {
            self.check_statement(statement, &mut errors);
        }

        if errors.is_empty() {
            Ok(TypeCheckOutput {
                inferred_types: self.scopes[0].clone(),
            })
        } else {
            Err(errors)
        }
    }

    fn check_statement(&mut self, statement: &Stmt, errors: &mut Vec<TypeError>) {
        match statement {
            Stmt::Use { target, alias } => {
                let name = alias
                    .clone()
                    .unwrap_or_else(|| target.default_binding_name());
                self.define(name, Type::Unknown);
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let condition_ty = self.infer_expr(condition, errors);
                if !is_assignable(&condition_ty, &Type::Bool)
                    && !matches!(condition_ty, Type::Unknown)
                {
                    errors.push(TypeError::new(format!(
                        "if condition must be bool, got '{}'",
                        condition_ty
                    )));
                }

                self.push_scope();
                for stmt in then_branch {
                    self.check_statement(stmt, errors);
                }
                self.pop_scope();

                if let Some(else_branch) = else_branch {
                    self.push_scope();
                    for stmt in else_branch {
                        self.check_statement(stmt, errors);
                    }
                    self.pop_scope();
                }
            }
            Stmt::While { condition, body } => {
                let condition_ty = self.infer_expr(condition, errors);
                if !is_assignable(&condition_ty, &Type::Bool)
                    && !matches!(condition_ty, Type::Unknown)
                {
                    errors.push(TypeError::new(format!(
                        "while condition must be bool, got '{}'",
                        condition_ty
                    )));
                }
                self.push_scope();
                for stmt in body {
                    self.check_statement(stmt, errors);
                }
                self.pop_scope();
            }
            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                self.push_scope();
                if let Some(initializer) = initializer {
                    self.check_statement(initializer, errors);
                }
                if let Some(condition) = condition {
                    let condition_ty = self.infer_expr(condition, errors);
                    if !is_assignable(&condition_ty, &Type::Bool)
                        && !matches!(condition_ty, Type::Unknown)
                    {
                        errors.push(TypeError::new(format!(
                            "for-loop condition must be bool, got '{}'",
                            condition_ty
                        )));
                    }
                }
                for stmt in body {
                    self.check_statement(stmt, errors);
                }
                if let Some(increment) = increment {
                    let _ = self.infer_expr(increment, errors);
                }
                self.pop_scope();
            }
            Stmt::ForIn {
                item_name,
                iterable,
                body,
            } => {
                let iterable_ty = self.infer_expr(iterable, errors);
                let item_ty = self.infer_iterable_item_type(&iterable_ty, errors);
                self.push_scope();
                self.define(item_name.clone(), item_ty);
                for stmt in body {
                    self.check_statement(stmt, errors);
                }
                self.pop_scope();
            }
            Stmt::Test { name: _, body } => {
                self.push_scope();
                for stmt in body {
                    self.check_statement(stmt, errors);
                }
                self.pop_scope();
            }
            Stmt::Assert { condition, message } => {
                let condition_ty = self.infer_expr(condition, errors);
                if !is_assignable(&condition_ty, &Type::Bool)
                    && !matches!(condition_ty, Type::Unknown)
                {
                    errors.push(TypeError::new(format!(
                        "assert condition must be bool, got '{}'",
                        condition_ty
                    )));
                }
                if let Some(message_expr) = message {
                    let message_ty = self.infer_expr(message_expr, errors);
                    if !is_assignable(&message_ty, &Type::String)
                        && !matches!(message_ty, Type::Unknown)
                    {
                        errors.push(TypeError::new(format!(
                            "assert message must be string, got '{}'",
                            message_ty
                        )));
                    }
                }
            }
            Stmt::VarDecl {
                name,
                type_annotation,
                initializer,
            } => {
                let inferred = self.infer_expr(initializer, errors);
                let declared = self.parse_declared_type(type_annotation.as_deref(), name, errors);
                let existing = self.lookup(name);
                let final_type = if !matches!(declared, Type::Unknown) {
                    if !is_assignable(&inferred, &declared) {
                        errors.push(TypeError::new(format!(
                            "cannot assign value of type '{}' to '{}' declared as '{}'",
                            inferred, name, declared
                        )));
                    }
                    declared
                } else if let Some(existing) = existing {
                    if !is_assignable(&inferred, &existing)
                        && !matches!(inferred, Type::Unknown)
                        && !matches!(existing, Type::Unknown)
                    {
                        errors.push(TypeError::new(format!(
                            "cannot assign value of type '{}' to variable '{}' of type '{}'",
                            inferred, name, existing
                        )));
                    }
                    existing
                } else {
                    inferred
                };
                self.assign_or_define(name.clone(), final_type);
            }
            Stmt::DestructureDecl {
                pattern,
                initializer,
            } => {
                let value_type = self.infer_expr(initializer, errors);
                self.bind_pattern(pattern, &value_type, errors);
            }
            Stmt::FunctionDef {
                name,
                params,
                return_type,
                body,
            } => self.check_function_definition(name, params, return_type.as_deref(), body, errors),
            Stmt::Return { value } => self.check_return_statement(value.as_ref(), errors),
            Stmt::Print { expr } | Stmt::Expr(expr) => {
                let _ = self.infer_expr(expr, errors);
            }
        }
    }

    fn check_function_definition(
        &mut self,
        name: &str,
        params: &[crate::parser::ast::Param],
        return_type: Option<&str>,
        body: &[Stmt],
        errors: &mut Vec<TypeError>,
    ) {
        let param_types = params
            .iter()
            .map(|param| {
                self.parse_declared_type(param.type_annotation.as_deref(), &param.name, errors)
            })
            .collect::<Vec<_>>();

        let declared_return = self.parse_declared_type(return_type, name, errors);

        let provisional = Type::Function {
            params: param_types.clone(),
            ret: Box::new(if matches!(declared_return, Type::Unknown) {
                Type::Unknown
            } else {
                declared_return.clone()
            }),
        };
        self.define(name.to_string(), provisional);

        self.push_scope();
        for (param, param_type) in params.iter().zip(param_types.iter()) {
            self.define(param.name.clone(), param_type.clone());
        }

        self.function_stack.push(FunctionContext {
            declared_return: declared_return.clone(),
            inferred_returns: Vec::new(),
        });

        for stmt in body {
            self.check_statement(stmt, errors);
        }

        let context = self.function_stack.pop().expect("function context exists");
        self.pop_scope();

        let inferred_return = if !matches!(declared_return, Type::Unknown) {
            declared_return
        } else if context.inferred_returns.is_empty() {
            Type::Nil
        } else {
            Type::Union(context.inferred_returns).normalize()
        };

        let final_fn_type = Type::Function {
            params: param_types,
            ret: Box::new(inferred_return),
        };
        self.assign_or_define(name.to_string(), final_fn_type);
    }

    fn check_return_statement(&mut self, value: Option<&Expr>, errors: &mut Vec<TypeError>) {
        let Some(declared_return) = self
            .function_stack
            .last()
            .map(|ctx| ctx.declared_return.clone())
        else {
            errors.push(TypeError::new("'return' can only appear inside a function"));
            return;
        };

        let return_type = if let Some(expr) = value {
            self.infer_expr(expr, errors)
        } else {
            Type::Nil
        };

        if !matches!(declared_return, Type::Unknown)
            && !is_assignable(&return_type, &declared_return)
        {
            errors.push(TypeError::new(format!(
                "return type '{}' is not assignable to function return type '{}'",
                return_type, declared_return
            )));
        }

        let ctx = self
            .function_stack
            .last_mut()
            .expect("function context exists");
        ctx.inferred_returns.push(return_type);
    }

    fn infer_expr(&mut self, expr: &Expr, errors: &mut Vec<TypeError>) -> Type {
        match expr {
            Expr::Int(_) => Type::Int,
            Expr::Float(_) => Type::Float,
            Expr::String { .. } => Type::String,
            Expr::Bool(_) => Type::Bool,
            Expr::Nil => Type::Nil,
            Expr::Variable(name) => self.lookup(name).unwrap_or_else(|| {
                if self.implicit_nil_for_unknown_variables {
                    Type::Nil
                } else {
                    errors.push(TypeError::new(format!("unknown variable '{}'", name)));
                    Type::Unknown
                }
            }),
            Expr::Unary { op, rhs } => {
                let rhs_type = self.infer_expr(rhs, errors);
                match op {
                    UnaryOp::Negate => {
                        if matches!(rhs_type, Type::Int | Type::Float | Type::Unknown) {
                            rhs_type
                        } else {
                            errors.push(TypeError::new(format!(
                                "operator '-' cannot be applied to '{}'",
                                rhs_type
                            )));
                            Type::Unknown
                        }
                    }
                    UnaryOp::Not => {
                        if matches!(rhs_type, Type::Bool | Type::Unknown) {
                            Type::Bool
                        } else {
                            errors.push(TypeError::new(format!(
                                "operator '!' expects bool, found '{}'",
                                rhs_type
                            )));
                            Type::Unknown
                        }
                    }
                }
            }
            Expr::Binary { lhs, op, rhs } => {
                let lhs_type = self.infer_expr(lhs, errors);
                let rhs_type = self.infer_expr(rhs, errors);
                self.infer_binary(op, &lhs_type, &rhs_type, errors)
            }
            Expr::Assign { name, value } => {
                let value_type = self.infer_expr(value, errors);
                let target_type = self.lookup(name).unwrap_or_else(|| {
                    errors.push(TypeError::new(format!(
                        "assignment to unknown variable '{}'",
                        name
                    )));
                    Type::Unknown
                });

                if !is_assignable(&value_type, &target_type) {
                    errors.push(TypeError::new(format!(
                        "cannot assign '{}' to variable '{}' of type '{}'",
                        value_type, name, target_type
                    )));
                }

                target_type
            }
            Expr::Grouping(inner) => self.infer_expr(inner, errors),
            Expr::Call { callee, args } => {
                let callee_type = self.infer_expr(callee, errors);
                let arg_types = args
                    .iter()
                    .map(|arg| self.infer_expr(arg, errors))
                    .collect::<Vec<_>>();
                self.infer_call(&callee_type, &arg_types, errors)
            }
            Expr::Member {
                object,
                property: _,
                optional,
            } => {
                let object_type = self.infer_expr(object, errors);
                if *optional {
                    Type::Union(vec![Type::Unknown, Type::Nil]).normalize()
                } else {
                    if matches!(object_type, Type::Nil) {
                        errors.push(TypeError::new(
                            "cannot access property on 'nil' without optional chaining",
                        ));
                    }
                    Type::Unknown
                }
            }
            Expr::Coalesce { lhs, rhs } => {
                let lhs_type = self.infer_expr(lhs, errors);
                let rhs_type = self.infer_expr(rhs, errors);
                let non_nil = remove_nil(&lhs_type);
                if contains_nil(&lhs_type) {
                    Type::Union(vec![non_nil, rhs_type]).normalize()
                } else {
                    lhs_type
                }
            }
            Expr::OrReturn { lhs, return_value } => {
                let lhs_type = self.infer_expr(lhs, errors);
                let return_type = self.infer_expr(return_value, errors);
                self.record_implicit_return(return_type, errors);
                successful_value_type(&lhs_type)
            }
            Expr::PanicUnwrap(inner) => {
                let inner_type = self.infer_expr(inner, errors);
                successful_value_type(&inner_type)
            }
            Expr::Match { subject, arms } => {
                let subject_type = self.infer_expr(subject, errors);
                self.infer_match(subject_type, arms, errors)
            }
            Expr::ListLiteral(items) => {
                if items.is_empty() {
                    Type::List(Box::new(Type::Unknown))
                } else {
                    let mut types = Vec::new();
                    for item in items {
                        types.push(self.infer_expr(item, errors));
                    }
                    Type::List(Box::new(Type::Union(types).normalize()))
                }
            }
            Expr::ListComprehension {
                expr,
                item_name,
                iterable,
                condition,
            } => {
                let iterable_type = self.infer_expr(iterable, errors);
                let item_type = match iterable_type {
                    Type::List(inner) => (*inner).clone(),
                    Type::Unknown => Type::Unknown,
                    other => {
                        errors.push(TypeError::new(format!(
                            "list comprehension requires list iterable, found '{}'",
                            other
                        )));
                        Type::Unknown
                    }
                };

                self.push_scope();
                self.define(item_name.clone(), item_type);
                if let Some(condition_expr) = condition {
                    let condition_type = self.infer_expr(condition_expr, errors);
                    if !is_assignable(&condition_type, &Type::Bool)
                        && !matches!(condition_type, Type::Unknown)
                    {
                        errors.push(TypeError::new(format!(
                            "list comprehension condition must be bool, got '{}'",
                            condition_type
                        )));
                    }
                }
                let body_type = self.infer_expr(expr, errors);
                self.pop_scope();
                Type::List(Box::new(body_type))
            }
            Expr::MapLiteral(entries) => {
                if entries.is_empty() {
                    Type::Map(Box::new(Type::String), Box::new(Type::Unknown))
                } else {
                    let mut value_types = Vec::new();
                    for entry in entries {
                        value_types.push(self.infer_expr(&entry.value, errors));
                    }
                    Type::Map(
                        Box::new(Type::String),
                        Box::new(Type::Union(value_types).normalize()),
                    )
                }
            }
            Expr::Index { object, index } => {
                let object_type = self.infer_expr(object, errors);
                let index_type = self.infer_expr(index, errors);
                match object_type {
                    Type::List(inner) => {
                        if !is_assignable(&index_type, &Type::Int)
                            && !matches!(index_type, Type::Unknown)
                        {
                            errors.push(TypeError::new(format!(
                                "list index must be int, got '{}'",
                                index_type
                            )));
                        }
                        *inner
                    }
                    Type::Map(_, value) => {
                        if !is_assignable(&index_type, &Type::String)
                            && !matches!(index_type, Type::Unknown)
                        {
                            errors.push(TypeError::new(format!(
                                "map index must be string, got '{}'",
                                index_type
                            )));
                        }
                        *value
                    }
                    Type::String => {
                        if !is_assignable(&index_type, &Type::Int)
                            && !matches!(index_type, Type::Unknown)
                        {
                            errors.push(TypeError::new(format!(
                                "string index must be int, got '{}'",
                                index_type
                            )));
                        }
                        Type::String
                    }
                    Type::Unknown => Type::Unknown,
                    other => {
                        errors.push(TypeError::new(format!("cannot index type '{}'", other)));
                        Type::Unknown
                    }
                }
            }
            Expr::Function {
                params,
                return_type,
                body,
            } => self.infer_function_expr(params, return_type.as_deref(), body, errors),
        }
    }

    fn infer_function_expr(
        &mut self,
        params: &[crate::parser::ast::Param],
        return_type: Option<&str>,
        body: &[Stmt],
        errors: &mut Vec<TypeError>,
    ) -> Type {
        let param_types = params
            .iter()
            .map(|param| {
                self.parse_declared_type(param.type_annotation.as_deref(), &param.name, errors)
            })
            .collect::<Vec<_>>();

        let declared_return = self.parse_declared_type(return_type, "<anonymous function>", errors);

        self.push_scope();
        for (param, param_type) in params.iter().zip(param_types.iter()) {
            self.define(param.name.clone(), param_type.clone());
        }

        self.function_stack.push(FunctionContext {
            declared_return: declared_return.clone(),
            inferred_returns: Vec::new(),
        });

        for stmt in body {
            self.check_statement(stmt, errors);
        }

        let context = self.function_stack.pop().expect("function context exists");
        self.pop_scope();

        let inferred_return = if !matches!(declared_return, Type::Unknown) {
            declared_return
        } else if context.inferred_returns.is_empty() {
            Type::Nil
        } else {
            Type::Union(context.inferred_returns).normalize()
        };

        Type::Function {
            params: param_types,
            ret: Box::new(inferred_return),
        }
    }

    fn infer_match(
        &mut self,
        subject_type: Type,
        arms: &[MatchArm],
        errors: &mut Vec<TypeError>,
    ) -> Type {
        if arms.is_empty() {
            errors.push(TypeError::new(
                "match expression must contain at least one arm",
            ));
            return Type::Unknown;
        }

        let mut arm_types = Vec::new();
        for arm in arms {
            self.push_scope();
            self.validate_pattern(&arm.pattern, &subject_type, errors);
            self.bind_pattern(&arm.pattern, &subject_type, errors);
            arm_types.push(self.infer_expr(&arm.value, errors));
            self.pop_scope();
        }
        Type::Union(arm_types).normalize()
    }

    fn validate_pattern(
        &self,
        pattern: &Pattern,
        subject_type: &Type,
        errors: &mut Vec<TypeError>,
    ) {
        let expected = pattern_expected_type(pattern);
        if let Some(expected_type) = expected {
            if !is_assignable(&expected_type, subject_type)
                && !is_assignable(subject_type, &expected_type)
                && !matches!(subject_type, Type::Unknown)
            {
                errors.push(TypeError::new(format!(
                    "pattern type '{}' does not match subject type '{}'",
                    expected_type, subject_type
                )));
            }
        }
    }

    fn bind_pattern(&mut self, pattern: &Pattern, value_type: &Type, errors: &mut Vec<TypeError>) {
        match pattern {
            Pattern::Wildcard => {}
            Pattern::Identifier(name) => {
                self.define(name.clone(), value_type.clone());
            }
            Pattern::Int(_)
            | Pattern::Float(_)
            | Pattern::String(_)
            | Pattern::Bool(_)
            | Pattern::Nil => {}
            Pattern::List(items) => {
                let item_type = match value_type {
                    Type::List(inner) => (**inner).clone(),
                    Type::Unknown => Type::Unknown,
                    other => {
                        errors.push(TypeError::new(format!(
                            "list pattern used with non-list type '{}'",
                            other
                        )));
                        Type::Unknown
                    }
                };
                for item in items {
                    self.bind_pattern(item, &item_type, errors);
                }
            }
            Pattern::Map(entries) => {
                let value_item_type = match value_type {
                    Type::Map(_, inner_value) => (**inner_value).clone(),
                    Type::Unknown => Type::Unknown,
                    other => {
                        errors.push(TypeError::new(format!(
                            "map pattern used with non-map type '{}'",
                            other
                        )));
                        Type::Unknown
                    }
                };
                for entry in entries {
                    self.bind_pattern(&entry.pattern, &value_item_type, errors);
                }
            }
        }
    }

    fn infer_binary(
        &self,
        op: &BinaryOp,
        lhs_type: &Type,
        rhs_type: &Type,
        errors: &mut Vec<TypeError>,
    ) -> Type {
        match op {
            BinaryOp::Add => {
                if matches!(lhs_type, Type::String) && matches!(rhs_type, Type::String) {
                    return Type::String;
                }
                if let Some(numeric) = common_numeric_type(lhs_type, rhs_type) {
                    return numeric;
                }
                if matches!(lhs_type, Type::Unknown) || matches!(rhs_type, Type::Unknown) {
                    return Type::Unknown;
                }
                errors.push(TypeError::new(format!(
                    "operator '+' expects (string,string) or numeric types, found ('{}', '{}')",
                    lhs_type, rhs_type
                )));
                Type::Unknown
            }
            BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Modulo => {
                if let Some(numeric) = common_numeric_type(lhs_type, rhs_type) {
                    return numeric;
                }
                if matches!(lhs_type, Type::Unknown) || matches!(rhs_type, Type::Unknown) {
                    return Type::Unknown;
                }
                errors.push(TypeError::new(format!(
                    "numeric operator expects numeric types, found ('{}', '{}')",
                    lhs_type, rhs_type
                )));
                Type::Unknown
            }
            BinaryOp::Divide => {
                if common_numeric_type(lhs_type, rhs_type).is_some() {
                    return Type::Float;
                }
                if matches!(lhs_type, Type::Unknown) || matches!(rhs_type, Type::Unknown) {
                    return Type::Unknown;
                }
                errors.push(TypeError::new(format!(
                    "operator '/' expects numeric types, found ('{}', '{}')",
                    lhs_type, rhs_type
                )));
                Type::Unknown
            }
            BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => {
                if common_numeric_type(lhs_type, rhs_type).is_some()
                    || matches!(lhs_type, Type::Unknown)
                    || matches!(rhs_type, Type::Unknown)
                {
                    return Type::Bool;
                }
                errors.push(TypeError::new(format!(
                    "comparison expects numeric types, found ('{}', '{}')",
                    lhs_type, rhs_type
                )));
                Type::Unknown
            }
            BinaryOp::Equal | BinaryOp::NotEqual => {
                if is_assignable(lhs_type, rhs_type)
                    || is_assignable(rhs_type, lhs_type)
                    || contains_nil(lhs_type)
                    || contains_nil(rhs_type)
                    || matches!(lhs_type, Type::Unknown)
                    || matches!(rhs_type, Type::Unknown)
                {
                    Type::Bool
                } else {
                    errors.push(TypeError::new(format!(
                        "cannot compare '{}' and '{}' with equality",
                        lhs_type, rhs_type
                    )));
                    Type::Unknown
                }
            }
        }
    }

    fn infer_call(
        &self,
        callee_type: &Type,
        arg_types: &[Type],
        errors: &mut Vec<TypeError>,
    ) -> Type {
        match callee_type {
            Type::Function { params, ret } => {
                if params.len() != arg_types.len() {
                    errors.push(TypeError::new(format!(
                        "function expected {} argument(s), got {}",
                        params.len(),
                        arg_types.len()
                    )));
                    return Type::Unknown;
                }
                for (index, (arg, expected)) in arg_types.iter().zip(params.iter()).enumerate() {
                    if !is_assignable(arg, expected) {
                        errors.push(TypeError::new(format!(
                            "argument {} expected '{}', got '{}'",
                            index + 1,
                            expected,
                            arg
                        )));
                    }
                }
                (**ret).clone()
            }
            Type::Unknown => Type::Unknown,
            other => {
                errors.push(TypeError::new(format!("type '{}' is not callable", other)));
                Type::Unknown
            }
        }
    }

    fn parse_declared_type(
        &self,
        raw: Option<&str>,
        name: &str,
        errors: &mut Vec<TypeError>,
    ) -> Type {
        let Some(raw_annotation) = raw else {
            return Type::Unknown;
        };

        match Type::parse(raw_annotation) {
            Ok(annotation_type) => annotation_type.normalize(),
            Err(err) => {
                errors.push(TypeError::new(format!(
                    "invalid type annotation on '{}': {}",
                    name, err
                )));
                Type::Unknown
            }
        }
    }

    fn record_implicit_return(&mut self, return_type: Type, errors: &mut Vec<TypeError>) {
        let Some(ctx) = self.function_stack.last_mut() else {
            errors.push(TypeError::new(
                "'or return' can only appear inside a function",
            ));
            return;
        };
        if !matches!(ctx.declared_return, Type::Unknown)
            && !is_assignable(&return_type, &ctx.declared_return)
        {
            errors.push(TypeError::new(format!(
                "'or return' value '{}' is not assignable to function return type '{}'",
                return_type, ctx.declared_return
            )));
        }
        ctx.inferred_returns.push(return_type);
    }

    fn infer_iterable_item_type(&self, iterable_ty: &Type, errors: &mut Vec<TypeError>) -> Type {
        match iterable_ty {
            Type::List(inner) => (**inner).clone(),
            Type::Map(_, _) => Type::String,
            Type::String => Type::String,
            Type::Unknown => Type::Unknown,
            other => {
                errors.push(TypeError::new(format!(
                    "for-in requires list, map, or string iterable, got '{}'",
                    other
                )));
                Type::Unknown
            }
        }
    }

    fn define(&mut self, name: String, ty: Type) {
        let scope = self.scopes.last_mut().expect("scope exists");
        scope.insert(name, ty);
    }

    fn assign_or_define(&mut self, name: String, ty: Type) {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(&name) {
                scope.insert(name.clone(), ty);
                return;
            }
        }
        self.define(name, ty);
    }

    fn lookup(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
    }
}

fn contains_nil(ty: &Type) -> bool {
    match ty {
        Type::Nil => true,
        Type::Nullable(_) => true,
        Type::Union(items) => items.iter().any(contains_nil),
        _ => false,
    }
}

fn remove_nil(ty: &Type) -> Type {
    match ty {
        Type::Nil => Type::Unknown,
        Type::Nullable(inner) => (**inner).clone().normalize(),
        Type::Union(items) => {
            let filtered = items
                .iter()
                .filter(|candidate| !matches!(candidate, Type::Nil))
                .cloned()
                .collect::<Vec<_>>();
            if filtered.is_empty() {
                Type::Unknown
            } else {
                Type::Union(filtered).normalize()
            }
        }
        other => other.clone(),
    }
}

fn successful_value_type(ty: &Type) -> Type {
    match ty {
        Type::Result(ok, _) => (**ok).clone().normalize(),
        Type::Nullable(inner) => (**inner).clone().normalize(),
        Type::Union(items) => {
            let filtered = items
                .iter()
                .filter(|item| !matches!(item, Type::Error | Type::Nil))
                .cloned()
                .collect::<Vec<_>>();
            if filtered.is_empty() {
                Type::Unknown
            } else {
                Type::Union(filtered).normalize()
            }
        }
        other => other.clone(),
    }
}

fn pattern_expected_type(pattern: &Pattern) -> Option<Type> {
    match pattern {
        Pattern::Wildcard | Pattern::Identifier(_) => None,
        Pattern::Int(_) => Some(Type::Int),
        Pattern::Float(_) => Some(Type::Float),
        Pattern::String(_) => Some(Type::String),
        Pattern::Bool(_) => Some(Type::Bool),
        Pattern::Nil => Some(Type::Nil),
        Pattern::List(_) => Some(Type::List(Box::new(Type::Unknown))),
        Pattern::Map(_) => Some(Type::Map(Box::new(Type::Unknown), Box::new(Type::Unknown))),
    }
}
