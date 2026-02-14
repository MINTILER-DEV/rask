use crate::parser::ast::{Expr, Program, Stmt};

#[derive(Debug, Clone)]
pub struct LintWarning {
    pub code: &'static str,
    pub message: String,
    pub hint: Option<String>,
}

pub fn lint_program(program: &Program) -> Vec<LintWarning> {
    let mut warnings = Vec::new();
    for statement in &program.statements {
        lint_statement(statement, &mut warnings);
    }
    warnings
}

fn lint_statement(statement: &Stmt, warnings: &mut Vec<LintWarning>) {
    match statement {
        Stmt::Use { .. } => {}
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            lint_expr(condition, warnings);
            for stmt in then_branch {
                lint_statement(stmt, warnings);
            }
            if let Some(else_branch) = else_branch {
                for stmt in else_branch {
                    lint_statement(stmt, warnings);
                }
            }
        }
        Stmt::While { condition, body } => {
            lint_expr(condition, warnings);
            for stmt in body {
                lint_statement(stmt, warnings);
            }
        }
        Stmt::For {
            initializer,
            condition,
            increment,
            body,
        } => {
            if let Some(initializer) = initializer {
                lint_statement(initializer, warnings);
            }
            if let Some(condition) = condition {
                lint_expr(condition, warnings);
            }
            if let Some(increment) = increment {
                lint_expr(increment, warnings);
            }
            for stmt in body {
                lint_statement(stmt, warnings);
            }
        }
        Stmt::ForIn { iterable, body, .. } => {
            lint_expr(iterable, warnings);
            for stmt in body {
                lint_statement(stmt, warnings);
            }
        }
        Stmt::Test { name, body } => {
            if body.is_empty() {
                warnings.push(LintWarning {
                    code: "LINT0002",
                    message: format!("test '{}' is empty", name),
                    hint: Some("add assertions or remove the test".to_string()),
                });
            }
            for stmt in body {
                lint_statement(stmt, warnings);
            }
        }
        Stmt::Assert { condition, message } => {
            lint_expr(condition, warnings);
            if let Some(message) = message {
                lint_expr(message, warnings);
            }
            if matches!(condition, Expr::Bool(true)) {
                warnings.push(LintWarning {
                    code: "LINT0005",
                    message: "assert true has no effect".to_string(),
                    hint: Some("replace with a meaningful condition".to_string()),
                });
            }
        }
        Stmt::VarDecl { initializer, .. } => lint_expr(initializer, warnings),
        Stmt::DestructureDecl { initializer, .. } => lint_expr(initializer, warnings),
        Stmt::FunctionDef { body, .. } => {
            for stmt in body {
                lint_statement(stmt, warnings);
            }
        }
        Stmt::Return { value } => {
            if let Some(value) = value {
                lint_expr(value, warnings);
            }
        }
        Stmt::Print { expr } => lint_expr(expr, warnings),
        Stmt::Expr(expr) => {
            lint_expr(expr, warnings);
            if is_pure_literal(expr) {
                warnings.push(LintWarning {
                    code: "LINT0004",
                    message: "expression statement has no effect".to_string(),
                    hint: Some("assign it, print it, or remove it".to_string()),
                });
            }
        }
    }
}

fn lint_expr(expr: &Expr, warnings: &mut Vec<LintWarning>) {
    match expr {
        Expr::PanicUnwrap(inner) => {
            warnings.push(LintWarning {
                code: "LINT0001",
                message: "panic unwrap operator '!' can crash at runtime".to_string(),
                hint: Some("prefer 'or' fallback or explicit error handling".to_string()),
            });
            lint_expr(inner, warnings);
        }
        Expr::Unary { rhs, .. } => lint_expr(rhs, warnings),
        Expr::Binary { lhs, rhs, .. } => {
            lint_expr(lhs, warnings);
            lint_expr(rhs, warnings);
        }
        Expr::Assign { value, .. } => lint_expr(value, warnings),
        Expr::Grouping(inner) => lint_expr(inner, warnings),
        Expr::Call { callee, args } => {
            lint_expr(callee, warnings);
            for arg in args {
                lint_expr(arg, warnings);
            }
        }
        Expr::Member { object, .. } => lint_expr(object, warnings),
        Expr::Coalesce { lhs, rhs } => {
            lint_expr(lhs, warnings);
            lint_expr(rhs, warnings);
        }
        Expr::OrReturn { lhs, return_value } => {
            lint_expr(lhs, warnings);
            lint_expr(return_value, warnings);
        }
        Expr::Match { subject, arms } => {
            lint_expr(subject, warnings);
            for arm in arms {
                lint_expr(&arm.value, warnings);
            }
        }
        Expr::ListLiteral(items) => {
            for item in items {
                lint_expr(item, warnings);
            }
        }
        Expr::ListComprehension {
            expr,
            iterable,
            condition,
            ..
        } => {
            lint_expr(expr, warnings);
            lint_expr(iterable, warnings);
            if let Some(condition) = condition {
                lint_expr(condition, warnings);
            }
        }
        Expr::MapLiteral(entries) => {
            for entry in entries {
                lint_expr(&entry.value, warnings);
            }
        }
        Expr::Index { object, index } => {
            lint_expr(object, warnings);
            lint_expr(index, warnings);
        }
        Expr::Function { body, .. } => {
            for statement in body {
                lint_statement(statement, warnings);
            }
        }
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String { .. }
        | Expr::Bool(_)
        | Expr::Nil
        | Expr::Variable(_) => {}
    }
}

fn is_pure_literal(expr: &Expr) -> bool {
    match expr {
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String { .. }
        | Expr::Bool(_)
        | Expr::Nil
        | Expr::ListLiteral(_)
        | Expr::MapLiteral(_) => true,
        Expr::Grouping(inner) => is_pure_literal(inner),
        _ => false,
    }
}
