use crate::parser::ast::{
    BinaryOp, Expr, MapEntryExpr, MatchArm, Pattern, Program, Stmt, UnaryOp, UseTarget,
};

pub fn format_program(program: &Program) -> String {
    let mut lines = Vec::new();
    for statement in &program.statements {
        lines.push(format_statement(statement, 0));
    }
    let mut out = lines.join("\n");
    if !out.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn format_statement(statement: &Stmt, indent: usize) -> String {
    let pad = "  ".repeat(indent);
    match statement {
        Stmt::Use { target, alias } => {
            let target_text = match target {
                UseTarget::ModulePath(path) => path.join("."),
                UseTarget::Url(url) => format!("\"{}\"", escape_string(url)),
            };
            if let Some(alias) = alias {
                format!("{}use {} as {}", pad, target_text, alias)
            } else {
                format!("{}use {}", pad, target_text)
            }
        }
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let mut out = format!("{}if {} {{", pad, format_expr(condition));
            if then_branch.is_empty() {
                out.push('}');
            } else {
                out.push('\n');
                for statement in then_branch {
                    out.push_str(&format_statement(statement, indent + 1));
                    out.push('\n');
                }
                out.push_str(&format!("{}}}", pad));
            }

            if let Some(else_branch) = else_branch {
                if else_branch.len() == 1 {
                    if let Stmt::If { .. } = else_branch[0] {
                        out.push_str(" else ");
                        out.push_str(format_statement(&else_branch[0], indent).trim_start());
                        return out;
                    }
                }

                out.push_str(" else {");
                if else_branch.is_empty() {
                    out.push('}');
                } else {
                    out.push('\n');
                    for statement in else_branch {
                        out.push_str(&format_statement(statement, indent + 1));
                        out.push('\n');
                    }
                    out.push_str(&format!("{}}}", pad));
                }
            }

            out
        }
        Stmt::While { condition, body } => {
            let mut out = format!("{}while {} {{", pad, format_expr(condition));
            if body.is_empty() {
                out.push('}');
                return out;
            }
            out.push('\n');
            for statement in body {
                out.push_str(&format_statement(statement, indent + 1));
                out.push('\n');
            }
            out.push_str(&format!("{}}}", pad));
            out
        }
        Stmt::For {
            initializer,
            condition,
            increment,
            body,
        } => {
            let init = initializer
                .as_ref()
                .map(|stmt| format_for_initializer(stmt))
                .unwrap_or_default();
            let cond = condition.as_ref().map(format_expr).unwrap_or_default();
            let inc = increment.as_ref().map(format_expr).unwrap_or_default();
            let mut out = format!("{}for {}; {}; {} {{", pad, init, cond, inc);
            if body.is_empty() {
                out.push('}');
                return out;
            }
            out.push('\n');
            for statement in body {
                out.push_str(&format_statement(statement, indent + 1));
                out.push('\n');
            }
            out.push_str(&format!("{}}}", pad));
            out
        }
        Stmt::ForIn {
            item_name,
            iterable,
            body,
        } => {
            let mut out = format!("{}for {} in {} {{", pad, item_name, format_expr(iterable));
            if body.is_empty() {
                out.push('}');
                return out;
            }
            out.push('\n');
            for statement in body {
                out.push_str(&format_statement(statement, indent + 1));
                out.push('\n');
            }
            out.push_str(&format!("{}}}", pad));
            out
        }
        Stmt::Test { name, body } => {
            let mut out = format!("{}test \"{}\" {{", pad, escape_string(name));
            if body.is_empty() {
                out.push('}');
                return out;
            }
            out.push('\n');
            for statement in body {
                out.push_str(&format_statement(statement, indent + 1));
                out.push('\n');
            }
            out.push_str(&format!("{}}}", pad));
            out
        }
        Stmt::Assert { condition, message } => {
            if let Some(message) = message {
                format!(
                    "{}assert {}, {}",
                    pad,
                    format_expr(condition),
                    format_expr(message)
                )
            } else {
                format!("{}assert {}", pad, format_expr(condition))
            }
        }
        Stmt::VarDecl {
            name,
            type_annotation,
            initializer,
        } => {
            if let Some(annotation) = type_annotation {
                format!(
                    "{}{}: {} = {}",
                    pad,
                    name,
                    annotation,
                    format_expr(initializer)
                )
            } else {
                format!("{}{} = {}", pad, name, format_expr(initializer))
            }
        }
        Stmt::DestructureDecl {
            pattern,
            initializer,
        } => {
            format!(
                "{}{} = {}",
                pad,
                format_pattern(pattern),
                format_expr(initializer)
            )
        }
        Stmt::FunctionDef {
            name,
            params,
            return_type,
            body,
        } => {
            let rendered_params = params
                .iter()
                .map(|param| {
                    if let Some(annotation) = &param.type_annotation {
                        format!("{}: {}", param.name, annotation)
                    } else {
                        param.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");

            let mut header = format!("{}def {}({})", pad, name, rendered_params);
            if let Some(ret) = return_type {
                header.push_str(&format!(" -> {}", ret));
            }
            header.push_str(" {");
            if body.is_empty() {
                header.push('}');
                return header;
            }

            let mut out = header;
            out.push('\n');
            for statement in body {
                out.push_str(&format_statement(statement, indent + 1));
                out.push('\n');
            }
            out.push_str(&format!("{}}}", pad));
            out
        }
        Stmt::Return { value } => {
            if let Some(value) = value {
                format!("{}return {}", pad, format_expr(value))
            } else {
                format!("{}return", pad)
            }
        }
        Stmt::Print { expr } => format!("{}print({})", pad, format_expr(expr)),
        Stmt::Expr(expr) => format!("{}{}", pad, format_expr(expr)),
    }
}

fn format_for_initializer(statement: &Stmt) -> String {
    match statement {
        Stmt::VarDecl {
            name,
            type_annotation,
            initializer,
        } => {
            if let Some(annotation) = type_annotation {
                format!("{}: {} = {}", name, annotation, format_expr(initializer))
            } else {
                format!("{} = {}", name, format_expr(initializer))
            }
        }
        Stmt::Expr(expr) => format_expr(expr),
        _ => format_statement(statement, 0),
    }
}

fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Int(value) => value.to_string(),
        Expr::Float(value) => value.to_string(),
        Expr::String { value, .. } => format!("\"{}\"", escape_string(value)),
        Expr::Bool(value) => value.to_string(),
        Expr::Nil => "nil".to_string(),
        Expr::Variable(name) => name.clone(),
        Expr::Unary { op, rhs } => format!("{}{}", unary_symbol(*op), wrap_expr(rhs)),
        Expr::Binary { lhs, op, rhs } => {
            format!(
                "{} {} {}",
                wrap_expr(lhs),
                binary_symbol(*op),
                wrap_expr(rhs)
            )
        }
        Expr::Assign { name, value } => format!("{} = {}", name, format_expr(value)),
        Expr::Grouping(inner) => format!("({})", format_expr(inner)),
        Expr::Call { callee, args } => {
            let rendered_args = args.iter().map(format_expr).collect::<Vec<_>>().join(", ");
            format!("{}({})", format_expr(callee), rendered_args)
        }
        Expr::Member {
            object,
            property,
            optional,
        } => {
            if *optional {
                format!("{}?.{}", format_expr(object), property)
            } else {
                format!("{}.{}", format_expr(object), property)
            }
        }
        Expr::Coalesce { lhs, rhs } => format!("{} or {}", format_expr(lhs), format_expr(rhs)),
        Expr::OrReturn { lhs, return_value } => {
            format!(
                "{} or return {}",
                format_expr(lhs),
                format_expr(return_value)
            )
        }
        Expr::PanicUnwrap(inner) => format!("{}!", wrap_expr(inner)),
        Expr::Match { subject, arms } => {
            let rendered_arms = arms
                .iter()
                .map(format_match_arm)
                .collect::<Vec<_>>()
                .join(", ");
            format!("match {} {{ {} }}", format_expr(subject), rendered_arms)
        }
        Expr::ListLiteral(items) => {
            let rendered_items = items.iter().map(format_expr).collect::<Vec<_>>().join(", ");
            format!("[{}]", rendered_items)
        }
        Expr::ListComprehension {
            expr,
            item_name,
            iterable,
            condition,
        } => {
            let mut out = format!(
                "[{} for {} in {}",
                format_expr(expr),
                item_name,
                format_expr(iterable)
            );
            if let Some(condition) = condition {
                out.push_str(&format!(" if {}", format_expr(condition)));
            }
            out.push(']');
            out
        }
        Expr::MapLiteral(entries) => {
            let rendered = entries
                .iter()
                .map(format_map_entry)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", rendered)
        }
        Expr::Index { object, index } => format!("{}[{}]", format_expr(object), format_expr(index)),
        Expr::Function {
            params,
            return_type,
            body,
        } => {
            let rendered_params = params
                .iter()
                .map(|param| {
                    if let Some(annotation) = &param.type_annotation {
                        format!("{}: {}", param.name, annotation)
                    } else {
                        param.name.clone()
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            let mut out = format!("def({})", rendered_params);
            if let Some(ret) = return_type {
                out.push_str(&format!(" -> {}", ret));
            }
            out.push_str(" {");
            if body.is_empty() {
                out.push('}');
                return out;
            }
            let rendered_body = body
                .iter()
                .map(format_inline_statement)
                .collect::<Vec<_>>()
                .join("; ");
            out.push(' ');
            out.push_str(&rendered_body);
            out.push_str(" }");
            out
        }
    }
}

fn wrap_expr(expr: &Expr) -> String {
    match expr {
        Expr::Binary { .. } | Expr::Coalesce { .. } | Expr::OrReturn { .. } => {
            format!("({})", format_expr(expr))
        }
        _ => format_expr(expr),
    }
}

fn format_match_arm(arm: &MatchArm) -> String {
    format!(
        "{} => {}",
        format_pattern(&arm.pattern),
        format_expr(&arm.value)
    )
}

fn format_map_entry(entry: &MapEntryExpr) -> String {
    if is_plain_identifier(&entry.key) {
        format!("{}: {}", entry.key, format_expr(&entry.value))
    } else {
        format!(
            "\"{}\": {}",
            escape_string(&entry.key),
            format_expr(&entry.value)
        )
    }
}

fn format_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Wildcard => "_".to_string(),
        Pattern::Identifier(name) => name.clone(),
        Pattern::Int(value) => value.to_string(),
        Pattern::Float(value) => value.to_string(),
        Pattern::String(value) => format!("\"{}\"", escape_string(value)),
        Pattern::Bool(value) => value.to_string(),
        Pattern::Nil => "nil".to_string(),
        Pattern::List(items) => {
            let rendered = items
                .iter()
                .map(format_pattern)
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{}]", rendered)
        }
        Pattern::Map(entries) => {
            let rendered = entries
                .iter()
                .map(|entry| {
                    if matches!(entry.pattern, Pattern::Identifier(ref name) if name == &entry.key)
                    {
                        entry.key.clone()
                    } else {
                        format!("{}: {}", entry.key, format_pattern(&entry.pattern))
                    }
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", rendered)
        }
    }
}

fn unary_symbol(op: UnaryOp) -> &'static str {
    match op {
        UnaryOp::Negate => "-",
        UnaryOp::Not => "!",
    }
}

fn binary_symbol(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Modulo => "%",
        BinaryOp::And => "and",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
    }
}

fn is_plain_identifier(text: &str) -> bool {
    let mut chars = text.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }
    chars.all(|c| c == '_' || c.is_ascii_alphanumeric())
}

fn escape_string(input: &str) -> String {
    let mut out = String::new();
    for ch in input.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            other => out.push(other),
        }
    }
    out
}

fn format_inline_statement(statement: &Stmt) -> String {
    format_statement(statement, 0)
        .replace('\n', " ")
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}
