pub mod highlighter;

use rustyline::completion::{Completer, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::history::DefaultHistory;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Context, Editor, Helper};
use std::collections::HashMap;
use std::error::Error as StdError;
use std::error::Error;
use std::fs;
use std::io::{self, IsTerminal, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Instant;

const MAX_HISTORY_ENTRIES: usize = 500;

enum CommandAction {
    NotHandled,
    Handled,
    Exit,
}

#[derive(Clone)]
struct ReplEditorHelper {
    symbols: Arc<Mutex<Vec<String>>>,
}

impl ReplEditorHelper {
    fn new() -> Self {
        Self {
            symbols: Arc::new(Mutex::new(Vec::new())),
        }
    }

    fn set_symbols(&self, symbols: Vec<String>) {
        if let Ok(mut guard) = self.symbols.lock() {
            *guard = symbols;
        }
    }

    fn symbols(&self) -> Vec<String> {
        self.symbols
            .lock()
            .map(|guard| guard.clone())
            .unwrap_or_default()
    }
}

impl Helper for ReplEditorHelper {}

impl Hinter for ReplEditorHelper {
    type Hint = String;
}

impl Validator for ReplEditorHelper {
    fn validate(
        &self,
        context: &mut ValidationContext<'_>,
    ) -> Result<ValidationResult, ReadlineError> {
        if highlighter::needs_more_input(context.input()) {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

impl Highlighter for ReplEditorHelper {
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> std::borrow::Cow<'l, str> {
        std::borrow::Cow::Owned(highlighter::colorize(line))
    }
}

impl Completer for ReplEditorHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        _ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), ReadlineError> {
        let line = &line[..pos.min(line.len())];
        let mut start = line.len();
        for (idx, ch) in line.char_indices().rev() {
            if ch == '_' || ch.is_ascii_alphanumeric() {
                start = idx;
            } else {
                break;
            }
        }

        let prefix = &line[start..];
        if prefix.is_empty() {
            return Ok((start, Vec::new()));
        }

        let suggestions = highlighter::complete(prefix, &self.symbols());
        let pairs = suggestions
            .into_iter()
            .map(|value| Pair {
                display: value.clone(),
                replacement: value,
            })
            .collect::<Vec<_>>();
        Ok((start, pairs))
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    run_with_permissions(crate::runtime::Permissions::default(), false)
}

pub fn run_with_permissions(
    permissions: crate::runtime::Permissions,
    implicit_nil_for_unknown_variables: bool,
) -> Result<(), Box<dyn Error>> {
    let mut multiline_buffer = String::new();
    let mut checker = crate::typechecker::TypeChecker::new()
        .with_implicit_nil_for_unknown_variables(implicit_nil_for_unknown_variables);
    let mut runtime = crate::runtime::Runtime::with_permissions(permissions)
        .with_implicit_nil_for_unknown_variables(implicit_nil_for_unknown_variables)
        .with_source_label("<repl>");
    let mut history = load_history();
    let docs = repl_docs();
    let mut show_timing = true;

    println!("Scalf REPL");
    println!("Commands: :help, :doc <symbol>, :complete <prefix>, :history, :timing on|off, :quit");
    println!("Multi-line mode is automatic when input is syntactically incomplete.");

    if io::stdin().is_terminal() {
        let helper = ReplEditorHelper::new();
        let mut editor = Editor::<ReplEditorHelper, DefaultHistory>::new()?;
        editor.set_helper(Some(helper.clone()));

        for entry in &history {
            let _ = editor.add_history_entry(entry.as_str());
        }

        loop {
            let prompt = if multiline_buffer.is_empty() {
                "scalf> "
            } else {
                "....> "
            };

            let raw_line = match editor.readline(prompt) {
                Ok(line) => line,
                Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => break,
                Err(err) => return Err(Box::<dyn StdError>::from(err)),
            };

            if multiline_buffer.is_empty() {
                match handle_meta_command(
                    raw_line.trim(),
                    &mut history,
                    &mut show_timing,
                    &checker,
                    &docs,
                )? {
                    CommandAction::NotHandled => {}
                    CommandAction::Handled => continue,
                    CommandAction::Exit => break,
                }
            }

            let normalized = highlighter::normalize_line(&raw_line);
            if multiline_buffer.is_empty() && normalized.trim().is_empty() {
                continue;
            }

            if !multiline_buffer.is_empty() {
                multiline_buffer.push('\n');
            }
            multiline_buffer.push_str(&normalized);

            if highlighter::needs_more_input(&multiline_buffer) {
                continue;
            }

            let input = std::mem::take(&mut multiline_buffer);
            if !input.trim().is_empty() {
                let _ = editor.add_history_entry(input.as_str());
                history.push(input.clone());
                trim_history(&mut history);
            }

            let started = Instant::now();
            match crate::lexer::lex(input.as_str()) {
                Ok(tokens) => {
                    let mut parser = crate::parser::Parser::new(tokens);
                    match parser.parse_program() {
                        Ok(program) => match checker.check_program(&program) {
                            Ok(_) => {
                                let should_echo = should_echo_repl_result(&program);
                                if let Some(editor_helper) = editor.helper_mut() {
                                    editor_helper.set_symbols(
                                        checker.inferred_types().keys().cloned().collect(),
                                    );
                                }
                                match runtime.run_program(&program) {
                                    Ok(value) => {
                                        if should_echo
                                            && !matches!(value, crate::runtime::value::Value::Nil)
                                        {
                                            println!("{}", value);
                                        }
                                        if show_timing {
                                            println!(
                                                "time: {:.3} ms",
                                                started.elapsed().as_secs_f64() * 1000.0
                                            );
                                        }
                                    }
                                    Err(err) => eprintln!("{}", err),
                                }
                            }
                            Err(errors) => {
                                for rendered in
                                    crate::errors::pretty::format_type_errors("<repl>", &errors)
                                {
                                    eprintln!("{}", rendered);
                                }
                            }
                        },
                        Err(err) => eprintln!(
                            "{}",
                            crate::errors::pretty::format_parse_error("<repl>", input.as_str(), &err)
                        ),
                    }
                }
                Err(err) => eprintln!(
                    "lex error [LEX0001]: {}\n--> <repl>:{}:{}\ndocs: https://scalf-lang.dev/errors/LEX0001",
                    err.message, err.line, err.column
                ),
            }
        }
    } else {
        let stdin = io::stdin();
        let mut line = String::new();
        loop {
            let prompt = if multiline_buffer.is_empty() {
                "scalf> "
            } else {
                "....> "
            };
            print!("{}", prompt);
            io::stdout().flush()?;

            line.clear();
            if stdin.read_line(&mut line)? == 0 {
                break;
            }

            let raw_line = line.trim_end_matches(['\n', '\r']);
            if multiline_buffer.is_empty() {
                match handle_meta_command(
                    raw_line.trim(),
                    &mut history,
                    &mut show_timing,
                    &checker,
                    &docs,
                )? {
                    CommandAction::NotHandled => {}
                    CommandAction::Handled => continue,
                    CommandAction::Exit => break,
                }
            }

            let normalized = highlighter::normalize_line(raw_line);
            if multiline_buffer.is_empty() && normalized.trim().is_empty() {
                continue;
            }

            if !multiline_buffer.is_empty() {
                multiline_buffer.push('\n');
            }
            multiline_buffer.push_str(&normalized);

            if highlighter::needs_more_input(&multiline_buffer) {
                continue;
            }

            let input = std::mem::take(&mut multiline_buffer);
            if !input.trim().is_empty() {
                history.push(input.clone());
                trim_history(&mut history);
            }

            let started = Instant::now();
            match crate::lexer::lex(input.as_str()) {
                Ok(tokens) => {
                    let mut parser = crate::parser::Parser::new(tokens);
                    match parser.parse_program() {
                        Ok(program) => match checker.check_program(&program) {
                            Ok(_) => {
                                let should_echo = should_echo_repl_result(&program);
                                match runtime.run_program(&program) {
                                Ok(value) => {
                                    if should_echo
                                        && !matches!(value, crate::runtime::value::Value::Nil)
                                    {
                                        println!("{}", value);
                                    }
                                    if show_timing {
                                        println!(
                                            "time: {:.3} ms",
                                            started.elapsed().as_secs_f64() * 1000.0
                                        );
                                    }
                                }
                                Err(err) => eprintln!("{}", err),
                            }
                            },
                            Err(errors) => {
                                for rendered in
                                    crate::errors::pretty::format_type_errors("<repl>", &errors)
                                {
                                    eprintln!("{}", rendered);
                                }
                            }
                        },
                        Err(err) => eprintln!(
                            "{}",
                            crate::errors::pretty::format_parse_error("<repl>", input.as_str(), &err)
                        ),
                    }
                }
                Err(err) => eprintln!(
                    "lex error [LEX0001]: {}\n--> <repl>:{}:{}\ndocs: https://scalf-lang.dev/errors/LEX0001",
                    err.message, err.line, err.column
                ),
            }
        }
    }

    if let Err(err) = save_history(&history) {
        eprintln!("failed to save repl history: {}", err);
    }

    Ok(())
}

fn handle_meta_command(
    command: &str,
    history: &mut Vec<String>,
    show_timing: &mut bool,
    checker: &crate::typechecker::TypeChecker,
    docs: &HashMap<&'static str, &'static str>,
) -> Result<CommandAction, Box<dyn Error>> {
    if command.is_empty() {
        return Ok(CommandAction::NotHandled);
    }
    if command.eq_ignore_ascii_case("exit") || command.eq_ignore_ascii_case("quit") {
        return Ok(CommandAction::Exit);
    }
    if !command.starts_with(':') {
        return Ok(CommandAction::NotHandled);
    }

    let mut parts = command.split_whitespace();
    let directive = parts.next().unwrap_or_default();
    match directive {
        ":quit" | ":exit" => return Ok(CommandAction::Exit),
        ":help" => {
            println!(":help                   Show this message");
            println!(":doc <symbol>           Show inline docs");
            println!(":complete <prefix>      Show completion suggestions");
            println!(":history [n]            Show recent history (default 20)");
            println!(":timing on|off          Toggle evaluation timing output");
            println!(":highlight <code>       Render ANSI syntax highlighting preview");
            println!(":quit                   Exit REPL");
        }
        ":doc" => {
            let Some(symbol) = parts.next() else {
                eprintln!("usage: :doc <symbol>");
                return Ok(CommandAction::Handled);
            };
            if let Some(text) = docs.get(symbol) {
                println!("{}", text);
            } else {
                eprintln!("no docs available for '{}'", symbol);
            }
        }
        ":complete" => {
            let Some(prefix) = parts.next() else {
                eprintln!("usage: :complete <prefix>");
                return Ok(CommandAction::Handled);
            };
            let symbols = checker.inferred_types().keys().cloned().collect::<Vec<_>>();
            let suggestions = highlighter::complete(prefix, &symbols);
            if suggestions.is_empty() {
                println!("(no matches)");
            } else {
                for suggestion in suggestions {
                    println!("{}", suggestion);
                }
            }
        }
        ":history" => {
            let count = parts
                .next()
                .and_then(|value| value.parse::<usize>().ok())
                .unwrap_or(20);
            let start = history.len().saturating_sub(count);
            for (idx, entry) in history.iter().enumerate().skip(start) {
                println!("{:>4} {}", idx + 1, highlighter::colorize(entry));
            }
        }
        ":timing" => {
            let Some(mode) = parts.next() else {
                eprintln!("usage: :timing on|off");
                return Ok(CommandAction::Handled);
            };
            match mode {
                "on" => {
                    *show_timing = true;
                    println!("timing: on");
                }
                "off" => {
                    *show_timing = false;
                    println!("timing: off");
                }
                _ => eprintln!("usage: :timing on|off"),
            }
        }
        ":highlight" => {
            let code = command.strip_prefix(":highlight").unwrap_or("").trim();
            if code.is_empty() {
                eprintln!("usage: :highlight <code>");
            } else {
                println!("{}", highlighter::colorize(code));
            }
        }
        other => {
            eprintln!("unknown command '{}'; use :help", other);
        }
    }

    Ok(CommandAction::Handled)
}

fn repl_docs() -> HashMap<&'static str, &'static str> {
    HashMap::from([
        ("math", "std.math: pi, e, min, max, abs, round"),
        (
            "fs",
            "std.fs: read(path), write(path, content), exists(path), delete(path)",
        ),
        ("json", "std.json: parse(string), stringify(value, pretty?)"),
        (
            "path",
            "std.path: join, normalize, basename, dirname, cwd, to_string",
        ),
        ("env", "std.env: get(key)"),
        (
            "http",
            "std.http: get/post/put/delete(url, body?, headers?, timeout_ms?)",
        ),
        ("time", "std.time: now_ms(), now_s(), sleep(ms)"),
        ("crypto", "std.crypto: sha256(text)"),
        (
            "concurrency",
            "std.concurrency: await(value), join(list), timeout(ms, value), channel()",
        ),
        (
            "channel",
            "channel methods: send(value), recv(), try_recv(), recv_timeout(ms), len()",
        ),
        ("len", "core len(value): works for string/path/list/map"),
        ("list", "core list(...): build list from arguments"),
        (
            "map",
            "core map(k1, v1, ...): build map from key/value pairs",
        ),
    ])
}

fn load_history() -> Vec<String> {
    let path = repl_history_path();
    let Ok(contents) = fs::read_to_string(path) else {
        return Vec::new();
    };
    contents
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .map(ToString::to_string)
        .collect()
}

fn save_history(history: &[String]) -> io::Result<()> {
    let path = repl_history_path();
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let mut trimmed = history.to_vec();
    trim_history(&mut trimmed);
    let mut encoded = trimmed.join("\n");
    if !encoded.is_empty() {
        encoded.push('\n');
    }
    fs::write(path, encoded)
}

fn trim_history(history: &mut Vec<String>) {
    if history.len() > MAX_HISTORY_ENTRIES {
        let drop_count = history.len() - MAX_HISTORY_ENTRIES;
        history.drain(0..drop_count);
    }
}

fn repl_history_path() -> PathBuf {
    if let Ok(path) = std::env::var("scalf_REPL_HISTORY") {
        return PathBuf::from(path);
    }
    if let Ok(home) = std::env::var("HOME") {
        return PathBuf::from(home).join(".scl").join("repl_history");
    }
    if let Ok(home) = std::env::var("USERPROFILE") {
        return PathBuf::from(home).join(".scl").join("repl_history");
    }
    PathBuf::from(".scl_repl_history")
}

fn should_echo_repl_result(program: &crate::parser::ast::Program) -> bool {
    if program.statements.len() != 1 {
        return false;
    }
    let stmt = &program.statements[0];
    match stmt {
        crate::parser::ast::Stmt::Expr(expr) => is_echoable_expr(expr),
        _ => false,
    }
}

fn is_echoable_expr(expr: &crate::parser::ast::Expr) -> bool {
    use crate::parser::ast::Expr;

    match expr {
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::String { .. }
        | Expr::Bool(_)
        | Expr::Nil
        | Expr::Variable(_) => true,
        Expr::Unary { rhs, .. } | Expr::Grouping(rhs) | Expr::PanicUnwrap(rhs) => {
            is_echoable_expr(rhs)
        }
        Expr::Binary { lhs, rhs, .. } | Expr::Coalesce { lhs, rhs } => {
            is_echoable_expr(lhs) && is_echoable_expr(rhs)
        }
        Expr::Member { object, .. } => is_echoable_expr(object),
        Expr::Index { object, index } => is_echoable_expr(object) && is_echoable_expr(index),
        Expr::ListLiteral(items) => items.iter().all(is_echoable_expr),
        Expr::MapLiteral(entries) => entries.iter().all(|entry| is_echoable_expr(&entry.value)),
        Expr::Match { subject, arms } => {
            is_echoable_expr(subject) && arms.iter().all(|arm| is_echoable_expr(&arm.value))
        }
        Expr::Call { .. }
        | Expr::Assign { .. }
        | Expr::OrReturn { .. }
        | Expr::ListComprehension { .. }
        | Expr::Function { .. } => false,
    }
}

#[cfg(test)]
mod tests {
    use super::should_echo_repl_result;

    fn parse_program(source: &str) -> crate::parser::ast::Program {
        let tokens = crate::lexer::lex(source).expect("lex should succeed");
        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse_program().expect("parse should succeed")
    }

    #[test]
    fn repl_echoes_simple_expression_only() {
        assert!(should_echo_repl_result(&parse_program("name")));
        assert!(should_echo_repl_result(&parse_program("1 + 2")));
        assert!(!should_echo_repl_result(&parse_program("name = \"scalf\"")));
        assert!(!should_echo_repl_result(&parse_program("print(\"scalf\")")));
        assert!(!should_echo_repl_result(&parse_program("len([1, 2, 3])")));
    }
}
