use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();

    if let Some(result) = maybe_run_docs_command(&args) {
        if let Err(err) = result {
            eprintln!("{}", err);
        }
        return;
    }

    match parse_cli(args) {
        Ok((script_path, permissions)) => {
            if let Some(path) = script_path {
                match fs::read_to_string(&path) {
                    Ok(source) => run_source(&source, permissions),
                    Err(err) => eprintln!("failed to read '{}': {}", path, err),
                }
            } else if let Err(err) = rask::repl::run_with_permissions(permissions) {
                eprintln!("repl error: {}", err);
            }
        }
        Err(err) => eprintln!("{}", err),
    }
}

fn maybe_run_docs_command(args: &[String]) -> Option<Result<(), String>> {
    if args.first().map(|arg| arg.as_str()) != Some("docs") {
        return None;
    }

    let mut output = PathBuf::from("docs/stdlib_reference.md");
    for arg in args.iter().skip(1) {
        if let Some(value) = arg.strip_prefix("--out=") {
            output = PathBuf::from(value);
            continue;
        }
        return Some(Err(format!(
            "unknown docs option '{}'; supported: --out=<path>",
            arg
        )));
    }

    Some(
        rask::docgen::generate_stdlib_reference(&output)
            .map(|report| {
                println!(
                    "generated stdlib docs: {} ({} module files)",
                    report.output_path.display(),
                    report.module_count
                );
            })
            .map_err(|err| err.to_string()),
    )
}

fn parse_cli(args: Vec<String>) -> Result<(Option<String>, rask::runtime::Permissions), String> {
    let mut script_path: Option<String> = None;
    let mut permissions = rask::runtime::Permissions::default();

    for arg in args {
        if let Some(value) = arg.strip_prefix("--allow-read=") {
            for path in split_csv(value) {
                permissions.allow_read.push(normalize_cli_path(path));
            }
            continue;
        }

        if let Some(value) = arg.strip_prefix("--allow-write=") {
            for path in split_csv(value) {
                permissions.allow_write.push(normalize_cli_path(path));
            }
            continue;
        }

        if let Some(value) = arg.strip_prefix("--allow-net=") {
            permissions.allow_net.extend(split_csv(value));
            continue;
        }

        if arg == "--allow-env" {
            permissions.allow_env = true;
            continue;
        }

        if arg == "--allow-all" {
            permissions = rask::runtime::Permissions::allow_all();
            continue;
        }

        if arg.starts_with("--") {
            return Err(format!("unknown flag '{}'", arg));
        }

        if script_path.is_none() {
            script_path = Some(arg);
        } else {
            return Err("multiple script paths provided".to_string());
        }
    }

    Ok((script_path, permissions))
}

fn split_csv(value: &str) -> Vec<String> {
    value
        .split(',')
        .map(str::trim)
        .filter(|part| !part.is_empty())
        .map(ToString::to_string)
        .collect()
}

fn normalize_cli_path(path: String) -> PathBuf {
    let candidate = PathBuf::from(&path);
    if candidate.is_absolute() {
        normalize_pathbuf(candidate)
    } else {
        let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        normalize_pathbuf(cwd.join(candidate))
    }
}

fn normalize_pathbuf(path: PathBuf) -> PathBuf {
    let mut normalized = PathBuf::new();
    for component in path.components() {
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

fn run_source(source: &str, permissions: rask::runtime::Permissions) {
    match rask::lexer::lex(source) {
        Ok(tokens) => {
            let mut parser = rask::parser::Parser::new(tokens);
            match parser.parse_program() {
                Ok(program) => {
                    let mut checker = rask::typechecker::TypeChecker::new();
                    match checker.check_program(&program) {
                        Ok(_) => {
                            let mut runtime = rask::runtime::Runtime::with_permissions(permissions);
                            match runtime.run_program(&program) {
                                Ok(value) => {
                                    if !matches!(value, rask::runtime::value::Value::Nil) {
                                        println!("{}", value);
                                    }
                                }
                                Err(err) => eprintln!("{}", err),
                            }
                        }
                        Err(errors) => {
                            for error in errors {
                                eprintln!("{}", error);
                            }
                        }
                    }
                }
                Err(err) => eprintln!("{}", err),
            }
        }
        Err(err) => eprintln!("{}", err),
    }
}
