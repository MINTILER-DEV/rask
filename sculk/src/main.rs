use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use scalf::runtime::value::Value as RuntimeValue;
use sculk::backend::cranelift::CraneliftBackend;
use sculk::backend::Backend;
use sculk::Compiler;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionMode {
    Runtime,
    Native,
}

fn main() {
    if let Err(err) = run() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let args = env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        return Err(usage());
    }

    let mut script_path: Option<String> = None;
    let mut emit_ir = false;
    let mut run_main = true;
    let mut emit_obj: Option<PathBuf> = None;
    let mut emit_exe: Option<Option<PathBuf>> = None;
    let mut execution_mode = ExecutionMode::Runtime;
    let mut opt_level: u8 = 0;

    let mut index = 0;
    while index < args.len() {
        let arg = &args[index];
        match arg.as_str() {
            "--emit-ir" => {
                emit_ir = true;
            }
            "--no-run" => {
                run_main = false;
            }
            "--run" => {
                run_main = true;
            }
            "--native" => {
                execution_mode = ExecutionMode::Native;
            }
            "--runtime" => {
                execution_mode = ExecutionMode::Runtime;
            }
            "--emit-obj" => {
                index += 1;
                let Some(path) = args.get(index) else {
                    return Err("--emit-obj requires a path".to_string());
                };
                emit_obj = Some(PathBuf::from(path));
            }
            "--emit-exe" => {
                if let Some(next) = args.get(index + 1) {
                    if !next.starts_with("--") {
                        index += 1;
                        emit_exe = Some(Some(PathBuf::from(next)));
                    } else {
                        emit_exe = Some(None);
                    }
                } else {
                    emit_exe = Some(None);
                }
            }
            "--out" => {
                index += 1;
                let Some(path) = args.get(index) else {
                    return Err("--out requires a path".to_string());
                };
                emit_exe = Some(Some(PathBuf::from(path)));
                run_main = false;
            }
            "--opt-level" => {
                index += 1;
                let Some(value) = args.get(index) else {
                    return Err("--opt-level requires a value between 0 and 3".to_string());
                };
                opt_level = parse_opt_level(value)?;
            }
            "-O0" => {
                opt_level = 0;
            }
            "-O1" => {
                opt_level = 1;
            }
            "-O2" => {
                opt_level = 2;
            }
            "-O3" => {
                opt_level = 3;
            }
            _ if arg.starts_with("--emit-obj=") => {
                let Some(path) = arg.strip_prefix("--emit-obj=") else {
                    unreachable!();
                };
                emit_obj = Some(PathBuf::from(path));
            }
            _ if arg.starts_with("--emit-exe=") => {
                let Some(path) = arg.strip_prefix("--emit-exe=") else {
                    unreachable!();
                };
                emit_exe = Some(Some(PathBuf::from(path)));
            }
            _ if arg.starts_with("--out=") => {
                let Some(path) = arg.strip_prefix("--out=") else {
                    unreachable!();
                };
                emit_exe = Some(Some(PathBuf::from(path)));
                run_main = false;
            }
            _ if arg.starts_with("--opt-level=") => {
                let Some(value) = arg.strip_prefix("--opt-level=") else {
                    unreachable!();
                };
                opt_level = parse_opt_level(value)?;
            }
            _ if arg.starts_with("--") => {
                return Err(format!("unknown option '{}'", arg));
            }
            _ => {
                if script_path.is_none() {
                    script_path = Some(arg.clone());
                } else {
                    return Err("multiple script paths provided".to_string());
                }
            }
        }
        index += 1;
    }

    let Some(script_path) = script_path else {
        return Err(usage());
    };

    let script_path_buf = PathBuf::from(&script_path);
    let compiler = Compiler::new().with_opt_level(opt_level);

    let need_module = emit_ir
        || emit_obj.is_some()
        || (run_main && matches!(execution_mode, ExecutionMode::Native));

    let mut module = None;
    if need_module {
        let compiled = compiler
            .compile_file(&script_path_buf)
            .map_err(|err| format!("compile failed: {}", err))?;
        module = Some(compiled);
    }

    if emit_ir {
        let Some(module) = module.as_ref() else {
            return Err("internal error: missing compiled module for --emit-ir".to_string());
        };
        println!("{}", module);
    }

    let need_backend =
        emit_obj.is_some() || (run_main && matches!(execution_mode, ExecutionMode::Native));

    let backend = if need_backend {
        Some(CraneliftBackend::new().map_err(|err| err.to_string())?)
    } else {
        None
    };

    if let Some(path) = emit_obj {
        let Some(module) = module.as_ref() else {
            return Err("internal error: missing compiled module for --emit-obj".to_string());
        };
        let object_bytes = backend
            .as_ref()
            .expect("backend is initialized")
            .generate(module)
            .map_err(|err| format!("object generation failed: {}", err))?;
        write_output_file(&path, &object_bytes)?;
        println!("wrote object file {}", path.display());
    }

    if let Some(exe_path_option) = emit_exe {
        let exe_path = exe_path_option.unwrap_or_else(|| default_exe_output_path(&script_path));
        emit_runtime_semantics_exe(&script_path_buf, &script_path, &exe_path)?;
        println!(
            "wrote executable {}",
            ensure_exe_extension(&exe_path).display()
        );
    }

    if run_main {
        match execution_mode {
            ExecutionMode::Runtime => run_with_full_runtime_semantics(&script_path_buf)?,
            ExecutionMode::Native => {
                let Some(module) = module.as_ref() else {
                    return Err(
                        "internal error: missing compiled module for native run".to_string()
                    );
                };
                let exit_code = backend
                    .as_ref()
                    .expect("backend is initialized")
                    .run_main(module)
                    .map_err(|err| format!("jit execution failed: {}", err))?;
                println!("program exited with code {}", exit_code);
            }
        }
    }

    Ok(())
}

fn run_with_full_runtime_semantics(script_path: &Path) -> Result<(), String> {
    let source = fs::read_to_string(script_path).map_err(|err| {
        format!(
            "runtime preparation failed: failed to read '{}': {}",
            script_path.display(),
            err
        )
    })?;
    let source_label = script_path.display().to_string();

    let tokens = scalf::lexer::lex(&source).map_err(|err| {
        format!(
            "lex error [LEX0001]: {}\n--> {}:{}:{}\ndocs: https://scalf-lang.dev/errors/LEX0001",
            err.message, source_label, err.line, err.column
        )
    })?;
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser
        .parse_program()
        .map_err(|err| scalf::errors::pretty::format_parse_error(&source_label, &source, &err))?;

    let mut checker = scalf::typechecker::TypeChecker::new();
    checker.check_program(&program).map_err(|errors| {
        scalf::errors::pretty::format_type_errors(&source_label, &errors).join("\n\n")
    })?;

    let mut runtime =
        scalf::runtime::Runtime::with_permissions(scalf::runtime::Permissions::allow_all())
            .with_source_label(source_label);
    let value = runtime
        .run_program(&program)
        .map_err(|err| err.to_string())?;

    println!("program exited with code {}", exit_code_from_value(&value));
    Ok(())
}

fn emit_runtime_semantics_exe(
    script_path: &Path,
    script_label: &str,
    exe_path: &Path,
) -> Result<(), String> {
    if !cfg!(windows) {
        return Err("--emit-exe is currently supported only on Windows targets".to_string());
    }

    let source = fs::read_to_string(script_path).map_err(|err| {
        format!(
            "failed to read source script '{}': {}",
            script_path.display(),
            err
        )
    })?;

    let exe_path = ensure_exe_extension(exe_path);
    if let Some(parent) = exe_path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|err| {
                format!(
                    "failed to create output directory '{}': {}",
                    parent.display(),
                    err
                )
            })?;
        }
    }

    let temp_dir = env::temp_dir().join(format!(
        "sculk-emit-runtime-exe-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|err| format!("system clock error: {}", err))?
            .as_nanos()
    ));
    let src_dir = temp_dir.join("src");
    fs::create_dir_all(&src_dir).map_err(|err| {
        format!(
            "failed to create temporary build directory '{}': {}",
            src_dir.display(),
            err
        )
    })?;

    let sculk_manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let scalf_root = sculk_manifest_dir
        .parent()
        .ok_or_else(|| "failed to locate scalf workspace root".to_string())?;
    let scalf_dep_path = scalf_root.display().to_string().replace('\\', "/");

    let cargo_toml = format!(
        "[package]\nname = \"sculk_embedded_runner\"\nversion = \"0.0.0\"\nedition = \"2021\"\n\n[dependencies]\nscalf = {{ path = \"{}\" }}\n\n[profile.release]\nopt-level = \"z\"\nlto = true\ncodegen-units = 1\npanic = \"abort\"\nstrip = \"symbols\"\n",
        scalf_dep_path
    );
    let launcher_source = generate_runtime_launcher_source(&source, script_label);

    fs::write(temp_dir.join("Cargo.toml"), cargo_toml).map_err(|err| {
        format!(
            "failed to write temporary Cargo.toml '{}': {}",
            temp_dir.join("Cargo.toml").display(),
            err
        )
    })?;
    fs::write(src_dir.join("main.rs"), launcher_source).map_err(|err| {
        format!(
            "failed to write temporary launcher '{}': {}",
            src_dir.join("main.rs").display(),
            err
        )
    })?;

    let build_target_dir = env::temp_dir().join("sculk-runtime-exe-cache");
    let output = Command::new("cargo")
        .arg("build")
        .arg("--release")
        .arg("--manifest-path")
        .arg(temp_dir.join("Cargo.toml"))
        .arg("--target-dir")
        .arg(&build_target_dir)
        .output()
        .map_err(|err| format!("failed to run cargo for emitted executable: {}", err))?;

    let _ = fs::remove_dir_all(&temp_dir);

    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "failed to build emitted executable\nstdout:\n{}\nstderr:\n{}",
            stdout, stderr
        ));
    }

    let built_exe = build_target_dir
        .join("release")
        .join("sculk_embedded_runner.exe");
    if !built_exe.exists() {
        return Err(format!(
            "build finished but expected executable was not found at '{}'",
            built_exe.display()
        ));
    }

    fs::copy(&built_exe, &exe_path).map_err(|err| {
        format!(
            "failed to copy emitted executable to '{}': {}",
            exe_path.display(),
            err
        )
    })?;

    Ok(())
}

fn generate_runtime_launcher_source(source: &str, source_label: &str) -> String {
    let source_lit = to_rust_raw_string_literal(source);
    let source_label_lit = to_rust_raw_string_literal(source_label);

    format!(
        "use scalf::runtime::value::Value;\n\nfn main() {{\n    if let Err(err) = run_embedded() {{\n        eprintln!(\"{{}}\", err);\n        std::process::exit(1);\n    }}\n}}\n\nfn run_embedded() -> Result<(), String> {{\n    let source = {source_lit};\n    let source_label = {source_label_lit};\n\n    let tokens = scalf::lexer::lex(source).map_err(|err| {{\n        format!(\n            \"lex error [LEX0001]: {{}}\\n--> {{}}:{{}}:{{}}\\ndocs: https://scalf-lang.dev/errors/LEX0001\",\n            err.message, source_label, err.line, err.column\n        )\n    }})?;\n\n    let mut parser = scalf::parser::Parser::new(tokens);\n    let program = parser\n        .parse_program()\n        .map_err(|err| scalf::errors::pretty::format_parse_error(source_label, source, &err))?;\n\n    let mut checker = scalf::typechecker::TypeChecker::new();\n    checker.check_program(&program).map_err(|errors| {{\n        scalf::errors::pretty::format_type_errors(source_label, &errors).join(\"\\n\\n\")\n    }})?;\n\n    let mut runtime =\n        scalf::runtime::Runtime::with_permissions(scalf::runtime::Permissions::allow_all())\n            .with_source_label(source_label);\n    let value = runtime.run_program(&program).map_err(|err| err.to_string())?;\n\n    let exit_code = match value {{\n        Value::Int(code) => code,\n        _ => 0,\n    }};\n    std::process::exit(exit_code as i32);\n}}\n"
    )
}

fn to_rust_raw_string_literal(value: &str) -> String {
    for hash_count in 0..=16 {
        let hashes = "#".repeat(hash_count);
        let closing = format!("\"{}", hashes);
        if !value.contains(&closing) {
            return format!("r{hashes}\"{value}\"{hashes}");
        }
    }

    let escaped = value
        .replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
        .replace('\t', "\\t");
    format!("\"{}\"", escaped)
}

fn exit_code_from_value(value: &RuntimeValue) -> i64 {
    match value {
        RuntimeValue::Int(code) => *code,
        _ => 0,
    }
}

fn write_output_file(path: &Path, bytes: &[u8]) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|err| {
                format!(
                    "failed to create output directory '{}': {}",
                    parent.display(),
                    err
                )
            })?;
        }
    }

    fs::write(path, bytes)
        .map_err(|err| format!("failed to write output file '{}': {}", path.display(), err))
}

fn ensure_exe_extension(path: &Path) -> PathBuf {
    if path.extension().is_some() {
        path.to_path_buf()
    } else {
        let mut with_ext = path.to_path_buf();
        with_ext.set_extension("exe");
        with_ext
    }
}

fn default_exe_output_path(script_path: &str) -> PathBuf {
    let stem = PathBuf::from(script_path)
        .file_stem()
        .and_then(|value| value.to_str())
        .unwrap_or("app")
        .to_string();
    PathBuf::from(format!("{}.exe", stem))
}

fn parse_opt_level(value: &str) -> Result<u8, String> {
    let parsed = value
        .parse::<u8>()
        .map_err(|_| format!("invalid optimization level '{}'; expected 0..=3", value))?;
    if parsed > 3 {
        return Err(format!(
            "invalid optimization level '{}'; expected 0..=3",
            value
        ));
    }
    Ok(parsed)
}
fn usage() -> String {
    "usage: sculk <file.scl> [--runtime|--native] [--emit-ir] [--emit-obj <path>] [--emit-exe[=<path>]] [--out <path>] [--run|--no-run] [--opt-level <0-3>|-O0|-O1|-O2|-O3]".to_string()
}
