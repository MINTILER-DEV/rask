use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use scalf::runtime::value::Value as RuntimeValue;
use sculk::backend::cranelift::CraneliftBackend;
use sculk::backend::Backend;
use sculk::ir::{Instruction, Module};
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
    let compiler = Compiler::new();

    let need_module = emit_ir
        || emit_obj.is_some()
        || emit_exe.is_some()
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

    let need_backend = emit_obj.is_some()
        || emit_exe.is_some()
        || (run_main && matches!(execution_mode, ExecutionMode::Native));

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
        let Some(module) = module.as_ref() else {
            return Err("internal error: missing compiled module for --emit-exe".to_string());
        };
        let exe_path = exe_path_option.unwrap_or_else(|| default_exe_output_path(&script_path));
        let (exe_module, entry_symbol) = prepare_exe_module(module)?;
        let object_bytes = backend
            .as_ref()
            .expect("backend is initialized")
            .generate(&exe_module)
            .map_err(|err| format!("object generation failed for exe: {}", err))?;
        link_windows_exe(&object_bytes, &exe_path, &entry_symbol)?;
        println!("wrote executable {}", exe_path.display());
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

fn exit_code_from_value(value: &RuntimeValue) -> i64 {
    match value {
        RuntimeValue::Int(code) => *code,
        _ => 0,
    }
}

fn prepare_exe_module(module: &Module) -> Result<(Module, String), String> {
    let mut rewritten = module.clone();
    let Some(main_index) = rewritten
        .functions
        .iter()
        .position(|func| func.name == "main")
    else {
        return Err("cannot emit exe: module does not define 'main'".to_string());
    };

    let entry_symbol = "__sculk_script_main".to_string();
    rewritten.functions[main_index].name = entry_symbol.clone();

    for function in &mut rewritten.functions {
        for block in &mut function.blocks {
            for instruction in &mut block.instructions {
                if let Instruction::Call { func, .. } = instruction {
                    if func == "main" {
                        *func = entry_symbol.clone();
                    }
                }
            }
        }
    }

    Ok((rewritten, entry_symbol))
}

fn link_windows_exe(
    object_bytes: &[u8],
    exe_path: &Path,
    entry_symbol: &str,
) -> Result<(), String> {
    if !cfg!(windows) {
        return Err("--emit-exe is currently supported only on Windows targets".to_string());
    }

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
        "sculk-link-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_err(|err| format!("system clock error: {}", err))?
            .as_nanos()
    ));
    fs::create_dir_all(&temp_dir).map_err(|err| {
        format!(
            "failed to create temporary link directory '{}': {}",
            temp_dir.display(),
            err
        )
    })?;

    let object_path = temp_dir.join("script.obj");
    fs::write(&object_path, object_bytes).map_err(|err| {
        format!(
            "failed to write temporary object '{}': {}",
            object_path.display(),
            err
        )
    })?;

    let launcher_path = temp_dir.join("launcher.rs");
    let launcher_source = generate_launcher_source(entry_symbol);
    fs::write(&launcher_path, launcher_source).map_err(|err| {
        format!(
            "failed to write temporary launcher '{}': {}",
            launcher_path.display(),
            err
        )
    })?;

    let rustc = env::var("RUSTC").unwrap_or_else(|_| "rustc".to_string());
    let output = Command::new(&rustc)
        .arg(&launcher_path)
        .arg("--edition=2021")
        .arg("-C")
        .arg("opt-level=2")
        .arg("-C")
        .arg(format!("link-arg={}", object_path.display()))
        .arg("-o")
        .arg(&exe_path)
        .output()
        .map_err(|err| format!("failed to run rustc linker step: {}", err))?;

    let _ = fs::remove_dir_all(&temp_dir);

    if !output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "failed to link executable with rustc\nstdout:\n{}\nstderr:\n{}",
            stdout, stderr
        ));
    }

    Ok(())
}

fn generate_launcher_source(entry_symbol: &str) -> String {
    format!(
        r#"use std::ffi::{{c_char, c_void, CStr}};
use std::process::Command;

#[derive(Debug)]
struct HttpResponseHandle {{
    status: i64,
}}

#[no_mangle]
pub extern "C" fn __sculk_print_cstr(ptr: *const c_char) {{
    if ptr.is_null() {{
        println!();
        return;
    }}

    let text = unsafe {{ CStr::from_ptr(ptr) }};
    println!("{{}}", text.to_string_lossy());
}}

#[no_mangle]
pub extern "C" fn __sculk_print_i64(value: i64) {{
    println!("{{}}", value);
}}

#[no_mangle]
pub extern "C" fn __sculk_print_f64(value: f64) {{
    println!("{{}}", value);
}}

#[no_mangle]
pub extern "C" fn __sculk_http_get(url: *const c_char) -> *mut c_void {{
    http_request_status("GET", url, std::ptr::null())
}}

#[no_mangle]
pub extern "C" fn __sculk_http_post(url: *const c_char, body: *const c_char) -> *mut c_void {{
    http_request_status("POST", url, body)
}}

#[no_mangle]
pub extern "C" fn __sculk_http_put(url: *const c_char, body: *const c_char) -> *mut c_void {{
    http_request_status("PUT", url, body)
}}

#[no_mangle]
pub extern "C" fn __sculk_http_delete(url: *const c_char) -> *mut c_void {{
    http_request_status("DELETE", url, std::ptr::null())
}}

#[no_mangle]
pub extern "C" fn __sculk_http_response_status(response: *mut c_void) -> i64 {{
    if response.is_null() {{
        return 0;
    }}

    let handle = unsafe {{ &*(response as *mut HttpResponseHandle) }};
    handle.status
}}

fn http_request_status(method: &str, url_ptr: *const c_char, body_ptr: *const c_char) -> *mut c_void {{
    let status = match run_curl_status(method, url_ptr, body_ptr) {{
        Ok(status) => status,
        Err(err) => {{
            eprintln!("sculk runtime error: {{}}", err);
            return std::ptr::null_mut();
        }}
    }};

    let handle = Box::new(HttpResponseHandle {{ status }});
    Box::into_raw(handle) as *mut c_void
}}

fn run_curl_status(method: &str, url_ptr: *const c_char, body_ptr: *const c_char) -> Result<i64, String> {{
    let url = parse_c_string(url_ptr, "http url")?;
    let body = if body_ptr.is_null() {{
        None
    }} else {{
        Some(parse_c_string(body_ptr, "http body")?)
    }};

    let mut command = Command::new("curl.exe");
    command
        .arg("-s")
        .arg("-o")
        .arg("NUL")
        .arg("-w")
        .arg("%{{http_code}}")
        .arg("-X")
        .arg(method)
        .arg(&url);

    if let Some(body) = body {{
        command.arg("--data").arg(body);
    }}

    let output = command
        .output()
        .map_err(|err| format!("failed to run curl.exe: {{}}", err))?;

    if !output.status.success() {{
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "curl failed for '{{}}' with status {{}}: {{}}",
            url,
            output.status,
            stderr.trim()
        ));
    }}

    let code = String::from_utf8_lossy(&output.stdout).trim().to_string();
    if code.is_empty() {{
        return Err(format!("curl returned no status code for '{{}}'", url));
    }}

    code.parse::<i64>()
        .map_err(|err| format!("invalid curl status code '{{}}' for '{{}}': {{}}", code, url, err))
}}

fn parse_c_string(ptr: *const c_char, label: &str) -> Result<String, String> {{
    if ptr.is_null() {{
        return Err(format!("{{}} pointer was null", label));
    }}

    let text = unsafe {{ CStr::from_ptr(ptr) }}
        .to_str()
        .map_err(|err| format!("{{}} was not valid UTF-8: {{}}", label, err))?;
    Ok(text.to_string())
}}

extern "C" {{
    #[link_name = "{entry_symbol}"]
    fn sculk_script_entry() -> i64;
}}

fn main() {{
    let code = unsafe {{ sculk_script_entry() }};
    std::process::exit(code as i32);
}}
"#
    )
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

fn usage() -> String {
    "usage: sculk <file.scl> [--runtime|--native] [--emit-ir] [--emit-obj <path>] [--emit-exe[=<path>]] [--run|--no-run]".to_string()
}
