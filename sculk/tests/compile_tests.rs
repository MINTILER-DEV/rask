//! Integration tests for Sculk compiler

use sculk::*;

#[test]
fn test_compiler_creation() {
    let compiler = Compiler::new();
    assert_eq!(compiler.opt_level, 0);
}

#[test]
fn test_opt_level_enables_constant_folding() {
    let source = r#"
def main() {
    return 2 + 3
}
"#;

    let compiler = Compiler::new().with_opt_level(1);
    let module = compiler
        .compile_source(source, "folded")
        .expect("source should compile with optimization");

    let main = module
        .functions
        .iter()
        .find(|function| function.name == "main")
        .expect("module should contain main");
    let entry = main
        .blocks
        .first()
        .expect("main should have an entry block");

    assert!(
        !entry
            .instructions
            .iter()
            .any(|instruction| matches!(instruction, ir::Instruction::BinOp { .. })),
        "expected constant folding to eliminate BinOp"
    );
    assert!(matches!(
        entry.terminator,
        ir::Terminator::Return(Some(ir::Value::Int(5)))
    ));
}
#[test]
fn test_ir_module_creation() {
    let module = ir::Module::new("test".to_string());
    assert_eq!(module.name, "test");
    assert_eq!(module.functions.len(), 0);
}

#[test]
fn test_ir_function_creation() {
    let func = ir::Function::new("main".to_string(), vec![], ir::Type::Int);
    assert_eq!(func.name, "main");
    assert_eq!(func.return_type, ir::Type::Int);
}

#[test]
fn test_ir_builder() {
    use ir::builder::*;

    let mut builder = ModuleBuilder::new("test".to_string());

    let func = ir::Function::new(
        "add".to_string(),
        vec![
            ir::Parameter {
                name: "a".to_string(),
                ty: ir::Type::Int,
            },
            ir::Parameter {
                name: "b".to_string(),
                ty: ir::Type::Int,
            },
        ],
        ir::Type::Int,
    );

    builder.add_function(func);
    let module = builder.build();

    assert_eq!(module.functions.len(), 1);
    assert_eq!(module.functions[0].name, "add");
}

#[cfg(feature = "cranelift-backend")]
#[test]
fn test_cranelift_backend() {
    use backend::cranelift::CraneliftBackend;
    use backend::Backend;

    let backend = CraneliftBackend::new();
    assert!(backend.is_ok());

    let backend = backend.unwrap();
    assert_eq!(backend.name(), "cranelift");
}

#[cfg(feature = "cranelift-backend")]
#[test]
fn test_compile_source_and_run_main() {
    use backend::cranelift::CraneliftBackend;

    let source = r#"
def main() {
    print("Hello from native SCALF!")
    return 0
}
"#;

    let compiler = Compiler::new();
    let module = compiler
        .compile_source(source, "hello_native")
        .expect("source should lower to IR");

    let backend = CraneliftBackend::new().expect("backend should initialize");
    let exit_code = backend
        .run_main(&module)
        .expect("jit execution should succeed");

    assert_eq!(exit_code, 0);
}

#[cfg(feature = "cranelift-backend")]
#[test]
fn test_compile_file_with_local_use_import() {
    use backend::cranelift::CraneliftBackend;

    let unique = format!(
        "sculk_use_test_{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock should be valid")
            .as_nanos()
    );
    let dir = std::env::temp_dir().join(unique);
    std::fs::create_dir_all(&dir).expect("temp dir should be creatable");

    let helper_path = dir.join("helper.scl");
    std::fs::write(
        &helper_path,
        "def forty() {\n    return 40\n}\n\ndef two() {\n    return 2\n}\n",
    )
    .expect("helper module should be writable");

    let main_path = dir.join("main.scl");
    std::fs::write(
        &main_path,
        "use \"helper.scl\"\n\ndef main() {\n    total = forty() + two()\n    return total\n}\n",
    )
    .expect("main script should be writable");

    let compiler = Compiler::new();
    let module = compiler
        .compile_file(&main_path)
        .expect("compile_file should resolve local use imports");

    let backend = CraneliftBackend::new().expect("backend should initialize");
    let exit_code = backend
        .run_main(&module)
        .expect("jit execution should succeed");

    assert_eq!(exit_code, 42);

    let _ = std::fs::remove_dir_all(&dir);
}

#[cfg(feature = "cranelift-backend")]
#[test]
fn test_use_alias_local_import_compiles_and_runs() {
    use backend::cranelift::CraneliftBackend;

    let unique = format!(
        "sculk_use_alias_test_{}_{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock should be valid")
            .as_nanos()
    );
    let dir = std::env::temp_dir().join(unique);
    std::fs::create_dir_all(&dir).expect("temp dir should be creatable");

    let helper_path = dir.join("helper.scl");
    std::fs::write(&helper_path, "def helper() {\n    return 1\n}\n")
        .expect("helper module should be writable");

    let main_path = dir.join("main.scl");
    std::fs::write(
        &main_path,
        "use \"helper.scl\" as helper\n\ndef main() {\n    return helper()\n}\n",
    )
    .expect("main script should be writable");

    let compiler = Compiler::new();
    let module = compiler
        .compile_file(&main_path)
        .expect("use alias local import should compile");

    let backend = CraneliftBackend::new().expect("backend should initialize");
    let exit_code = backend
        .run_main(&module)
        .expect("jit execution should succeed");

    assert_eq!(exit_code, 1);

    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn test_compile_std_http_module_path_use() {
    let source = r#"
use std.http

response = http.get("https://example.com")
print(response.status)
"#;

    let compiler = Compiler::new();
    let module = compiler
        .compile_source(source, "http_client")
        .expect("std.http module-path use should lower in sculk");

    assert!(
        module.functions.iter().any(|func| func.name == "main"),
        "expected script main function"
    );
}

#[cfg(feature = "cranelift-backend")]
#[test]
fn test_function_parameters_are_lowered_and_executable() {
    use backend::cranelift::CraneliftBackend;

    let source = r#"
def add(a: int, b: int) -> int {
    return a + b
}

def main() {
    return add(19, 23)
}
"#;

    let compiler = Compiler::new();
    let module = compiler
        .compile_source(source, "params")
        .expect("parameterized functions should lower to IR");

    let backend = CraneliftBackend::new().expect("backend should initialize");
    let exit_code = backend
        .run_main(&module)
        .expect("jit execution should succeed");

    assert_eq!(exit_code, 42);
}
