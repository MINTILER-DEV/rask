use std::time::{SystemTime, UNIX_EPOCH};

use rask::runtime::value::Value;

fn run(source: &str) -> Value {
    run_with_permissions(source, rask::runtime::Permissions::allow_all())
}

fn run_with_permissions(source: &str, permissions: rask::runtime::Permissions) -> Value {
    let tokens = rask::lexer::lex(source).expect("lex should succeed");
    let mut parser = rask::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut runtime = rask::runtime::Runtime::with_permissions(permissions);
    runtime.run_program(&program).expect("runtime should succeed")
}

#[test]
fn evaluates_string_and_list_methods() {
    let value = run(
        "def double(x: int) -> int { return x * 2 }\n\
         nums = [1, 2]\n\
         nums.push(3)\n\
         upper = \"  hi \".trim().uppercase()\n\
         mapped = nums.map(double)\n\
         mapped[2]",
    );
    assert_eq!(value, Value::Int(6));
}

#[test]
fn evaluates_map_methods_and_indexing() {
    let value = run(
        "m = {name: \"rask\"}\n\
         m.set(\"version\", 1)\n\
         m[\"version\"]",
    );
    assert_eq!(value, Value::Int(1));
}

#[test]
fn evaluates_json_roundtrip() {
    let value = run(
        "data = {name: \"rask\", nums: [1, 2, 3]}\n\
         encoded = json.stringify(data)\n\
         decoded = json.parse(encoded)\n\
         decoded[\"name\"]",
    );
    assert_eq!(value, Value::String("rask".to_string()));
}

#[test]
fn evaluates_use_alias_for_module() {
    let value = run("use std.math as m\nm.max(2, 5)");
    assert_eq!(value, Value::Float(5.0));
}

#[test]
fn evaluates_fs_module() {
    let mut path = std::env::temp_dir();
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be valid")
        .as_nanos();
    path.push(format!("rask_phase3_{}.txt", stamp));
    let path_str = path.to_string_lossy().replace('\\', "\\\\");

    let script = format!(
        "fs.write(\"{}\", \"hello\")\n\
         content = fs.read(\"{}\")\n\
         fs.delete(\"{}\")\n\
         content",
        path_str, path_str, path_str
    );

    let value = run(&script);
    assert_eq!(value, Value::String("hello".to_string()));
    assert!(!path.exists(), "temporary file should be deleted");
}

#[test]
fn denies_fs_without_permissions() {
    let tokens = rask::lexer::lex("fs.exists(\"/tmp/not-used\")").expect("lex should succeed");
    let mut parser = rask::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut runtime = rask::runtime::Runtime::new();
    let result = runtime.run_program(&program);
    assert!(result.is_err());
}

#[test]
fn evaluates_list_comprehension_and_graphemes() {
    let value = run(
        "chars = [c for c in \"👍🏽ok\" if c != \"o\"]\n\
         len(chars)",
    );
    assert_eq!(value, Value::Int(2));
}

#[test]
fn evaluates_path_module() {
    let value = run(
        "p = path.join(path.cwd(), \"stdlib\", \"std\", \"math.rask\")\n\
         path.basename(p)",
    );
    assert_eq!(value, Value::String("math.rask".to_string()));
}

#[test]
fn env_get_requires_permission() {
    std::env::set_var("RASK_TEST_ENV_KEY", "ok");
    let tokens =
        rask::lexer::lex("env.get(\"RASK_TEST_ENV_KEY\")").expect("lex should succeed");
    let mut parser = rask::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut restricted = rask::runtime::Runtime::new();
    assert!(restricted.run_program(&program).is_err());

    let mut permissions = rask::runtime::Permissions::default();
    permissions.allow_env = true;
    let mut enabled = rask::runtime::Runtime::with_permissions(permissions);
    let value = enabled.run_program(&program).expect("runtime should succeed");
    assert_eq!(value, Value::String("ok".to_string()));
}
