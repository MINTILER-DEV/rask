use std::io::{Read, Write};
use std::net::TcpListener;
use std::path::PathBuf;
use std::sync::{mpsc, Mutex, MutexGuard, OnceLock};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use scalf::runtime::value::Value;

fn run(source: &str) -> Value {
    run_with_permissions(source, scalf::runtime::Permissions::allow_all())
}

fn run_with_permissions(source: &str, permissions: scalf::runtime::Permissions) -> Value {
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut runtime = scalf::runtime::Runtime::with_permissions(permissions);
    runtime
        .run_program(&program)
        .expect("runtime should succeed")
}

fn run_result(
    source: &str,
    permissions: scalf::runtime::Permissions,
) -> Result<Value, scalf::runtime::RuntimeError> {
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut runtime = scalf::runtime::Runtime::with_permissions(permissions);
    runtime.run_program(&program)
}

fn run_with_implicit_nil(source: &str) -> Value {
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut runtime =
        scalf::runtime::Runtime::with_permissions(scalf::runtime::Permissions::allow_all())
            .with_implicit_nil_for_unknown_variables(true);
    runtime
        .run_program(&program)
        .expect("runtime should succeed")
}

fn spawn_http_server(
    body: &str,
    content_type: &str,
    delay: Duration,
) -> (String, mpsc::Receiver<String>, thread::JoinHandle<()>) {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind server");
    let addr = listener.local_addr().expect("read local addr");
    let url = format!("http://{}", addr);
    let response_body = body.to_string();
    let response_content_type = content_type.to_string();
    let (tx, rx) = mpsc::channel();

    let handle = thread::spawn(move || {
        let (mut stream, _) = listener.accept().expect("accept client");
        stream
            .set_read_timeout(Some(Duration::from_millis(500)))
            .expect("set read timeout");

        let mut buffer = vec![0_u8; 8192];
        let bytes = stream.read(&mut buffer).expect("read request bytes");
        let request = String::from_utf8_lossy(&buffer[..bytes]).to_string();
        tx.send(request).expect("send request capture");

        if !delay.is_zero() {
            thread::sleep(delay);
        }

        let response = format!(
            "HTTP/1.1 200 OK\r\nContent-Type: {}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
            response_content_type,
            response_body.len(),
            response_body
        );
        stream
            .write_all(response.as_bytes())
            .expect("write response bytes");
    });

    (url, rx, handle)
}

fn test_env_lock() -> &'static Mutex<()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
}

struct ImportTestEnv {
    _guard: MutexGuard<'static, ()>,
    root: PathBuf,
    cache_dir: PathBuf,
    lockfile: PathBuf,
}

impl ImportTestEnv {
    fn setup() -> Self {
        let guard = test_env_lock()
            .lock()
            .expect("test environment lock should succeed");

        let mut root = std::env::temp_dir();
        let stamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be valid")
            .as_nanos();
        root.push(format!("scalf_phase5_test_{}", stamp));
        let cache_dir = root.join("cache");
        let lockfile = root.join(".scalf.lock");

        std::fs::create_dir_all(&cache_dir).expect("cache dir should be created");
        std::env::set_var("scalf_CACHE_DIR", &cache_dir);
        std::env::set_var("scalf_LOCKFILE", &lockfile);

        Self {
            _guard: guard,
            root,
            cache_dir,
            lockfile,
        }
    }
}

impl Drop for ImportTestEnv {
    fn drop(&mut self) {
        std::env::remove_var("scalf_CACHE_DIR");
        std::env::remove_var("scalf_LOCKFILE");
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

#[test]
fn evaluates_string_and_list_methods() {
    let value = run("def double(x: int) -> int { return x * 2 }\n\
         nums = [1, 2]\n\
         nums.push(3)\n\
         upper = \"  hi \".trim().uppercase()\n\
         mapped = nums.map(double)\n\
         mapped[2]");
    assert_eq!(value, Value::Int(6));
}

#[test]
fn evaluates_inline_function_expression() {
    let value = run("adder = def(x, y) { return x + y }\nadder(2, 3)");
    assert_eq!(value, Value::Int(5));
}

#[test]
fn implicit_nil_treats_unknown_variables_as_nil() {
    let value = run_with_implicit_nil("[missing or 5, missing?.name or \"none\"]");
    assert_eq!(
        value,
        Value::list(vec![Value::Int(5), Value::String("none".to_string())])
    );
}

#[test]
fn evaluates_string_interpolation() {
    let value = run("name = \"scalf\"\n\
         count = 2 + 2\n\
         \"Hello, {name}! {count}\"");
    assert_eq!(value, Value::String("Hello, scalf! 4".to_string()));
}

#[test]
fn keeps_escaped_braces_in_strings() {
    let value = run("\"literal: \\{name\\}\"");
    assert_eq!(value, Value::String("literal: {name}".to_string()));
}

#[test]
fn evaluates_map_methods_and_indexing() {
    let value = run("m = {name: \"scalf\"}\n\
         m.set(\"version\", 1)\n\
         m[\"version\"]");
    assert_eq!(value, Value::Int(1));
}

#[test]
fn evaluates_json_roundtrip() {
    let value = run("data = {name: \"scalf\", nums: [1, 2, 3]}\n\
         encoded = json.stringify(data)\n\
         decoded = json.parse(encoded)\n\
         decoded[\"name\"]");
    assert_eq!(value, Value::String("scalf".to_string()));
}

#[test]
fn evaluates_use_alias_for_module() {
    let value = run("use std.math as m\nm.max(2, 5)");
    assert_eq!(value, Value::Float(5.0));
}

#[test]
fn evaluates_local_string_import() {
    let mut module_path = std::env::temp_dir();
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be valid")
        .as_nanos();
    module_path.push(format!("scalf_local_import_{}.scl", stamp));
    std::fs::write(&module_path, "value = 123\n").expect("module file should be written");
    let escaped_path = module_path.to_string_lossy().replace('\\', "\\\\");

    let script = format!("use \"{}\" as local\nlocal.value", escaped_path);
    let value = run(&script);
    assert_eq!(value, Value::Int(123));

    let _ = std::fs::remove_file(module_path);
}

#[test]
fn evaluates_fs_module() {
    let mut path = std::env::temp_dir();
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be valid")
        .as_nanos();
    path.push(format!("scalf_phase3_{}.txt", stamp));
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
fn coalesce_recovers_from_fs_read_error() {
    let mut missing = std::env::temp_dir();
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be valid")
        .as_nanos();
    missing.push(format!("scalf_missing_{}.txt", stamp));
    let missing_str = missing.to_string_lossy().replace('\\', "\\\\");

    let script = format!("fs.read(\"{}\") or \"fallback\"", missing_str);
    let value = run(&script);
    assert_eq!(value, Value::String("fallback".to_string()));
}

#[test]
fn or_return_recovers_from_fs_read_error() {
    let mut missing = std::env::temp_dir();
    let stamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("clock should be valid")
        .as_nanos();
    missing.push(format!("scalf_missing_{}.txt", stamp));
    let missing_str = missing.to_string_lossy().replace('\\', "\\\\");

    let script = format!(
        "def load() -> List<string> {{\n\
         content = fs.read(\"{}\") or return []\n\
         return [content]\n\
         }}\n\
         load()",
        missing_str
    );
    let value = run(&script);
    assert_eq!(value, Value::list(vec![]));
}

#[test]
fn or_return_without_value_returns_nil() {
    let value = run("def pick(v) { stable = v or return; return stable }\npick(nil)");
    assert_eq!(value, Value::Nil);
}

#[test]
fn denies_fs_without_permissions() {
    let tokens = scalf::lexer::lex("fs.exists(\"/tmp/not-used\")").expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut runtime = scalf::runtime::Runtime::new();
    let result = runtime.run_program(&program);
    assert!(result.is_err());
}

#[test]
fn evaluates_list_comprehension_and_graphemes() {
    let value = run("chars = [c for c in \"👍🏽ok\" if c != \"o\"]\n\
         len(chars)");
    assert_eq!(value, Value::Int(2));
}

#[test]
fn evaluates_path_module() {
    let value = run(
        "p = path.join(path.cwd(), \"stdlib\", \"std\", \"math.scl\")\n\
         path.basename(p)",
    );
    assert_eq!(value, Value::String("math.scl".to_string()));
}

#[test]
fn env_get_requires_permission() {
    std::env::set_var("scalf_TEST_ENV_KEY", "ok");
    let tokens = scalf::lexer::lex("env.get(\"scalf_TEST_ENV_KEY\")").expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut restricted = scalf::runtime::Runtime::new();
    assert!(restricted.run_program(&program).is_err());

    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_env = true;
    let mut enabled = scalf::runtime::Runtime::with_permissions(permissions);
    let value = enabled
        .run_program(&program)
        .expect("runtime should succeed");
    assert_eq!(value, Value::String("ok".to_string()));
}

#[test]
fn http_requires_allow_net_permission() {
    let result = run_result(
        "http.get(\"https://example.com\").status",
        scalf::runtime::Permissions::default(),
    );
    let message = result.expect_err("http should be denied").to_string();
    assert!(
        message.contains("network access denied"),
        "expected network permission error, got: {}",
        message
    );
}

#[test]
fn evaluates_http_get_headers_and_json() {
    let (base_url, requests, handle) = spawn_http_server(
        "{\"value\":7}",
        "application/json",
        Duration::from_millis(0),
    );
    let script = format!(
        "resp = http.get(\"{}/v1\", {{\"X-Test\": \"ok\"}}, 2000)\n\
         resp.json()[\"value\"] + resp.status",
        base_url
    );

    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_net.push("127.0.0.1".to_string());
    let value = run_with_permissions(&script, permissions);
    assert_eq!(value, Value::Int(207));

    let request = requests
        .recv_timeout(Duration::from_secs(2))
        .expect("server should capture request");
    let lower = request.to_ascii_lowercase();
    assert!(
        lower.contains("get /v1"),
        "expected GET /v1 request line, got: {}",
        request
    );
    assert!(
        lower.contains("x-test: ok"),
        "expected custom header in request, got: {}",
        request
    );

    handle.join().expect("server thread should finish");
}

#[test]
fn http_honors_timeout() {
    let (base_url, _requests, handle) = spawn_http_server(
        "{\"ok\":true}",
        "application/json",
        Duration::from_millis(150),
    );
    let script = format!("http.get(\"{}/slow\", 10).status", base_url);

    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_net.push("127.0.0.1".to_string());
    let result = run_result(&script, permissions);
    let message = result
        .expect_err("request should timeout")
        .to_string()
        .to_lowercase();
    assert!(
        message.contains("timeout") || message.contains("timed out"),
        "expected timeout error, got: {}",
        message
    );

    handle.join().expect("server thread should finish");
}

#[test]
fn http_requests_start_concurrently() {
    let (url_a, _req_a, handle_a) =
        spawn_http_server("{\"a\":1}", "application/json", Duration::from_millis(300));
    let (url_b, _req_b, handle_b) =
        spawn_http_server("{\"b\":1}", "application/json", Duration::from_millis(300));
    let script = format!(
        "a = http.get(\"{}\", 2000)\n\
         b = http.get(\"{}\", 2000)\n\
         a.status + b.status",
        url_a, url_b
    );

    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_net.push("127.0.0.1".to_string());

    let started = Instant::now();
    let value = run_with_permissions(&script, permissions);
    let elapsed = started.elapsed();

    assert_eq!(value, Value::Int(400));
    assert!(
        elapsed < Duration::from_millis(550),
        "expected overlapping requests, elapsed {:?}",
        elapsed
    );

    handle_a.join().expect("server A thread should finish");
    handle_b.join().expect("server B thread should finish");
}

#[test]
fn evaluates_time_module() {
    let value = run("start = time.now_ms()\n\
         time.sleep(5)\n\
         finish = time.now_ms()\n\
         finish >= start");
    assert_eq!(value, Value::Bool(true));
}

#[test]
fn evaluates_crypto_module() {
    let value = run("crypto.sha256(\"abc\")");
    assert_eq!(
        value,
        Value::String(
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad".to_string()
        )
    );
}

#[test]
fn imports_url_module_with_namespace_isolation() {
    let env = ImportTestEnv::setup();
    let module_source =
        "answer = 41\ndef inc(x: int) -> int { return x + 1 }\nvalue = inc(answer)\n";
    let (base_url, requests, handle) =
        spawn_http_server(module_source, "text/plain", Duration::from_millis(0));

    let import_spec = format!("{}/lib.scalf@v1", base_url);
    let script = format!("use \"{}\" as remote\nremote.value", import_spec);
    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_net.push("127.0.0.1".to_string());
    let value = run_with_permissions(&script, permissions);
    assert_eq!(value, Value::Int(42));

    let lock_text = std::fs::read_to_string(&env.lockfile).expect("lockfile should exist");
    let lock_json: serde_json::Value =
        serde_json::from_str(&lock_text).expect("lockfile should be valid JSON");
    let imports = lock_json
        .get("imports")
        .and_then(serde_json::Value::as_object)
        .expect("lockfile imports should exist");
    let entry = imports
        .get(&import_spec)
        .and_then(serde_json::Value::as_object)
        .expect("import lock entry should exist");
    assert_eq!(entry.get("version").and_then(|v| v.as_str()), Some("v1"));
    let expected_url = format!("{}/lib.scalf", base_url);
    assert_eq!(
        entry.get("url").and_then(|v| v.as_str()),
        Some(expected_url.as_str())
    );

    let request = requests
        .recv_timeout(Duration::from_secs(2))
        .expect("server should capture import request");
    assert!(
        request.to_ascii_lowercase().contains("get /lib.scalf"),
        "expected import request path, got: {}",
        request
    );
    handle.join().expect("import server thread should finish");
}

#[test]
fn import_fails_when_cached_hash_is_tampered() {
    let env = ImportTestEnv::setup();
    let module_source = "value = 7\n";
    let (base_url, _requests, handle) =
        spawn_http_server(module_source, "text/plain", Duration::from_millis(0));

    let import_spec = format!("{}/mod.scalf", base_url);
    let script = format!("use \"{}\" as remote\nremote.value", import_spec);
    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_net.push("127.0.0.1".to_string());
    let first = run_with_permissions(&script, permissions.clone());
    assert_eq!(first, Value::Int(7));
    handle.join().expect("import server thread should finish");

    let lock_text = std::fs::read_to_string(&env.lockfile).expect("lockfile should exist");
    let lock_json: serde_json::Value =
        serde_json::from_str(&lock_text).expect("lockfile should be valid JSON");
    let sha = lock_json["imports"][&import_spec]["sha256"]
        .as_str()
        .expect("sha should exist")
        .to_string();
    let cache_file = env.cache_dir.join("modules").join(format!("{}.scalf", sha));
    std::fs::write(&cache_file, "value = 999\n").expect("tamper cache file");

    let result = run_result(&script, permissions);
    let message = result.expect_err("tampered cache should fail").to_string();
    assert!(
        message.contains("cached import hash mismatch"),
        "expected cache hash mismatch, got: {}",
        message
    );
}

#[test]
fn runs_test_blocks_and_reports_failures() {
    let source = "test \"pass\" { assert 1 + 1 == 2 }\n\
                  test \"fail\" { assert 1 + 1 == 3, \"bad math\" }";
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");

    let mut runtime =
        scalf::runtime::Runtime::with_permissions(scalf::runtime::Permissions::allow_all())
            .with_source_label("tests/runtime_tests.scalf");
    let report = runtime.run_tests(&program);

    assert_eq!(report.passed, 1);
    assert_eq!(report.failed, 1);
    assert_eq!(report.results.len(), 2);
    assert!(report.results[0].passed);
    assert!(!report.results[1].passed);
}

#[test]
fn evaluates_if_while_and_c_style_for() {
    let value = run("sum = 0\n\
         for i = 0; i < 5; i = i + 1 { sum = sum + i }\n\
         while sum < 20 { sum = sum + 1 }\n\
         if sum == 20 { \"ok\" } else { \"bad\" }");
    assert_eq!(value, Value::String("ok".to_string()));
}

#[test]
fn evaluates_for_in_on_list_string_and_map() {
    let value = run("total = 0\n\
         for n in [1, 2, 3] { total = total + n }\n\
         chars = 0\n\
         for ch in \"hi\" { chars = chars + len(ch) }\n\
         keys = 0\n\
         for key in {a: 1, b: 2} { keys = keys + len(key) }\n\
         total + chars + keys");
    assert_eq!(value, Value::Int(10));
}

#[test]
fn resolves_pending_values_with_concurrency_join() {
    let (url_a, _req_a, handle_a) =
        spawn_http_server("{\"a\":1}", "application/json", Duration::from_millis(100));
    let (url_b, _req_b, handle_b) =
        spawn_http_server("{\"b\":1}", "application/json", Duration::from_millis(100));
    let script = format!(
        "a = http.get(\"{}\", 2000)\n\
         b = http.get(\"{}\", 2000)\n\
         all = concurrency.join([a, b])\n\
         all[0].status + all[1].status",
        url_a, url_b
    );

    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_net.push("127.0.0.1".to_string());
    let value = run_with_permissions(&script, permissions);
    assert_eq!(value, Value::Int(400));

    handle_a.join().expect("server A thread should finish");
    handle_b.join().expect("server B thread should finish");
}

#[test]
fn concurrency_timeout_returns_error_value() {
    let (url, _req, handle) = spawn_http_server(
        "{\"ok\":true}",
        "application/json",
        Duration::from_millis(120),
    );
    let script = format!(
        "pending = http.get(\"{}\", 2000)\n\
         concurrency.timeout(10, pending)",
        url
    );

    let mut permissions = scalf::runtime::Permissions::default();
    permissions.allow_net.push("127.0.0.1".to_string());
    let value = run_with_permissions(&script, permissions);
    match value {
        Value::Error(message) => {
            assert!(
                message.to_ascii_lowercase().contains("timed out"),
                "expected timeout error message, got: {}",
                message
            );
        }
        other => panic!("expected Error value, got {:?}", other),
    }

    handle.join().expect("server thread should finish");
}

#[test]
fn channel_send_and_recv_work() {
    let value = run("ch = concurrency.channel()\n\
         ch.send(1)\n\
         ch.send(2)\n\
         sum = ch.recv() + ch.try_recv()\n\
         sum + ch.len()");
    assert_eq!(value, Value::Int(3));
}
