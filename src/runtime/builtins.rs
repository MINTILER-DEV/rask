use std::collections::HashMap;
use std::rc::Rc;

use super::value::Value;

pub fn core_globals() -> HashMap<String, Value> {
    let mut globals = HashMap::new();

    globals.insert(
        "list".to_string(),
        Value::NativeFunction("core.list".to_string()),
    );
    globals.insert(
        "map".to_string(),
        Value::NativeFunction("core.map".to_string()),
    );
    globals.insert(
        "len".to_string(),
        Value::NativeFunction("core.len".to_string()),
    );
    globals.insert(
        "Path".to_string(),
        Value::NativeFunction("core.Path".to_string()),
    );

    globals.insert("math".to_string(), math_module());
    globals.insert("fs".to_string(), fs_module());
    globals.insert("json".to_string(), json_module());
    globals.insert("path".to_string(), path_module());
    globals.insert("env".to_string(), env_module());

    globals
}

fn math_module() -> Value {
    let mut entries = HashMap::new();
    entries.insert("pi".to_string(), Value::Float(std::f64::consts::PI));
    entries.insert("e".to_string(), Value::Float(std::f64::consts::E));
    entries.insert(
        "min".to_string(),
        Value::NativeFunction("math.min".to_string()),
    );
    entries.insert(
        "max".to_string(),
        Value::NativeFunction("math.max".to_string()),
    );
    entries.insert(
        "abs".to_string(),
        Value::NativeFunction("math.abs".to_string()),
    );
    entries.insert(
        "round".to_string(),
        Value::NativeFunction("math.round".to_string()),
    );
    Value::Module(Rc::new(entries))
}

fn fs_module() -> Value {
    let mut entries = HashMap::new();
    entries.insert(
        "read".to_string(),
        Value::NativeFunction("fs.read".to_string()),
    );
    entries.insert(
        "write".to_string(),
        Value::NativeFunction("fs.write".to_string()),
    );
    entries.insert(
        "exists".to_string(),
        Value::NativeFunction("fs.exists".to_string()),
    );
    entries.insert(
        "delete".to_string(),
        Value::NativeFunction("fs.delete".to_string()),
    );
    Value::Module(Rc::new(entries))
}

fn json_module() -> Value {
    let mut entries = HashMap::new();
    entries.insert(
        "parse".to_string(),
        Value::NativeFunction("json.parse".to_string()),
    );
    entries.insert(
        "stringify".to_string(),
        Value::NativeFunction("json.stringify".to_string()),
    );
    Value::Module(Rc::new(entries))
}

fn path_module() -> Value {
    let mut entries = HashMap::new();
    entries.insert(
        "join".to_string(),
        Value::NativeFunction("path.join".to_string()),
    );
    entries.insert(
        "normalize".to_string(),
        Value::NativeFunction("path.normalize".to_string()),
    );
    entries.insert(
        "basename".to_string(),
        Value::NativeFunction("path.basename".to_string()),
    );
    entries.insert(
        "dirname".to_string(),
        Value::NativeFunction("path.dirname".to_string()),
    );
    entries.insert(
        "cwd".to_string(),
        Value::NativeFunction("path.cwd".to_string()),
    );
    entries.insert(
        "to_string".to_string(),
        Value::NativeFunction("path.to_string".to_string()),
    );
    Value::Module(Rc::new(entries))
}

fn env_module() -> Value {
    let mut entries = HashMap::new();
    entries.insert(
        "get".to_string(),
        Value::NativeFunction("env.get".to_string()),
    );
    Value::Module(Rc::new(entries))
}
