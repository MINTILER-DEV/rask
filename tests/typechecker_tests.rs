use rask::typechecker::types::{is_assignable, Type};

fn check(source: &str) -> Result<rask::typechecker::TypeCheckOutput, Vec<rask::typechecker::TypeError>> {
    let tokens = rask::lexer::lex(source).expect("lex should succeed");
    let mut parser = rask::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut checker = rask::typechecker::TypeChecker::new();
    checker.check_program(&program)
}

#[test]
fn parses_complex_type_annotation() {
    let parsed = Type::parse("(int, string?) -> Result<float, Error>").expect("type parse");
    let expected = Type::Function {
        params: vec![Type::Int, Type::Union(vec![Type::String, Type::Nil])],
        ret: Box::new(Type::Result(
            Box::new(Type::Float),
            Box::new(Type::Error),
        )),
    };
    assert_eq!(parsed, expected);
}

#[test]
fn checks_basic_inference() {
    let output = check("x = 42\nprint(x + 2)").expect("typecheck should succeed");
    assert_eq!(output.inferred_types.get("x"), Some(&Type::Int));
}

#[test]
fn supports_nullable_annotation_assignment() {
    let output = check("user_id: int? = nil").expect("typecheck should succeed");
    let user_id_type = output
        .inferred_types
        .get("user_id")
        .expect("user_id type should exist");
    assert!(is_assignable(&Type::Nil, user_id_type));
}

#[test]
fn detects_annotation_mismatch() {
    let errors = check("name: string = 42").expect_err("typecheck should fail");
    assert!(
        errors
            .iter()
            .any(|err| err.message.contains("cannot assign value of type 'int'")),
        "expected int -> string mismatch, got: {:?}",
        errors
    );
}

#[test]
fn detects_unknown_variable() {
    let errors = check("print(missing)").expect_err("typecheck should fail");
    assert!(
        errors
            .iter()
            .any(|err| err.message.contains("unknown variable 'missing'")),
        "expected unknown variable error, got: {:?}",
        errors
    );
}

#[test]
fn checks_or_return_inside_function() {
    let output = check(
        "def ensure_name(value: string?) -> string { stable = value or return \"missing\"; return stable }\nname = ensure_name(nil)",
    )
    .expect("typecheck should succeed");
    assert_eq!(output.inferred_types.get("name"), Some(&Type::String));
}

#[test]
fn checks_match_expression_inference() {
    let output = check(
        "def classify(code: int) -> string { return match code { 200 => \"ok\", _ => \"bad\" } }\nstatus = classify(200)",
    )
    .expect("typecheck should succeed");
    assert_eq!(output.inferred_types.get("status"), Some(&Type::String));
}

#[test]
fn checks_destructuring_bindings() {
    let output =
        check("def first(values: List<int>) -> int { [head, _] = values; return head }")
            .expect("typecheck should succeed");
    match output.inferred_types.get("first") {
        Some(Type::Function { ret, .. }) => assert_eq!(ret.as_ref(), &Type::Int),
        other => panic!("expected function type for first, got {:?}", other),
    }
}

#[test]
fn detects_match_pattern_type_mismatch() {
    let errors = check("value = match 1 { \"a\" => 1, _ => 2 }").expect_err("typecheck should fail");
    assert!(
        errors
            .iter()
            .any(|err| err.message.contains("pattern type 'string'")),
        "expected pattern mismatch error, got: {:?}",
        errors
    );
}

#[test]
fn infers_list_comprehension_type() {
    let output = check("nums = [1,2,3]\ndoubled = [x * 2 for x in nums]").expect("typecheck ok");
    assert_eq!(
        output.inferred_types.get("doubled"),
        Some(&Type::List(Box::new(Type::Int)))
    );
}
