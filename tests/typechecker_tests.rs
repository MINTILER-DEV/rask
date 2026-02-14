use scalf::typechecker::types::{is_assignable, Type};

fn check(
    source: &str,
) -> Result<scalf::typechecker::TypeCheckOutput, Vec<scalf::typechecker::TypeError>> {
    check_with_implicit_nil(source, false)
}

fn check_with_implicit_nil(
    source: &str,
    enabled: bool,
) -> Result<scalf::typechecker::TypeCheckOutput, Vec<scalf::typechecker::TypeError>> {
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    let mut checker =
        scalf::typechecker::TypeChecker::new().with_implicit_nil_for_unknown_variables(enabled);
    checker.check_program(&program)
}

#[test]
fn parses_complex_type_annotation() {
    let parsed = Type::parse("(int, string?) -> Result<float, Error>").expect("type parse");
    let expected = Type::Function {
        params: vec![Type::Int, Type::Union(vec![Type::String, Type::Nil])],
        ret: Box::new(Type::Result(Box::new(Type::Float), Box::new(Type::Error))),
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
fn allows_unknown_variable_when_implicit_nil_enabled() {
    let output = check_with_implicit_nil("print(missing or 5)\nprint(missing?.name)", true);
    assert!(
        output.is_ok(),
        "expected implicit nil mode to allow unknowns"
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
fn infers_inline_function_expression_type() {
    let output = check("adder = def(x: int, y: int) -> int { return x + y }\nvalue = adder(1, 2)")
        .expect("typecheck should succeed");
    assert_eq!(output.inferred_types.get("value"), Some(&Type::Int));
}

#[test]
fn infers_boolean_logic_types() {
    let output =
        check("a = false or true\nb = a and true\nresult = a and b").expect("typecheck ok");
    assert_eq!(output.inferred_types.get("result"), Some(&Type::Bool));
}

#[test]
fn checks_destructuring_bindings() {
    let output = check("def first(values: List<int>) -> int { [head, _] = values; return head }")
        .expect("typecheck should succeed");
    match output.inferred_types.get("first") {
        Some(Type::Function { ret, .. }) => assert_eq!(ret.as_ref(), &Type::Int),
        other => panic!("expected function type for first, got {:?}", other),
    }
}

#[test]
fn detects_match_pattern_type_mismatch() {
    let errors =
        check("value = match 1 { \"a\" => 1, _ => 2 }").expect_err("typecheck should fail");
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

#[test]
fn checks_test_and_assert_statements() {
    let output = check("test \"ok\" { assert 1 + 1 == 2 }").expect("typecheck should succeed");
    assert!(output.inferred_types.contains_key("len"));
}

#[test]
fn checks_if_while_and_for_in_statements() {
    let output = check(
        "nums = [1, 2, 3]\n\
         total = 0\n\
         ok = false\n\
         for n in nums { total = total + n }\n\
         if total > 0 { ok = true } else { ok = false }\n\
         while false { total = total + 1 }",
    )
    .expect("typecheck should succeed");

    assert_eq!(output.inferred_types.get("total"), Some(&Type::Int));
    assert_eq!(output.inferred_types.get("ok"), Some(&Type::Bool));
}

#[test]
fn rejects_non_bool_loop_condition() {
    let errors = check("while 123 { value = 1 }").expect_err("typecheck should fail");
    assert!(
        errors
            .iter()
            .any(|err| err.message.contains("while condition must be bool")),
        "expected while condition type error, got: {:?}",
        errors
    );
}

#[test]
fn allows_equality_comparison_against_nil_for_any_type() {
    let output = check("headers = {host: \"x\"}\nmissing = headers == nil")
        .expect("typecheck should succeed");
    assert_eq!(output.inferred_types.get("missing"), Some(&Type::Bool));
}
