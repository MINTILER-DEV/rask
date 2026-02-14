use scalf::parser::ast::{BinaryOp, Expr, Pattern, Stmt, UseTarget};

fn parse(source: &str) -> Vec<Stmt> {
    let tokens = scalf::lexer::lex(source).expect("lex should succeed");
    let mut parser = scalf::parser::Parser::new(tokens);
    let program = parser.parse_program().expect("parse should succeed");
    program.statements
}

#[test]
fn parses_variable_declaration() {
    let statements = parse("count: int = 2 + 3");
    match &statements[0] {
        Stmt::VarDecl {
            name,
            type_annotation,
            initializer,
        } => {
            assert_eq!(name, "count");
            assert_eq!(type_annotation.as_deref(), Some("int"));
            match initializer {
                Expr::Binary { op, .. } => assert_eq!(*op, BinaryOp::Add),
                _ => panic!("expected binary expression"),
            }
        }
        _ => panic!("expected variable declaration"),
    }
}

#[test]
fn parses_print_statement() {
    let statements = parse("print(1 + 2)");
    match &statements[0] {
        Stmt::Print { expr } => match expr {
            Expr::Binary { op, .. } => assert_eq!(*op, BinaryOp::Add),
            _ => panic!("expected binary expression"),
        },
        _ => panic!("expected print statement"),
    }
}

#[test]
fn parses_untyped_variable_declaration() {
    let statements = parse("value = 10");
    match &statements[0] {
        Stmt::VarDecl { .. } => {}
        _ => panic!("expected top-level declaration form"),
    }
}

#[test]
fn captures_complex_type_annotation() {
    let statements = parse("handler: (int, string?) -> Result<float, Error> = str");
    match &statements[0] {
        Stmt::VarDecl {
            type_annotation: Some(annotation),
            ..
        } => {
            assert_eq!(annotation, "(int,string?)->Result<float,Error>");
        }
        _ => panic!("expected typed variable declaration"),
    }
}

#[test]
fn parses_optional_chain_and_coalesce() {
    let statements = parse("name = user?.profile?.display or \"anon\"");
    match &statements[0] {
        Stmt::VarDecl { initializer, .. } => match initializer {
            Expr::Coalesce { lhs, rhs } => {
                assert!(matches!(rhs.as_ref(), Expr::String { .. }));
                match lhs.as_ref() {
                    Expr::Member { optional, .. } => assert!(*optional),
                    _ => panic!("expected optional member chain"),
                }
            }
            _ => panic!("expected coalesce expression"),
        },
        _ => panic!("expected variable declaration"),
    }
}

#[test]
fn parses_member_call_with_keyword_property_name() {
    let statements = parse("app.use(router.logger())");
    match &statements[0] {
        Stmt::Expr(Expr::Call { callee, args }) => {
            assert_eq!(args.len(), 1);
            match callee.as_ref() {
                Expr::Member {
                    object, property, ..
                } => {
                    assert_eq!(property, "use");
                    assert!(matches!(object.as_ref(), Expr::Variable(name) if name == "app"));
                }
                _ => panic!("expected member call"),
            }
            assert!(matches!(args[0], Expr::Call { .. }));
        }
        _ => panic!("expected expression statement"),
    }
}

#[test]
fn parses_inline_function_expression_in_call_arguments() {
    let statements = parse("app.get(\"/\", def(req, res) { return res })");
    match &statements[0] {
        Stmt::Expr(Expr::Call { args, .. }) => {
            assert_eq!(args.len(), 2);
            assert!(matches!(args[0], Expr::String { .. }));
            match &args[1] {
                Expr::Function { params, body, .. } => {
                    assert_eq!(params.len(), 2);
                    assert_eq!(params[0].name, "req");
                    assert_eq!(params[1].name, "res");
                    assert_eq!(body.len(), 1);
                }
                _ => panic!("expected inline function expression"),
            }
        }
        _ => panic!("expected call expression statement"),
    }
}

#[test]
fn parses_function_with_or_return_and_panic_unwrap() {
    let statements = parse(
        "def process(user: string) -> string { parsed = parse(user) or return \"err\"; return parsed! }",
    );
    match &statements[0] {
        Stmt::FunctionDef {
            name,
            params,
            return_type,
            body,
        } => {
            assert_eq!(name, "process");
            assert_eq!(params.len(), 1);
            assert_eq!(params[0].type_annotation.as_deref(), Some("string"));
            assert_eq!(return_type.as_deref(), Some("string"));
            assert_eq!(body.len(), 2);
            match &body[0] {
                Stmt::VarDecl { initializer, .. } => {
                    assert!(matches!(initializer, Expr::OrReturn { .. }))
                }
                _ => panic!("expected var declaration in function body"),
            }
            match &body[1] {
                Stmt::Return { value: Some(expr) } => {
                    assert!(matches!(expr, Expr::PanicUnwrap(_)));
                }
                _ => panic!("expected return with panic unwrap"),
            }
        }
        _ => panic!("expected function definition"),
    }
}

#[test]
fn parses_or_return_without_value_as_nil() {
    let statements =
        parse("def load() { value = fs.read(\"missing.txt\") or return; return value }");
    match &statements[0] {
        Stmt::FunctionDef { body, .. } => match &body[0] {
            Stmt::VarDecl { initializer, .. } => match initializer {
                Expr::OrReturn { return_value, .. } => {
                    assert!(matches!(return_value.as_ref(), Expr::Nil));
                }
                _ => panic!("expected or-return expression"),
            },
            _ => panic!("expected var declaration in function body"),
        },
        _ => panic!("expected function definition"),
    }
}

#[test]
fn parses_match_expression() {
    let statements = parse("result = match status { 200 => \"ok\", _ => \"bad\" }");
    match &statements[0] {
        Stmt::VarDecl { initializer, .. } => match initializer {
            Expr::Match { arms, .. } => {
                assert_eq!(arms.len(), 2);
                assert!(matches!(arms[1].pattern, Pattern::Wildcard));
            }
            _ => panic!("expected match expression"),
        },
        _ => panic!("expected variable declaration"),
    }
}

#[test]
fn parses_destructuring_declaration() {
    let statements = parse("[first, _] = names");
    match &statements[0] {
        Stmt::DestructureDecl { pattern, .. } => match pattern {
            Pattern::List(items) => {
                assert_eq!(items.len(), 2);
                assert!(matches!(&items[0], Pattern::Identifier(name) if name == "first"));
                assert!(matches!(&items[1], Pattern::Wildcard));
            }
            _ => panic!("expected list pattern"),
        },
        _ => panic!("expected destructuring declaration"),
    }
}

#[test]
fn parses_use_statement_with_alias() {
    let statements = parse("use std.json as jsonlib");
    match &statements[0] {
        Stmt::Use { target, alias } => {
            assert_eq!(
                target,
                &UseTarget::ModulePath(vec!["std".to_string(), "json".to_string()])
            );
            assert_eq!(alias.as_deref(), Some("jsonlib"));
        }
        _ => panic!("expected use statement"),
    }
}

#[test]
fn parses_url_use_statement_with_alias() {
    let statements = parse("use \"https://example.com/lib.scalf@v1\" as lib");
    match &statements[0] {
        Stmt::Use { target, alias } => {
            assert_eq!(
                target,
                &UseTarget::Url("https://example.com/lib.scalf@v1".to_string())
            );
            assert_eq!(alias.as_deref(), Some("lib"));
        }
        _ => panic!("expected use statement"),
    }
}

#[test]
fn parses_list_map_literals_and_index() {
    let statements = parse("value = {name: \"scalf\", nums: [1, 2, 3]}[\"name\"]");
    match &statements[0] {
        Stmt::VarDecl { initializer, .. } => match initializer {
            Expr::Index { object, .. } => {
                assert!(matches!(object.as_ref(), Expr::MapLiteral(_)));
            }
            _ => panic!("expected index expression"),
        },
        _ => panic!("expected var declaration"),
    }
}

#[test]
fn parses_multiline_map_literal() {
    let statements = parse("value = {\n  name: \"scalf\",\n  version: 1,\n}\nvalue[\"name\"]");
    match &statements[0] {
        Stmt::VarDecl { initializer, .. } => {
            assert!(matches!(initializer, Expr::MapLiteral(_)));
        }
        _ => panic!("expected var declaration"),
    }
}

#[test]
fn parses_list_comprehension() {
    let statements = parse("items = [x * 2 for x in nums if x > 1]");
    match &statements[0] {
        Stmt::VarDecl { initializer, .. } => match initializer {
            Expr::ListComprehension { item_name, .. } => {
                assert_eq!(item_name, "x");
            }
            _ => panic!("expected list comprehension"),
        },
        _ => panic!("expected var declaration"),
    }
}

#[test]
fn parses_multiline_list_literal() {
    let statements = parse("items = [\n  1,\n  2,\n  3,\n]\nitems[0]");
    match &statements[0] {
        Stmt::VarDecl { initializer, .. } => {
            assert!(matches!(initializer, Expr::ListLiteral(_)));
        }
        _ => panic!("expected var declaration"),
    }
}

#[test]
fn parses_test_block_and_assert() {
    let statements = parse("test \"math works\" { assert 2 + 2 == 4 }");
    match &statements[0] {
        Stmt::Test { name, body } => {
            assert_eq!(name, "math works");
            assert_eq!(body.len(), 1);
            assert!(matches!(body[0], Stmt::Assert { .. }));
        }
        _ => panic!("expected test statement"),
    }
}

#[test]
fn parses_if_else_and_while() {
    let statements = parse("if true { x = 1 } else { x = 2 }\nwhile x < 10 { x = x + 1 }");
    match &statements[0] {
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert!(matches!(condition, Expr::Bool(true)));
            assert_eq!(then_branch.len(), 1);
            assert!(else_branch.as_ref().is_some_and(|branch| branch.len() == 1));
        }
        _ => panic!("expected if statement"),
    }

    match &statements[1] {
        Stmt::While { condition, body } => {
            assert!(matches!(condition, Expr::Binary { .. }));
            assert_eq!(body.len(), 1);
        }
        _ => panic!("expected while statement"),
    }
}

#[test]
fn parses_for_in_and_c_style_for() {
    let statements =
        parse("for item in items { print(item) }\nfor i = 0; i < 3; i = i + 1 { print(i) }");

    match &statements[0] {
        Stmt::ForIn {
            item_name,
            iterable,
            body,
        } => {
            assert_eq!(item_name, "item");
            assert!(matches!(iterable, Expr::Variable(name) if name == "items"));
            assert_eq!(body.len(), 1);
        }
        _ => panic!("expected for-in statement"),
    }

    match &statements[1] {
        Stmt::For {
            initializer,
            condition,
            increment,
            body,
        } => {
            assert!(matches!(
                initializer.as_deref(),
                Some(Stmt::VarDecl { name, .. }) if name == "i"
            ));
            assert!(matches!(condition, Some(Expr::Binary { .. })));
            assert!(matches!(increment, Some(Expr::Assign { name, .. }) if name == "i"));
            assert_eq!(body.len(), 1);
        }
        _ => panic!("expected c-style for statement"),
    }
}
