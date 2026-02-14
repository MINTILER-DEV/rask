use scalf::lexer::token::TokenKind;

#[test]
fn lexes_basic_declaration() {
    let tokens = scalf::lexer::lex("x: int = 42\n").expect("lex should succeed");
    assert!(matches!(tokens[0].kind, TokenKind::Identifier(_)));
    assert_eq!(tokens[1].kind, TokenKind::Colon);
    assert!(matches!(tokens[2].kind, TokenKind::Identifier(_)));
    assert_eq!(tokens[3].kind, TokenKind::Equal);
    assert_eq!(tokens[4].kind, TokenKind::Int(42));
}

#[test]
fn lexes_float_and_comparison() {
    let tokens = scalf::lexer::lex("pi = 3.14 >= 3").expect("lex should succeed");
    assert!(matches!(tokens[0].kind, TokenKind::Identifier(_)));
    assert_eq!(tokens[1].kind, TokenKind::Equal);
    assert_eq!(tokens[2].kind, TokenKind::Float(3.14));
    assert_eq!(tokens[3].kind, TokenKind::GreaterEqual);
    assert_eq!(tokens[4].kind, TokenKind::Int(3));
}

#[test]
fn lexes_string_with_interpolation_flag() {
    let tokens = scalf::lexer::lex("message = \"Hello, {name}!\"").expect("lex should succeed");
    match &tokens[2].kind {
        TokenKind::String {
            value,
            has_interpolation,
        } => {
            assert_eq!(value, "Hello, {name}!");
            assert!(*has_interpolation);
        }
        other => panic!("expected string token, got {:?}", other),
    }
}

#[test]
fn lexes_nullable_and_union_tokens() {
    let tokens = scalf::lexer::lex("value: int? | Error = nil").expect("lex should succeed");
    assert_eq!(tokens[3].kind, TokenKind::Question);
    assert_eq!(tokens[4].kind, TokenKind::Pipe);
}

#[test]
fn lexes_phase2_control_tokens() {
    let tokens = scalf::lexer::lex("match value { _ => value or return err and ok }")
        .expect("lex should succeed");
    assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Match)));
    assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::FatArrow)));
    assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::Or)));
    assert!(tokens.iter().any(|t| matches!(t.kind, TokenKind::And)));
}
