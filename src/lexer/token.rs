#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Colon,
    Question,
    Pipe,
    FatArrow,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Arrow,
    Newline,
    Identifier(String),
    Int(i64),
    Float(f64),
    String {
        value: String,
        has_interpolation: bool,
    },
    Def,
    Print,
    If,
    Else,
    While,
    For,
    Return,
    Or,
    And,
    Match,
    Use,
    True,
    False,
    Nil,
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize, column: usize) -> Self {
        Self {
            kind,
            lexeme,
            line,
            column,
        }
    }
}
