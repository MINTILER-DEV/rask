pub mod token;

use std::error::Error;
use std::fmt;

use token::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub struct LexError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl LexError {
    fn new(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            message: message.into(),
            line,
            column,
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "lex error at line {}, column {}: {}",
            self.line, self.column, self.message
        )
    }
}

impl Error for LexError {}

pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(source).lex()
}

struct Lexer<'a> {
    chars: Vec<char>,
    source: &'a str,
    current: usize,
    start: usize,
    line: usize,
    column: usize,
    token_line: usize,
    token_column: usize,
    tokens: Vec<Token>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().collect(),
            source,
            current: 0,
            start: 0,
            line: 1,
            column: 1,
            token_line: 1,
            token_column: 1,
            tokens: Vec::new(),
        }
    }

    fn lex(mut self) -> Result<Vec<Token>, LexError> {
        while !self.is_at_end() {
            self.start_token();
            self.scan_token()?;
        }

        self.tokens.push(Token::new(
            TokenKind::Eof,
            String::new(),
            self.line,
            self.column,
        ));

        Ok(self.tokens)
    }

    fn scan_token(&mut self) -> Result<(), LexError> {
        let c = self.advance();
        match c {
            '(' => self.add_simple(TokenKind::LeftParen),
            ')' => self.add_simple(TokenKind::RightParen),
            '{' => self.add_simple(TokenKind::LeftBrace),
            '}' => self.add_simple(TokenKind::RightBrace),
            '[' => self.add_simple(TokenKind::LeftBracket),
            ']' => self.add_simple(TokenKind::RightBracket),
            ',' => self.add_simple(TokenKind::Comma),
            '.' => self.add_simple(TokenKind::Dot),
            ':' => self.add_simple(TokenKind::Colon),
            '?' => self.add_simple(TokenKind::Question),
            '|' => self.add_simple(TokenKind::Pipe),
            ';' => self.add_simple(TokenKind::Semicolon),
            '+' => self.add_simple(TokenKind::Plus),
            '*' => self.add_simple(TokenKind::Star),
            '%' => self.add_simple(TokenKind::Percent),
            '\n' => self.add_simple(TokenKind::Newline),
            '-' => {
                if self.matches('>') {
                    self.add_simple(TokenKind::Arrow);
                } else {
                    self.add_simple(TokenKind::Minus);
                }
            }
            '!' => {
                if self.matches('=') {
                    self.add_simple(TokenKind::BangEqual);
                } else {
                    self.add_simple(TokenKind::Bang);
                }
            }
            '=' => {
                if self.matches('=') {
                    self.add_simple(TokenKind::EqualEqual);
                } else if self.matches('>') {
                    self.add_simple(TokenKind::FatArrow);
                } else {
                    self.add_simple(TokenKind::Equal);
                }
            }
            '<' => {
                if self.matches('=') {
                    self.add_simple(TokenKind::LessEqual);
                } else {
                    self.add_simple(TokenKind::Less);
                }
            }
            '>' => {
                if self.matches('=') {
                    self.add_simple(TokenKind::GreaterEqual);
                } else {
                    self.add_simple(TokenKind::Greater);
                }
            }
            '/' => {
                if self.matches('/') || self.matches('#') {
                    self.skip_line_comment();
                } else if self.matches('*') {
                    self.skip_block_comment()?;
                } else {
                    self.add_simple(TokenKind::Slash);
                }
            }
            '#' => self.skip_line_comment(),
            '"' => self.string()?,
            ' ' | '\r' | '\t' => {}
            d if d.is_ascii_digit() => self.number()?,
            a if is_ident_start(a) => self.identifier(),
            _ => {
                return Err(LexError::new(
                    format!("unexpected character '{}'", c),
                    self.token_line,
                    self.token_column,
                ))
            }
        }

        Ok(())
    }

    fn string(&mut self) -> Result<(), LexError> {
        let mut value = String::new();
        let mut has_interpolation = false;
        let mut closed = false;

        while !self.is_at_end() {
            let c = self.advance();
            match c {
                '"' => {
                    closed = true;
                    break;
                }
                '\\' => {
                    if self.is_at_end() {
                        break;
                    }
                    let escaped = self.advance();
                    match escaped {
                        '"' => value.push('"'),
                        '\\' => value.push('\\'),
                        'n' => value.push('\n'),
                        'r' => value.push('\r'),
                        't' => value.push('\t'),
                        '{' => value.push('{'),
                        '}' => value.push('}'),
                        other => value.push(other),
                    }
                }
                '{' => {
                    has_interpolation = true;
                    value.push(c);
                }
                _ => value.push(c),
            }
        }

        if !closed {
            return Err(LexError::new(
                "unterminated string literal",
                self.token_line,
                self.token_column,
            ));
        }

        self.add_token(TokenKind::String {
            value,
            has_interpolation,
        });
        Ok(())
    }

    fn number(&mut self) -> Result<(), LexError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        let mut is_float = false;
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            is_float = true;
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let lexeme = self.current_lexeme();
        if is_float {
            let value = lexeme.parse::<f64>().map_err(|_| {
                LexError::new(
                    format!("invalid float literal '{}'", lexeme),
                    self.token_line,
                    self.token_column,
                )
            })?;
            self.add_token(TokenKind::Float(value));
        } else {
            let value = lexeme.parse::<i64>().map_err(|_| {
                LexError::new(
                    format!("invalid int literal '{}'", lexeme),
                    self.token_line,
                    self.token_column,
                )
            })?;
            self.add_token(TokenKind::Int(value));
        }

        Ok(())
    }

    fn identifier(&mut self) {
        while is_ident_continue(self.peek()) {
            self.advance();
        }

        let lexeme = self.current_lexeme();
        let kind = match lexeme.as_str() {
            "def" => TokenKind::Def,
            "print" => TokenKind::Print,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "while" => TokenKind::While,
            "for" => TokenKind::For,
            "return" => TokenKind::Return,
            "or" => TokenKind::Or,
            "and" => TokenKind::And,
            "match" => TokenKind::Match,
            "use" => TokenKind::Use,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,
            _ => TokenKind::Identifier(lexeme.clone()),
        };

        self.add_token(kind);
    }

    fn skip_line_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        while !self.is_at_end() {
            if self.peek() == '*' && self.peek_next() == '/' {
                self.advance();
                self.advance();
                return Ok(());
            }
            self.advance();
        }

        Err(LexError::new(
            "unterminated block comment",
            self.token_line,
            self.token_column,
        ))
    }

    fn add_simple(&mut self, kind: TokenKind) {
        self.add_token(kind);
    }

    fn add_token(&mut self, kind: TokenKind) {
        let lexeme = self.current_lexeme();
        self.tokens
            .push(Token::new(kind, lexeme, self.token_line, self.token_column));
    }

    fn start_token(&mut self) {
        self.start = self.current;
        self.token_line = self.line;
        self.token_column = self.column;
    }

    fn current_lexeme(&self) -> String {
        self.chars[self.start..self.current].iter().collect()
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            return false;
        }
        self.advance();
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.chars[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.current + 1]
        }
    }

    fn advance(&mut self) -> char {
        let c = self.chars[self.current];
        self.current += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        c
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }

    #[allow(dead_code)]
    fn source(&self) -> &str {
        self.source
    }
}

fn is_ident_start(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

fn is_ident_continue(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric()
}
