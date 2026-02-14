pub mod ast;
mod expr;

use std::error::Error;
use std::fmt;

use crate::lexer::token::{Token, TokenKind};
use ast::{MapPatternEntry, Param, Pattern, Program, Stmt, UseTarget};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl ParseError {
    fn new(message: impl Into<String>, token: &Token) -> Self {
        Self {
            message: message.into(),
            line: token.line,
            column: token.column,
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "parse error at line {}, column {}: {}",
            self.line, self.column, self.message
        )
    }
}

impl Error for ParseError {}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut statements = Vec::new();

        self.skip_statement_breaks();
        while !self.is_at_end() {
            statements.push(self.statement()?);
            self.skip_statement_breaks();
        }

        Ok(Program { statements })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.matches_keyword(TokenKind::Use) {
            return self.use_statement();
        }

        if self.matches_keyword(TokenKind::If) {
            return self.if_statement();
        }

        if self.matches_keyword(TokenKind::While) {
            return self.while_statement();
        }

        if self.matches_keyword(TokenKind::For) {
            return self.for_statement();
        }

        if self.matches_keyword(TokenKind::Def) {
            return self.function_definition();
        }

        if self.matches_keyword(TokenKind::Return) {
            return self.return_statement();
        }

        if self.matches_keyword(TokenKind::Print) {
            return self.print_statement();
        }

        if self.looks_like_var_decl() {
            return self.variable_declaration();
        }

        if self.looks_like_destructure_decl() {
            return self.destructure_declaration();
        }

        if self.matches_identifier_literal("test") {
            return self.test_statement();
        }

        if self.matches_identifier_literal("assert") {
            return self.assert_statement();
        }

        let expr = self.expression()?;
        Ok(Stmt::Expr(expr))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        let condition = self.expression()?;
        self.consume_symbol(TokenKind::LeftBrace, "expected '{' before if body")?;
        let then_branch = self.parse_block_statements()?;

        let else_branch = if self.matches_keyword(TokenKind::Else) {
            if self.matches_keyword(TokenKind::If) {
                Some(vec![self.if_statement()?])
            } else {
                self.consume_symbol(TokenKind::LeftBrace, "expected '{' before else body")?;
                Some(self.parse_block_statements()?)
            }
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        let condition = self.expression()?;
        self.consume_symbol(TokenKind::LeftBrace, "expected '{' before while body")?;
        let body = self.parse_block_statements()?;
        Ok(Stmt::While { condition, body })
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        if matches!(self.peek_kind(), TokenKind::Identifier(_))
            && matches!(self.peek_kind_at(1), Some(TokenKind::Identifier(name)) if name == "in")
        {
            return self.for_in_statement();
        }
        self.for_c_style_statement()
    }

    fn for_in_statement(&mut self) -> Result<Stmt, ParseError> {
        let item_name = self.consume_identifier("expected item name after 'for'")?;
        if !self.matches_identifier_literal("in") {
            return Err(ParseError::new(
                "expected 'in' in for-in statement",
                self.peek(),
            ));
        }
        let iterable = self.expression()?;
        self.consume_symbol(TokenKind::LeftBrace, "expected '{' before for-in body")?;
        let body = self.parse_block_statements()?;
        Ok(Stmt::ForIn {
            item_name,
            iterable,
            body,
        })
    }

    fn for_c_style_statement(&mut self) -> Result<Stmt, ParseError> {
        let initializer = if self.matches_symbol(TokenKind::Semicolon) {
            None
        } else {
            let stmt = if self.looks_like_var_decl() {
                self.variable_declaration()?
            } else {
                Stmt::Expr(self.expression()?)
            };
            self.consume_symbol(
                TokenKind::Semicolon,
                "expected ';' after for-loop initializer",
            )?;
            Some(Box::new(stmt))
        };

        let condition = if self.matches_symbol(TokenKind::Semicolon) {
            None
        } else {
            let expr = self.expression()?;
            self.consume_symbol(
                TokenKind::Semicolon,
                "expected ';' after for-loop condition",
            )?;
            Some(expr)
        };

        let increment = if self.check_kind(&TokenKind::LeftBrace) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume_symbol(TokenKind::LeftBrace, "expected '{' before for-loop body")?;
        let body = self.parse_block_statements()?;

        Ok(Stmt::For {
            initializer,
            condition,
            increment,
            body,
        })
    }

    fn use_statement(&mut self) -> Result<Stmt, ParseError> {
        let target = match self.peek_kind().clone() {
            TokenKind::String { value, .. } => {
                self.advance();
                UseTarget::Url(value)
            }
            TokenKind::Identifier(_) => {
                let mut path = Vec::new();
                path.push(self.consume_identifier("expected module path after 'use'")?);
                while self.matches_symbol(TokenKind::Dot) {
                    path.push(
                        self.consume_identifier("expected identifier after '.' in use path")?,
                    );
                }
                UseTarget::ModulePath(path)
            }
            _ => {
                return Err(ParseError::new(
                    "expected module path or URL string after 'use'",
                    self.peek(),
                ))
            }
        };

        let alias = if self.matches_identifier_literal("as") {
            Some(self.consume_identifier("expected alias name after 'as'")?)
        } else {
            None
        };

        Ok(Stmt::Use { target, alias })
    }

    fn test_statement(&mut self) -> Result<Stmt, ParseError> {
        let name = match self.peek_kind().clone() {
            TokenKind::String { value, .. } => {
                self.advance();
                value
            }
            _ => {
                return Err(ParseError::new(
                    "expected test name string after 'test'",
                    self.peek(),
                ))
            }
        };
        self.consume_symbol(TokenKind::LeftBrace, "expected '{' before test body")?;
        let body = self.parse_block_statements()?;
        Ok(Stmt::Test { name, body })
    }

    fn assert_statement(&mut self) -> Result<Stmt, ParseError> {
        let condition = self.expression()?;
        let message = if self.matches_symbol(TokenKind::Comma) {
            Some(self.expression()?)
        } else {
            None
        };
        Ok(Stmt::Assert { condition, message })
    }

    fn function_definition(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume_identifier("expected function name after 'def'")?;
        self.consume_symbol(TokenKind::LeftParen, "expected '(' after function name")?;

        let mut params = Vec::new();
        if !self.check_kind(&TokenKind::RightParen) {
            loop {
                let param_name = self.consume_identifier("expected parameter name")?;
                let type_annotation = if self.matches_symbol(TokenKind::Colon) {
                    Some(self.collect_type_annotation_until(
                        |kind| matches!(kind, TokenKind::Comma | TokenKind::RightParen),
                        "expected parameter type annotation after ':'",
                    )?)
                } else {
                    None
                };
                params.push(Param {
                    name: param_name,
                    type_annotation,
                });

                if !self.matches_symbol(TokenKind::Comma) {
                    break;
                }
            }
        }
        self.consume_symbol(TokenKind::RightParen, "expected ')' after parameter list")?;

        let return_type = if self.matches_symbol(TokenKind::Arrow) {
            Some(self.collect_type_annotation_until(
                |kind| matches!(kind, TokenKind::LeftBrace),
                "expected return type after '->'",
            )?)
        } else {
            None
        };

        self.consume_symbol(TokenKind::LeftBrace, "expected '{' before function body")?;
        let body = self.parse_block_statements()?;
        Ok(Stmt::FunctionDef {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_block_statements(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut body = Vec::new();
        self.skip_statement_breaks();
        while !self.check_kind(&TokenKind::RightBrace) && !self.is_at_end() {
            body.push(self.statement()?);
            self.skip_statement_breaks();
        }
        self.consume_symbol(TokenKind::RightBrace, "expected '}' to close block")?;
        Ok(body)
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check_kind(&TokenKind::Newline)
            || self.check_kind(&TokenKind::Semicolon)
            || self.check_kind(&TokenKind::RightBrace)
            || self.check_kind(&TokenKind::Eof)
        {
            return Ok(Stmt::Return { value: None });
        }

        let value = self.expression()?;
        Ok(Stmt::Return { value: Some(value) })
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = if self.matches_symbol(TokenKind::LeftParen) {
            let value = self.expression()?;
            self.consume_symbol(TokenKind::RightParen, "expected ')' after print argument")?;
            value
        } else {
            self.expression()?
        };

        Ok(Stmt::Print { expr })
    }

    fn variable_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume_identifier("expected variable name")?;
        let mut type_annotation = None;
        if self.matches_symbol(TokenKind::Colon) {
            type_annotation = Some(self.collect_type_annotation_until(
                |kind| matches!(kind, TokenKind::Equal),
                "expected type annotation after ':'",
            )?);
        }
        self.consume_symbol(TokenKind::Equal, "expected '=' in variable declaration")?;
        let initializer = self.expression()?;

        Ok(Stmt::VarDecl {
            name,
            type_annotation,
            initializer,
        })
    }

    fn destructure_declaration(&mut self) -> Result<Stmt, ParseError> {
        let pattern = self.parse_pattern()?;
        self.consume_symbol(
            TokenKind::Equal,
            "expected '=' in destructuring declaration",
        )?;
        let initializer = self.expression()?;
        Ok(Stmt::DestructureDecl {
            pattern,
            initializer,
        })
    }

    fn looks_like_var_decl(&self) -> bool {
        if !matches!(self.peek_kind(), TokenKind::Identifier(_)) {
            return false;
        }

        matches!(
            self.peek_kind_at(1),
            Some(TokenKind::Equal) | Some(TokenKind::Colon)
        )
    }

    fn looks_like_destructure_decl(&self) -> bool {
        if !matches!(
            self.peek_kind(),
            TokenKind::LeftBracket | TokenKind::LeftBrace
        ) {
            return false;
        }

        let mut i = self.current;
        let mut bracket_depth: i32 = 0;
        let mut brace_depth: i32 = 0;
        while let Some(token) = self.tokens.get(i) {
            match token.kind {
                TokenKind::LeftBracket => bracket_depth += 1,
                TokenKind::RightBracket => bracket_depth -= 1,
                TokenKind::LeftBrace => brace_depth += 1,
                TokenKind::RightBrace => brace_depth -= 1,
                TokenKind::Equal if bracket_depth == 0 && brace_depth == 0 => return true,
                TokenKind::Semicolon | TokenKind::Newline
                    if bracket_depth == 0 && brace_depth == 0 =>
                {
                    return false
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn collect_type_annotation_until<F>(
        &mut self,
        mut stop: F,
        empty_message: &str,
    ) -> Result<String, ParseError>
    where
        F: FnMut(&TokenKind) -> bool,
    {
        let mut pieces = Vec::new();
        let mut paren_depth: i32 = 0;
        let mut bracket_depth: i32 = 0;
        let mut brace_depth: i32 = 0;
        let mut angle_depth: i32 = 0;

        while !self.is_at_end() {
            let kind = self.peek_kind();
            if paren_depth == 0
                && bracket_depth == 0
                && brace_depth == 0
                && angle_depth == 0
                && stop(kind)
            {
                break;
            }

            match kind {
                TokenKind::LeftParen => paren_depth += 1,
                TokenKind::RightParen => paren_depth -= 1,
                TokenKind::LeftBracket => bracket_depth += 1,
                TokenKind::RightBracket => bracket_depth -= 1,
                TokenKind::LeftBrace => brace_depth += 1,
                TokenKind::RightBrace => brace_depth -= 1,
                TokenKind::Less => angle_depth += 1,
                TokenKind::Greater => angle_depth -= 1,
                _ => {}
            }

            pieces.push(self.advance().lexeme.clone());
        }

        if pieces.is_empty() {
            return Err(ParseError::new(empty_message, self.peek()));
        }

        Ok(pieces.join(""))
    }

    pub(crate) fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        match self.peek_kind().clone() {
            TokenKind::Identifier(name) => {
                self.advance();
                if name == "_" {
                    Ok(Pattern::Wildcard)
                } else {
                    Ok(Pattern::Identifier(name))
                }
            }
            TokenKind::Int(value) => {
                self.advance();
                Ok(Pattern::Int(value))
            }
            TokenKind::Float(value) => {
                self.advance();
                Ok(Pattern::Float(value))
            }
            TokenKind::String { value, .. } => {
                self.advance();
                Ok(Pattern::String(value))
            }
            TokenKind::True => {
                self.advance();
                Ok(Pattern::Bool(true))
            }
            TokenKind::False => {
                self.advance();
                Ok(Pattern::Bool(false))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Pattern::Nil)
            }
            TokenKind::LeftBracket => {
                self.advance();
                let mut items = Vec::new();
                if !self.check_kind(&TokenKind::RightBracket) {
                    loop {
                        items.push(self.parse_pattern()?);
                        if !self.matches_symbol(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.consume_symbol(TokenKind::RightBracket, "expected ']' after list pattern")?;
                Ok(Pattern::List(items))
            }
            TokenKind::LeftBrace => {
                self.advance();
                let mut entries = Vec::new();
                if !self.check_kind(&TokenKind::RightBrace) {
                    loop {
                        let key =
                            self.consume_identifier("expected identifier key in map pattern")?;
                        let pattern = if self.matches_symbol(TokenKind::Colon) {
                            self.parse_pattern()?
                        } else if key == "_" {
                            Pattern::Wildcard
                        } else {
                            Pattern::Identifier(key.clone())
                        };

                        entries.push(MapPatternEntry { key, pattern });
                        if !self.matches_symbol(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.consume_symbol(TokenKind::RightBrace, "expected '}' after map pattern")?;
                Ok(Pattern::Map(entries))
            }
            _ => Err(ParseError::new("expected pattern", self.peek())),
        }
    }

    pub(crate) fn skip_statement_breaks(&mut self) {
        while self.matches_symbol(TokenKind::Newline) || self.matches_symbol(TokenKind::Semicolon) {
        }
    }

    pub(crate) fn matches_keyword(&mut self, kind: TokenKind) -> bool {
        self.matches_symbol(kind)
    }

    pub(crate) fn matches_symbol(&mut self, kind: TokenKind) -> bool {
        if self.check_kind(&kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    pub(crate) fn consume_symbol(
        &mut self,
        kind: TokenKind,
        message: &str,
    ) -> Result<(), ParseError> {
        if self.check_kind(&kind) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::new(message, self.peek()))
        }
    }

    pub(crate) fn consume_identifier(&mut self, message: &str) -> Result<String, ParseError> {
        match self.peek_kind() {
            TokenKind::Identifier(name) => {
                let value = name.clone();
                self.advance();
                Ok(value)
            }
            _ => Err(ParseError::new(message, self.peek())),
        }
    }

    pub(crate) fn consume_property_name(&mut self, message: &str) -> Result<String, ParseError> {
        match self.peek_kind() {
            TokenKind::Identifier(name) => {
                let value = name.clone();
                self.advance();
                Ok(value)
            }
            TokenKind::Def
            | TokenKind::Print
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::While
            | TokenKind::For
            | TokenKind::Return
            | TokenKind::Or
            | TokenKind::Match
            | TokenKind::Use
            | TokenKind::True
            | TokenKind::False
            | TokenKind::Nil => {
                let value = self.peek().lexeme.clone();
                self.advance();
                Ok(value)
            }
            _ => Err(ParseError::new(message, self.peek())),
        }
    }

    pub(crate) fn matches_identifier_literal(&mut self, expected: &str) -> bool {
        match self.peek_kind() {
            TokenKind::Identifier(name) if name == expected => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    pub(crate) fn check_kind(&self, kind: &TokenKind) -> bool {
        if self.is_at_end() {
            return matches!(kind, TokenKind::Eof);
        }

        token_kinds_equal(self.peek_kind(), kind)
    }

    pub(crate) fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    pub(crate) fn is_at_end(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }

    pub(crate) fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub(crate) fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    fn peek_kind_at(&self, offset: usize) -> Option<&TokenKind> {
        self.tokens
            .get(self.current + offset)
            .map(|token| &token.kind)
    }

    pub(crate) fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}

fn token_kinds_equal(a: &TokenKind, b: &TokenKind) -> bool {
    match (a, b) {
        (TokenKind::LeftParen, TokenKind::LeftParen)
        | (TokenKind::RightParen, TokenKind::RightParen)
        | (TokenKind::LeftBrace, TokenKind::LeftBrace)
        | (TokenKind::RightBrace, TokenKind::RightBrace)
        | (TokenKind::LeftBracket, TokenKind::LeftBracket)
        | (TokenKind::RightBracket, TokenKind::RightBracket)
        | (TokenKind::Comma, TokenKind::Comma)
        | (TokenKind::Dot, TokenKind::Dot)
        | (TokenKind::Colon, TokenKind::Colon)
        | (TokenKind::Question, TokenKind::Question)
        | (TokenKind::Pipe, TokenKind::Pipe)
        | (TokenKind::FatArrow, TokenKind::FatArrow)
        | (TokenKind::Semicolon, TokenKind::Semicolon)
        | (TokenKind::Plus, TokenKind::Plus)
        | (TokenKind::Minus, TokenKind::Minus)
        | (TokenKind::Star, TokenKind::Star)
        | (TokenKind::Slash, TokenKind::Slash)
        | (TokenKind::Percent, TokenKind::Percent)
        | (TokenKind::Bang, TokenKind::Bang)
        | (TokenKind::BangEqual, TokenKind::BangEqual)
        | (TokenKind::Equal, TokenKind::Equal)
        | (TokenKind::EqualEqual, TokenKind::EqualEqual)
        | (TokenKind::Greater, TokenKind::Greater)
        | (TokenKind::GreaterEqual, TokenKind::GreaterEqual)
        | (TokenKind::Less, TokenKind::Less)
        | (TokenKind::LessEqual, TokenKind::LessEqual)
        | (TokenKind::Arrow, TokenKind::Arrow)
        | (TokenKind::Newline, TokenKind::Newline)
        | (TokenKind::Def, TokenKind::Def)
        | (TokenKind::Print, TokenKind::Print)
        | (TokenKind::If, TokenKind::If)
        | (TokenKind::Else, TokenKind::Else)
        | (TokenKind::While, TokenKind::While)
        | (TokenKind::For, TokenKind::For)
        | (TokenKind::Return, TokenKind::Return)
        | (TokenKind::Or, TokenKind::Or)
        | (TokenKind::Match, TokenKind::Match)
        | (TokenKind::Use, TokenKind::Use)
        | (TokenKind::True, TokenKind::True)
        | (TokenKind::False, TokenKind::False)
        | (TokenKind::Nil, TokenKind::Nil)
        | (TokenKind::Eof, TokenKind::Eof) => true,
        _ => false,
    }
}
