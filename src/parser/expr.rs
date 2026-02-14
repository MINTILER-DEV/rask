use crate::lexer::token::TokenKind;

use super::ast::{BinaryOp, Expr, MapEntryExpr, MatchArm, Param, UnaryOp};
use super::{ParseError, Parser};

impl Parser {
    pub(crate) fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.coalesce()?;

        if self.matches_symbol(TokenKind::Equal) {
            let value = self.assignment()?;
            return match expr {
                Expr::Variable(name) => Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                }),
                Expr::Member {
                    object,
                    property,
                    optional,
                } => {
                    if optional {
                        return Err(ParseError::new(
                            "invalid assignment target",
                            self.previous(),
                        ));
                    }
                    Ok(Expr::Call {
                        callee: Box::new(Expr::Member {
                            object,
                            property: "set".to_string(),
                            optional: false,
                        }),
                        args: vec![
                            Expr::String {
                                value: property,
                                has_interpolation: false,
                            },
                            value,
                        ],
                    })
                }
                Expr::Index { object, index } => Ok(Expr::Call {
                    callee: Box::new(Expr::Member {
                        object,
                        property: "set".to_string(),
                        optional: false,
                    }),
                    args: vec![*index, value],
                }),
                _ => Err(ParseError::new(
                    "invalid assignment target",
                    self.previous(),
                )),
            };
        }

        Ok(expr)
    }

    fn coalesce(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.logical_and()?;

        while self.matches_keyword(TokenKind::Or) {
            if self.matches_keyword(TokenKind::Return) {
                let return_value = if self.check_kind(&TokenKind::Newline)
                    || self.check_kind(&TokenKind::Semicolon)
                    || self.check_kind(&TokenKind::RightBrace)
                    || self.check_kind(&TokenKind::Eof)
                {
                    Expr::Nil
                } else {
                    self.expression()?
                };
                expr = Expr::OrReturn {
                    lhs: Box::new(expr),
                    return_value: Box::new(return_value),
                };
            } else {
                let rhs = self.logical_and()?;
                expr = Expr::Coalesce {
                    lhs: Box::new(expr),
                    rhs: Box::new(rhs),
                };
            }
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;

        while self.matches_keyword(TokenKind::And) {
            let rhs = self.equality()?;
            expr = Expr::Binary {
                lhs: Box::new(expr),
                op: BinaryOp::And,
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;

        loop {
            let op = if self.matches_symbol(TokenKind::EqualEqual) {
                Some(BinaryOp::Equal)
            } else if self.matches_symbol(TokenKind::BangEqual) {
                Some(BinaryOp::NotEqual)
            } else {
                None
            };

            let Some(op) = op else { break };
            let rhs = self.comparison()?;
            expr = Expr::Binary {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;

        loop {
            let op = if self.matches_symbol(TokenKind::Greater) {
                Some(BinaryOp::Greater)
            } else if self.matches_symbol(TokenKind::GreaterEqual) {
                Some(BinaryOp::GreaterEqual)
            } else if self.matches_symbol(TokenKind::Less) {
                Some(BinaryOp::Less)
            } else if self.matches_symbol(TokenKind::LessEqual) {
                Some(BinaryOp::LessEqual)
            } else {
                None
            };

            let Some(op) = op else { break };
            let rhs = self.term()?;
            expr = Expr::Binary {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;

        loop {
            let op = if self.matches_symbol(TokenKind::Plus) {
                Some(BinaryOp::Add)
            } else if self.matches_symbol(TokenKind::Minus) {
                Some(BinaryOp::Subtract)
            } else {
                None
            };

            let Some(op) = op else { break };
            let rhs = self.factor()?;
            expr = Expr::Binary {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;

        loop {
            let op = if self.matches_symbol(TokenKind::Star) {
                Some(BinaryOp::Multiply)
            } else if self.matches_symbol(TokenKind::Slash) {
                Some(BinaryOp::Divide)
            } else if self.matches_symbol(TokenKind::Percent) {
                Some(BinaryOp::Modulo)
            } else {
                None
            };

            let Some(op) = op else { break };
            let rhs = self.unary()?;
            expr = Expr::Binary {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(rhs),
            };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        if self.matches_symbol(TokenKind::Bang) {
            let rhs = self.unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Not,
                rhs: Box::new(rhs),
            });
        }

        if self.matches_symbol(TokenKind::Minus) {
            let rhs = self.unary()?;
            return Ok(Expr::Unary {
                op: UnaryOp::Negate,
                rhs: Box::new(rhs),
            });
        }

        self.postfix()
    }

    fn postfix(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;

        loop {
            if self.matches_symbol(TokenKind::LeftParen) {
                let mut args = Vec::new();
                if !self.check_kind(&TokenKind::RightParen) {
                    loop {
                        args.push(self.expression()?);
                        if !self.matches_symbol(TokenKind::Comma) {
                            break;
                        }
                    }
                }
                self.consume_symbol(TokenKind::RightParen, "expected ')' after arguments")?;
                expr = Expr::Call {
                    callee: Box::new(expr),
                    args,
                };
                continue;
            }

            if self.matches_symbol(TokenKind::Dot) {
                let property = self.consume_property_name("expected property name after '.'")?;
                expr = Expr::Member {
                    object: Box::new(expr),
                    property,
                    optional: false,
                };
                continue;
            }

            if self.matches_symbol(TokenKind::Question) {
                self.consume_symbol(TokenKind::Dot, "expected '.' after '?' in optional chain")?;
                let property = self.consume_property_name("expected property name after '?.'")?;
                expr = Expr::Member {
                    object: Box::new(expr),
                    property,
                    optional: true,
                };
                continue;
            }

            if self.matches_symbol(TokenKind::LeftBracket) {
                let index = self.expression()?;
                self.consume_symbol(
                    TokenKind::RightBracket,
                    "expected ']' after index expression",
                )?;
                expr = Expr::Index {
                    object: Box::new(expr),
                    index: Box::new(index),
                };
                continue;
            }

            if self.matches_symbol(TokenKind::Bang) {
                expr = Expr::PanicUnwrap(Box::new(expr));
                continue;
            }

            break;
        }

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind().clone() {
            TokenKind::Int(value) => {
                self.advance();
                Ok(Expr::Int(value))
            }
            TokenKind::Float(value) => {
                self.advance();
                Ok(Expr::Float(value))
            }
            TokenKind::String {
                value,
                has_interpolation,
            } => {
                self.advance();
                Ok(Expr::String {
                    value,
                    has_interpolation,
                })
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::Bool(true))
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::Bool(false))
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Expr::Nil)
            }
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(Expr::Variable(name))
            }
            TokenKind::Def => self.function_expression(),
            TokenKind::LeftParen => {
                self.advance();
                let expr = self.expression()?;
                self.consume_symbol(TokenKind::RightParen, "expected ')' after expression")?;
                Ok(Expr::Grouping(Box::new(expr)))
            }
            TokenKind::LeftBracket => self.list_literal(),
            TokenKind::LeftBrace => self.map_literal(),
            TokenKind::Match => self.match_expression(),
            _ => Err(ParseError::new("expected expression", self.peek())),
        }
    }

    fn function_expression(&mut self) -> Result<Expr, ParseError> {
        self.consume_symbol(TokenKind::Def, "expected 'def'")?;
        self.consume_symbol(
            TokenKind::LeftParen,
            "expected '(' after 'def' in function expression",
        )?;

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
        Ok(Expr::Function {
            params,
            return_type,
            body,
        })
    }

    fn list_literal(&mut self) -> Result<Expr, ParseError> {
        self.consume_symbol(TokenKind::LeftBracket, "expected '['")?;
        self.skip_statement_breaks();
        if self.check_kind(&TokenKind::RightBracket) {
            self.consume_symbol(TokenKind::RightBracket, "expected ']' after list literal")?;
            return Ok(Expr::ListLiteral(Vec::new()));
        }

        let first_expr = self.expression()?;
        if self.matches_keyword(TokenKind::For) {
            let item_name =
                self.consume_identifier("expected item variable after 'for' in comprehension")?;
            if !self.matches_identifier_literal("in") {
                return Err(ParseError::new(
                    "expected 'in' in list comprehension",
                    self.peek(),
                ));
            }
            let iterable = self.expression()?;
            let condition = if self.matches_keyword(TokenKind::If) {
                Some(Box::new(self.expression()?))
            } else {
                None
            };
            self.consume_symbol(
                TokenKind::RightBracket,
                "expected ']' after list comprehension",
            )?;
            return Ok(Expr::ListComprehension {
                expr: Box::new(first_expr),
                item_name,
                iterable: Box::new(iterable),
                condition,
            });
        }

        let mut items = vec![first_expr];
        while self.matches_symbol(TokenKind::Comma) {
            self.skip_statement_breaks();
            if self.check_kind(&TokenKind::RightBracket) {
                break;
            }
            items.push(self.expression()?);
        }
        self.skip_statement_breaks();
        self.consume_symbol(TokenKind::RightBracket, "expected ']' after list literal")?;
        Ok(Expr::ListLiteral(items))
    }

    fn map_literal(&mut self) -> Result<Expr, ParseError> {
        self.consume_symbol(TokenKind::LeftBrace, "expected '{'")?;
        self.skip_statement_breaks();
        let mut entries = Vec::new();
        if !self.check_kind(&TokenKind::RightBrace) {
            loop {
                let key = match self.peek_kind().clone() {
                    TokenKind::String { value, .. } => {
                        self.advance();
                        value
                    }
                    _ => self.consume_property_name(
                        "expected identifier or string key in map literal",
                    )?,
                };
                self.consume_symbol(TokenKind::Colon, "expected ':' after map key")?;
                let value = self.expression()?;
                entries.push(MapEntryExpr { key, value });

                if self.matches_symbol(TokenKind::Comma) {
                    self.skip_statement_breaks();
                    if self.check_kind(&TokenKind::RightBrace) {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        self.skip_statement_breaks();
        self.consume_symbol(TokenKind::RightBrace, "expected '}' after map literal")?;
        Ok(Expr::MapLiteral(entries))
    }

    fn match_expression(&mut self) -> Result<Expr, ParseError> {
        self.consume_symbol(TokenKind::Match, "expected 'match'")?;
        let subject = self.expression()?;
        self.consume_symbol(TokenKind::LeftBrace, "expected '{' after match subject")?;

        let mut arms = Vec::new();
        self.skip_statement_breaks();
        while !self.check_kind(&TokenKind::RightBrace) && !self.is_at_end() {
            let pattern = self.parse_pattern()?;
            self.consume_symbol(TokenKind::FatArrow, "expected '=>' in match arm")?;
            let value = self.expression()?;
            arms.push(MatchArm { pattern, value });

            if self.matches_symbol(TokenKind::Comma) {
                self.skip_statement_breaks();
            } else {
                self.skip_statement_breaks();
            }
        }

        self.consume_symbol(TokenKind::RightBrace, "expected '}' after match arms")?;
        Ok(Expr::Match {
            subject: Box::new(subject),
            arms,
        })
    }
}
