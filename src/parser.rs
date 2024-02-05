#![allow(dead_code)]
use std::iter::Peekable;
use thiserror::Error;

use crate::lexer::{Lexer, Token, TokenType};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected token: expected {expected:?}, received {received:?}")]
    UnexpectedToken {
        received: TokenType,
        expected: TokenType,
    },
    #[error("unexpected token: received {received:?}, while parsing {wile:?}")]
    UnexpectedTokenWhile { received: TokenType, wile: String },
}

pub struct Parser<'a> {
    tokens: Peekable<Lexer<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum Value {
    List(Vec<Expression>),
    Identifier(String),
    Number(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, PartialEq)]
pub struct Sexpr {
    pub head: Expression,
    pub tail: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Atom(Value),
    Sexpr(Box<Sexpr>),
}

impl Parser<'_> {
    pub fn new(input: &str) -> Parser {
        Parser {
            tokens: Lexer::new(input).peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.tokens.peek() {
            return match token.token_type {
                TokenType::LParen => self.parse_sexp(),
                // (quote (1 2 3)) evals into (1 2 3) (the code, e.g. Expression::Sexpr..) not (1 2
                // 3) (an unevaluatable thing that would probably be evaluated as a function and
                // fail because there's no function called "1")
                TokenType::Tick => {
                    // 'foo => (quote Atom::Identifier("foo"))
                    // '() => (quote Atom::Nil)
                    // '(1 2 3) => (quote (1 2 3))
                    self.tokens.next();
                    let tail = self.parse()?;
                    let head = Expression::Atom(Value::Identifier("quote".to_string()));
                    let sexpr = Sexpr { head, tail };
                    Ok(Expression::Sexpr(Box::new(sexpr)))
                }
                _ => self.parse_atom(),
            };
        }

        Ok(Expression::Atom(Value::Nil))
    }

    fn parse_sexp(&mut self) -> Result<Expression, ParseError> {
        self.consume(TokenType::LParen)?;
        let head = self.parse()?;
        let tail = self.parse_sexp_partial()?;

        let sexpr = Sexpr { head, tail };
        Ok(Expression::Sexpr(Box::new(sexpr)))
    }

    // used to support the modern interpretation of (1 2 3) as an implicitly nested s-expression:
    // (1 2 3) == (1 . (2 . (3 . nil)))
    fn parse_sexp_partial(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.tokens.peek() {
            if token.token_type == TokenType::RParen {
                _ = self.tokens.next();
                // base case:
                return Ok(Expression::Atom(Value::Nil));
            }

            let head = self.parse()?;
            let tail = self.parse_sexp_partial()?;

            let sexpr = Sexpr { head, tail };
            return Ok(Expression::Sexpr(Box::new(sexpr)));
        };

        Err(ParseError::UnexpectedTokenWhile {
            received: TokenType::Eof,
            wile: "sexp partial".to_string(),
        })
    }

    fn parse_atom(&mut self) -> Result<Expression, ParseError> {
        if let Some(token) = self.tokens.next() {
            return match token.token_type {
                TokenType::Identifier => {
                    Ok(Expression::Atom(Value::Identifier(token.lexeme.clone())))
                }
                TokenType::Number => Ok(Expression::Atom(Value::Number(
                    token.lexeme.parse().unwrap(),
                ))),
                TokenType::String => Ok(Expression::Atom(Value::String(token.lexeme.clone()))),
                _ => Err(ParseError::UnexpectedTokenWhile {
                    received: token.token_type,
                    wile: "atom".to_string(),
                }),
            };
        }

        Err(ParseError::UnexpectedTokenWhile {
            received: TokenType::Eof,
            wile: "atom".to_string(),
        })
    }

    fn consume(&mut self, token_type: TokenType) -> Result<Token, ParseError> {
        match self.tokens.next() {
            Some(token) if token.token_type == token_type => Ok(token),
            Some(token) => Err(ParseError::UnexpectedToken {
                received: token.token_type,
                expected: token_type,
            }),
            None => Err(ParseError::UnexpectedToken {
                received: TokenType::Eof,
                expected: token_type,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let mut parser = Parser::new("(1 2 3)");
        let result = parser.parse().unwrap();
        assert_eq!(
            result,
            Expression::Sexpr(Box::new(Sexpr {
                head: Expression::Atom(Value::Number(1.0)),
                tail: Expression::Sexpr(Box::new(Sexpr {
                    head: Expression::Atom(Value::Number(2.0)),
                    tail: Expression::Sexpr(Box::new(Sexpr {
                        head: Expression::Atom(Value::Number(3.0)),
                        tail: Expression::Atom(Value::Nil),
                    })),
                })),
            }))
        );
    }

    #[test]
    fn test_parse_quotes() {
        let mut parser = Parser::new("'(1 2 3)");
        let result = parser.parse().unwrap();
        assert_eq!(
            result,
            Expression::Sexpr(Box::new(Sexpr {
                head: Expression::Atom(Value::Identifier("quote".to_string())),
                tail: Expression::Sexpr(Box::new(Sexpr {
                    head: Expression::Atom(Value::Number(1.0)),
                    tail: Expression::Sexpr(Box::new(Sexpr {
                        head: Expression::Atom(Value::Number(2.0)),
                        tail: Expression::Sexpr(Box::new(Sexpr {
                            head: Expression::Atom(Value::Number(3.0)),
                            tail: Expression::Atom(Value::Nil),
                        })),
                    })),
                })),
            }))
        );
    }
}
