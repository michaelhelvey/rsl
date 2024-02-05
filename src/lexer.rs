#![allow(dead_code)]
use std::{iter::Peekable, str::Chars};

#[derive(Debug, PartialEq)]
pub enum TokenType {
    LParen,
    RParen,
    Number,
    Tick,
    String,
    Identifier,
    Eof,
}

#[derive(Debug)]
pub struct Location {
    file_name: String,
    // row, column
    start: (usize, usize),
    // row, column
    end: (usize, usize),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String) -> Self {
        Self { token_type, lexeme }
    }
}

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer {
        Lexer {
            chars: input.chars().peekable(),
        }
    }
}

fn is_identifier_char(c: char) -> bool {
    c.is_ascii() && !c.is_whitespace() && !c.is_ascii_digit() && c != '(' && c != ')' && c != '\''
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut lexeme = String::new();
        let mut token_type = TokenType::Number;

        'outer: while let Some(c) = self.chars.next() {
            if c.is_whitespace() {
                continue;
            }

            if c.is_ascii_digit() {
                lexeme.push(c);
                token_type = TokenType::Number;

                // read until we find a non-digit character:
                while let Some(c) = self.chars.peek() {
                    if c.is_ascii_digit() {
                        lexeme.push(*c);
                        _ = self.chars.next();
                    } else {
                        // return the token
                        break 'outer;
                    }
                }
            }

            if c == '"' {
                token_type = TokenType::String;

                while let Some(c) = self.chars.next() {
                    // handle escape characters
                    if c == '\\' {
                        // we have already consumed the back slash, so just add the next character
                        // to the string literally and move on
                        match self.chars.next() {
                            Some(c) => lexeme.push(c),
                            None => break,
                        }
                    }

                    if c == '"' {
                        _ = self.chars.next();
                        break 'outer;
                    } else {
                        lexeme.push(c);
                    }
                }
            }

            match c {
                '\'' => {
                    token_type = TokenType::Tick;
                    lexeme.push(c);
                    break;
                }
                '(' => {
                    token_type = TokenType::LParen;
                    lexeme.push(c);
                    break;
                }
                ')' => {
                    token_type = TokenType::RParen;
                    lexeme.push(c);
                    break;
                }
                _ => {}
            }

            // otherwise it's just an identifier
            if is_identifier_char(c) {
                token_type = TokenType::Identifier;
                lexeme.push(c);

                // read until it's not an alphabetic character
                while let Some(c) = self.chars.peek() {
                    if is_identifier_char(*c) {
                        lexeme.push(*c);
                        _ = self.chars.next();
                    } else {
                        // return the token
                        break 'outer;
                    }
                }
            }
        }

        // if we get all the way to the end and we haven't parsed anything, return None
        if lexeme.is_empty() {
            return None;
        }

        Some(Token::new(token_type, lexeme))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let lexer = Lexer::new(" (11 + 2) (add 3 4)");
        let tokens: Vec<Token> = lexer.collect();

        eprintln!("tokens: {:?}", tokens);
        assert_eq!(tokens.len(), 10);
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::LParen, "(".to_string()),
                Token::new(TokenType::Number, "11".to_string()),
                Token::new(TokenType::Identifier, "+".to_string()),
                Token::new(TokenType::Number, "2".to_string()),
                Token::new(TokenType::RParen, ")".to_string()),
                Token::new(TokenType::LParen, "(".to_string()),
                Token::new(TokenType::Identifier, "add".to_string()),
                Token::new(TokenType::Number, "3".to_string()),
                Token::new(TokenType::Number, "4".to_string()),
                Token::new(TokenType::RParen, ")".to_string()),
            ]
        );
    }
}
