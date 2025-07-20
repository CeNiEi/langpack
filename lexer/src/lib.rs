use std::str::Chars;

use error::{Error, ErrorKind, Result};
use token::{Token, TokenKind};
use utils::span::Span;

pub mod error;
pub mod token;

#[derive(Clone, Copy)]
pub struct Lexer<'a> {
    source: &'a str,
    pointer: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pointer: 0 }
    }

    pub fn source(&self) -> &str {
        self.source
    }

    pub fn lex(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let Some(peeked) = self.peek() else {
            return Ok(Token::new(Span::new(self.pointer, 0), TokenKind::Eof));
        };

        let (kind, len) = match peeked {
            '+' => (TokenKind::Plus, peeked.len_utf8()),
            '-' => (TokenKind::Minus, peeked.len_utf8()),
            '*' => (TokenKind::Star, peeked.len_utf8()),
            '/' => (TokenKind::Slash, peeked.len_utf8()),

            '(' => (TokenKind::LeftParen, peeked.len_utf8()),
            ')' => (TokenKind::RightParen, peeked.len_utf8()),

            _ if peeked.is_alphabetic() => self.lex_idents(),
            _ if peeked.is_ascii_digit() => self.lex_numbers(),

            _ => {
                return Err(Error::new(
                    Span::new(self.pointer, peeked.len_utf8()),
                    ErrorKind::UnexpectedInput,
                ));
            }
        };

        Ok(Token::new(Span::new(self.pointer, len), kind))
    }

    fn lex_idents(&mut self) -> (TokenKind, usize) {
        let mut lexed_bytes = 0;
        let mut iter = self.chars();

        // lex the first character separately
        let first_char = iter.next().unwrap();

        lexed_bytes += first_char.len_utf8();

        while let Some(next_char) = iter.next() {
            match next_char {
                _ if next_char.is_alphanumeric() => {
                    lexed_bytes += next_char.len_utf8();
                }
                _ => {
                    break;
                }
            }
        }

        (TokenKind::Ident, lexed_bytes)
    }

    fn lex_numbers(&mut self) -> (TokenKind, usize) {
        let mut lexed_bytes = 0;
        let mut iter = self.chars();

        let mut decimal_found = false;

        while let Some(next_char) = iter.next() {
            match next_char {
                _ if next_char.is_ascii_digit() => {
                    lexed_bytes += next_char.len_utf8();
                }
                '.' => {
                    decimal_found = true;
                    lexed_bytes += next_char.len_utf8();
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        if decimal_found {
            while let Some(next_char) = iter.next() {
                match next_char {
                    _ if next_char.is_ascii_digit() => {
                        lexed_bytes += next_char.len_utf8();
                    }
                    _ => {
                        break;
                    }
                }
            }
        }

        (TokenKind::Number, lexed_bytes)
    }

    fn skip_whitespace(&mut self) {
        let mut lexed_bytes = 0;

        let mut iter = self.chars();
        while let Some(next_char) = iter.next() {
            if next_char.is_whitespace() {
                lexed_bytes += next_char.len_utf8();
            } else {
                break;
            }
        }

        self.bump(lexed_bytes);
    }

    fn chars(&self) -> Chars {
        self.source[self.pointer..].chars()
    }

    fn peek(&self) -> Option<char> {
        self.chars().next()
    }

    fn bump(&mut self, by: usize) {
        self.pointer += by;
    }
}
