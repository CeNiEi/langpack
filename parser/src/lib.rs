use error::Result;
use lexer::{Lexer, token::Token as LexToken};

mod error;
mod symbols;
mod util;

pub(crate) mod prelude {
    pub(crate) use lexer::{
        error::{Error as LexError, ErrorKind as LexErrorKind},
        token::{Token as LexToken, TokenKind as LexTokenKind},
    };
}

pub struct Parser<'s> {
    lexer: Lexer<'s>,
}

impl<'s> Parser<'s> {
    pub fn new(source: &'s str) -> Self {
        Self {
            lexer: Lexer::new(source),
        }
    }

    pub fn next_lexed_token(&mut self) -> Result<LexToken> {
        self.lexer.lex().map_err(Into::into)
    }

    pub fn peek_lexed_token(&mut self) -> Result<LexToken> {
        let mut lexer = self.lexer;

        lexer.lex().map_err(Into::into)
    }

    pub fn source(&self) -> &str {
        self.lexer.source()
    }
}
