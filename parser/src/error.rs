use crate::prelude::{LexError, LexErrorKind, LexToken, LexTokenKind};
use utils::span::Span;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for Error {}

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEOF,
    UnexpectedToken,
    LexError(LexErrorKind),
}

impl From<LexError> for Error {
    fn from(value: LexError) -> Self {
        Self::new(value.span(), ErrorKind::LexError(value.kind()))
    }
}

pub trait EofErrMapper: Sized {
    fn map_eof_to_err(self) -> Self;
}

impl EofErrMapper for Result<LexToken> {
    fn map_eof_to_err(self) -> Self {
        self.and_then(|token| {
            if token.kind() == LexTokenKind::Eof {
                Err(Error::new(token.span(), ErrorKind::UnexpectedEOF))
            } else {
                Ok(token)
            }
        })
    }
}
