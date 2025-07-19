use crate::Span;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedInput,
}
