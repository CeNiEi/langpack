use crate::Span;

pub(crate) type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    span: Span,
    kind: ErrorKind,
}

impl Error {
    pub fn new(span: Span, kind: ErrorKind) -> Self {
        Self { span, kind }
    }

    pub fn kind(&self) -> ErrorKind {
        self.kind
    }

    pub fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    UnexpectedEof,
    UnexpectedInput,
}
