use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token {
    span: Span,
    kind: TokenKind,
}

impl Token {
    pub fn new(span: Span, kind: TokenKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    Plus,
    Minus,
    Star,
    Slash,

    LeftParen,
    RightParen,

    Ident,
    Number,

    Eof,
}
