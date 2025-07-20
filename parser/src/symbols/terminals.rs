use std::str::FromStr;

use utils::span::Span;

use crate::error::{EofErrMapper, Error, ErrorKind};
use crate::prelude::LexTokenKind;

use super::Symbol;

#[derive(Debug, PartialEq)]
pub(crate) enum LiteralToken {
    Num(NumToken),
}

impl LiteralToken {
    pub(crate) fn span(&self) -> Span {
        match self {
            Self::Num(a) => a.span(),
        }
    }
}

impl<'s> Symbol<'s> for LiteralToken {
    fn parse(parser: &mut super::Parser<'s>) -> crate::error::Result<Self> {
        let next_token = parser.peek_lexed_token().map_eof_to_err()?;

        match next_token.kind() {
            LexTokenKind::Number => NumToken::parse(parser).map(LiteralToken::Num),
            _ => Err(Error::new(next_token.span(), ErrorKind::UnexpectedToken)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct NumToken {
    value: f32,
    span: Span,
}

impl NumToken {
    pub(crate) fn new(value: f32, span: Span) -> Self {
        Self { value, span }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl<'s> Symbol<'s> for NumToken {
    fn parse(parser: &mut super::Parser<'s>) -> crate::error::Result<Self> {
        let next_token = parser.next_lexed_token().map_eof_to_err()?;

        let kind = next_token.kind();
        let span = next_token.span();

        match kind {
            LexTokenKind::Number => {
                let raw_num = &parser.source()[span.start()..(span.start() + span.len())];

                let value = f32::from_str(raw_num)
                    .expect("already checked for validity in the lexing stage");

                Ok(NumToken::new(value, span))
            }
            _ => Err(Error::new(span, ErrorKind::UnexpectedToken)),
        }
    }
}
