use crate::prelude::{LexToken, LexTokenKind};

pub(crate) trait LexerTokenKindExt {
    fn is_prefix_op(&self) -> bool;
    fn is_infix_op(&self) -> bool;
    fn is_literal(&self) -> bool;
}

impl LexerTokenKindExt for LexTokenKind {
    fn is_prefix_op(&self) -> bool {
        matches!(self, Self::Minus)
    }

    fn is_infix_op(&self) -> bool {
        matches!(self, Self::Plus | Self::Minus | Self::Slash | Self::Star)
    }

    fn is_literal(&self) -> bool {
        matches!(self, Self::Number)
    }
}
