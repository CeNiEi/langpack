use crate::{
    error::EofErrMapper,
    prelude::{LexToken, LexTokenKind},
    util::LexerTokenKindExt,
};
use utils::span::Span;

use crate::error::{Error, ErrorKind};

use super::{Symbol, terminals::LiteralToken};

#[derive(Debug, PartialEq)]
pub(crate) enum ExprToken {
    LitExpr(LiteralToken),
    BinExpr(BinExprToken),
    UnExpr(UnEpxrToken),
    ParenExpr(ParenExprToken),
}

impl ExprToken {
    pub(crate) fn span(&self) -> Span {
        match self {
            Self::LitExpr(a) => a.span(),
            Self::BinExpr(a) => a.span(),
            Self::UnExpr(a) => a.span(),
            Self::ParenExpr(a) => a.span(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum BinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl BinOp {
    fn infix_binding_power(&self) -> (usize, usize) {
        match self {
            Self::Add | Self::Subtract => (3, 4),
            Self::Multiply | Self::Divide => (5, 6),
        }
    }
}

impl TryFrom<LexToken> for BinOp {
    type Error = crate::error::Error;
    fn try_from(value: LexToken) -> Result<Self, Self::Error> {
        match value.kind() {
            LexTokenKind::Minus => Ok(Self::Subtract),
            LexTokenKind::Plus => Ok(Self::Add),
            LexTokenKind::Star => Ok(Self::Multiply),
            LexTokenKind::Slash => Ok(Self::Divide),
            _ => Err(Error::new(value.span(), ErrorKind::UnexpectedToken)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct BinExprToken {
    lhs: Box<ExprToken>,
    rhs: Box<ExprToken>,
    op: BinOp,
    span: Span,
}

impl BinExprToken {
    pub(crate) fn new(lhs: ExprToken, rhs: ExprToken, op: BinOp, span: Span) -> Self {
        Self {
            span,
            op,

            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl<'s> Symbol<'s> for BinExprToken {
    fn parse(parser: &mut super::Parser<'s>) -> crate::error::Result<Self> {
        match parse_expr_helper(parser, 0)? {
            ExprToken::BinExpr(expr) => Ok(expr),
            rest => Err(Error::new(rest.span(), ErrorKind::UnexpectedToken)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum UnOp {
    Negation,
}

impl UnOp {
    fn prefix_binding_power(&self) -> usize {
        match self {
            Self::Negation => 7,
        }
    }
}

impl TryFrom<LexToken> for UnOp {
    type Error = crate::error::Error;
    fn try_from(value: LexToken) -> Result<Self, Self::Error> {
        match value.kind() {
            LexTokenKind::Minus => Ok(Self::Negation),
            _ => Err(Error::new(value.span(), ErrorKind::UnexpectedToken)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct UnEpxrToken {
    pub op: UnOp,
    pub expr: Box<ExprToken>,
    pub span: Span,
}

impl UnEpxrToken {
    pub(crate) fn new(expr: ExprToken, op: UnOp, span: Span) -> Self {
        Self {
            op,
            expr: Box::new(expr),
            span,
        }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl<'s> Symbol<'s> for UnEpxrToken {
    fn parse(parser: &mut super::Parser<'s>) -> crate::error::Result<Self> {
        let next_token = parser.next_lexed_token().map_eof_to_err()?;

        let un_op = TryInto::<UnOp>::try_into(next_token)?;
        let right_bp = un_op.prefix_binding_power();
        let inner = parse_expr_helper(parser, right_bp)?;

        let span = next_token.span() + inner.span();

        Ok(Self::new(inner, un_op, span))
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct ParenExprToken {
    inner: Box<ExprToken>,
    span: Span,
}

impl ParenExprToken {
    pub(crate) fn new(inner: ExprToken, span: Span) -> Self {
        Self {
            inner: Box::new(inner),
            span,
        }
    }

    pub(crate) fn span(&self) -> Span {
        self.span
    }
}

impl<'s> Symbol<'s> for ParenExprToken {
    fn parse(parser: &mut super::Parser<'s>) -> crate::error::Result<Self> {
        let next_token = parser.next_lexed_token().map_eof_to_err()?;

        if next_token.kind() != LexTokenKind::LeftParen {
            return Err(Error::new(next_token.span(), ErrorKind::UnexpectedToken));
        };

        let mut span = next_token.span();

        let expr = parse_expr_helper(parser, 0)?;

        span += expr.span();

        let next_token = parser.next_lexed_token().map_eof_to_err()?;

        if next_token.kind() != LexTokenKind::RightParen {
            return Err(Error::new(next_token.span(), ErrorKind::UnexpectedToken));
        };

        span += next_token.span();

        Ok(Self::new(expr, span))
    }
}

fn parse_expr_helper<'s>(
    parser: &mut super::Parser<'s>,
    minimum_bp: usize,
) -> crate::error::Result<ExprToken> {
    let next_token = parser.peek_lexed_token().map_eof_to_err()?;

    let mut lhs = match next_token.kind() {
        kind if kind.is_prefix_op() => UnEpxrToken::parse(parser).map(ExprToken::UnExpr)?,
        kind if kind.is_literal() => LiteralToken::parse(parser).map(ExprToken::LitExpr)?,
        LexTokenKind::LeftParen => ParenExprToken::parse(parser).map(ExprToken::ParenExpr)?,
        _ => {
            return Err(Error::new(next_token.span(), ErrorKind::UnexpectedToken));
        }
    };

    loop {
        let next_token = parser.peek_lexed_token().map_eof_to_err()?;

        if !next_token.kind().is_infix_op() {
            break;
        }

        let bin_op = TryInto::<BinOp>::try_into(next_token)?;
        let (left_bp, right_bp) = bin_op.infix_binding_power();

        if left_bp < minimum_bp {
            break;
        }

        parser.next_lexed_token()?;
        let rhs = parse_expr_helper(parser, right_bp)?;

        let span = lhs.span() + next_token.span() + rhs.span();

        lhs = ExprToken::BinExpr(BinExprToken::new(lhs, rhs, bin_op, span))
    }

    Ok(lhs)
}
