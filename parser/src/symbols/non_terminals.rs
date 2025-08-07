use crate::{
    error::EofErrMapper,
    prelude::{LexToken, LexTokenKind},
    util::LexerTokenKindExt,
};
use utils::span::Span;

use crate::error::{Error, ErrorKind};

use super::{
    Reduce, Symbol,
    terminals::{LiteralToken, NumToken},
};

#[derive(Debug, PartialEq)]
pub enum ExprToken {
    LitExpr(LiteralToken),
    BinExpr(BinExprToken),
    UnExpr(UnEpxrToken),
    ParenExpr(ParenExprToken),
}

impl Reduce for ExprToken {
    fn reduce(&self) -> Self {
        match self {
            Self::LitExpr(token) => token.reduce(),
            Self::BinExpr(token) => token.reduce(),
            Self::UnExpr(token) => token.reduce(),
            Self::ParenExpr(token) => token.reduce(),
        }
    }
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

    pub(crate) fn set_span(&mut self, span: Span) {
        match self {
            Self::LitExpr(a) => a.set_span(span),
            Self::BinExpr(a) => a.set_span(span),
            Self::UnExpr(a) => a.set_span(span),
            Self::ParenExpr(a) => a.set_span(span),
        }
    }
}

impl<'s> Symbol<'s> for ExprToken {
    fn parse(parser: &mut super::Parser<'s>) -> crate::error::Result<Self> {
        parse_expr_helper(parser, 0)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
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
pub struct BinExprToken {
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

    pub(crate) fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

impl Reduce for BinExprToken {
    fn reduce(&self) -> ExprToken {
        let lhs = self.lhs.reduce();
        let rhs = self.rhs.reduce();
        let op = self.op;

        match (lhs, rhs) {
            (
                ExprToken::LitExpr(LiteralToken::Num(lhs_num)),
                ExprToken::LitExpr(LiteralToken::Num(rhs_num)),
            ) => {
                let value = match op {
                    BinOp::Add => lhs_num.value() + rhs_num.value(),
                    BinOp::Subtract => lhs_num.value() - rhs_num.value(),
                    BinOp::Multiply => lhs_num.value() * rhs_num.value(),
                    BinOp::Divide => lhs_num.value() / rhs_num.value(),
                };

                ExprToken::LitExpr(LiteralToken::Num(NumToken::new(value, self.span)))
            }
            (lhs, rhs) => ExprToken::BinExpr(BinExprToken::new(lhs, rhs, op, self.span)),
        }
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

#[derive(Debug, PartialEq, Clone, Copy)]
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

impl Reduce for UnEpxrToken {
    fn reduce(&self) -> ExprToken {
        let expr = self.expr.reduce();
        let op = self.op;

        match expr {
            ExprToken::LitExpr(LiteralToken::Num(token)) => {
                let value = match op {
                    UnOp::Negation => NumToken::new(token.value(), self.span),
                };

                ExprToken::LitExpr(LiteralToken::Num(value))
            }
            rest => ExprToken::UnExpr(UnEpxrToken::new(rest, op, self.span)),
        }
    }
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

    pub(crate) fn set_span(&mut self, span: Span) {
        self.span = span;
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
pub struct ParenExprToken {
    inner: Box<ExprToken>,
    span: Span,
}

impl Reduce for ParenExprToken {
    fn reduce(&self) -> ExprToken {
        let mut expr = self.inner.reduce();
        expr.set_span(self.span);

        expr
    }
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

    pub(crate) fn set_span(&mut self, span: Span) {
        self.span = span;
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
        let next_token = parser.peek_lexed_token()?;

        if next_token.kind() == LexTokenKind::Eof || !next_token.kind().is_infix_op() {
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
