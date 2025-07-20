#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Span {
    start: usize,
    len: usize,
}

impl Span {
    pub(crate) fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }
}

#[cfg(feature = "miette")]
impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        SourceSpan::new(value.start.into(), value.len)
    }
}
