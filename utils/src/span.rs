use std::ops::{Add, AddAssign};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    len: usize,
}

impl Span {
    pub fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

impl Add for Span {
    type Output = Span;
    fn add(self, rhs: Self) -> Self::Output {
        assert!(self.start + self.len <= rhs.start);

        Span::new(self.start, (rhs.len + rhs.start - 1) - self.start + 1)
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}

#[cfg(feature = "miette")]
impl From<Span> for miette::SourceSpan {
    fn from(value: Span) -> Self {
        miette::SourceSpan::new(value.start.into(), value.len)
    }
}
