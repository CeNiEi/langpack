use crate::Parser;
use crate::error::Result;

pub mod non_terminals;
pub mod terminals;

pub trait Symbol<'s>: Sized + 's {
    fn parse(parser: &mut Parser<'s>) -> Result<Self>;
}
