use crate::Parser;
use crate::error::Result;

mod non_terminals;
mod terminals;

pub trait Symbol<'s>: Sized + 's {
    fn parse(parser: &mut Parser<'s>) -> Result<Self>;
}
