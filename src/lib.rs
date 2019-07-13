extern crate combine;

mod error;
mod models;
mod parser;

pub use self::error::*;
pub use self::models::*;
use self::parser::toc_parser;

use combine::stream::state::State;
use combine::Parser;

pub fn parse(input: &str) -> Result<TOC, TOCError> {
    toc_parser()
        .easy_parse(State::new(input))
        .map_err(TOCError::new)
        .map(|result| result.0)
}
