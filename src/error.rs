use combine::easy::{Error as EasyError, Errors};
use combine::stream::state::SourcePosition;
use std::error::Error as StdError;
use std::fmt;
use std::fmt::Display;

type CombineError<'a> = Errors<char, &'a str, SourcePosition>;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct TOCError {
    msg: String,
}

impl TOCError {
    pub(crate) fn new(error: CombineError) -> Self {
        Self {
            msg: format!("{}", CombineParseError::new(error)),
        }
    }
}

impl Display for TOCError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl StdError for TOCError {
    fn description(&self) -> &str {
        "tocchri error"
    }
}

#[derive(Debug, PartialEq)]
pub struct CombineParseError<'a> {
    error: CombineError<'a>,
}

impl<'a> CombineParseError<'a> {
    pub(crate) fn new(error: CombineError<'a>) -> Self {
        Self { error }
    }
}

impl<'a> Display for CombineParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let SourcePosition { line, column } = self.error.position;
        writeln!(f, "TOC parse error at line {}, column {}", line, column)?;
        EasyError::fmt_errors(self.error.errors.as_ref(), f)
    }
}
impl<'a> From<CombineError<'a>> for CombineParseError<'a> {
    fn from(error: CombineError<'a>) -> Self {
        Self { error }
    }
}

impl<'a> From<CombineError<'a>> for TOCError {
    fn from(error: CombineError<'a>) -> Self {
        Self::new(error)
    }
}
