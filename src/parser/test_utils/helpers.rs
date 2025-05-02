use crate::{domain::Source, parser::types::Ast};

use super::ParseContext;

pub fn init(text: &str) -> ParseContext {
    ParseContext::new(Source::from_text(text))
}

pub fn parse_all(input: &str) -> Ast {
    init(input)
        .parse_all()
        .expect("Failed to parse all statements!")
}

macro_rules! expect_error {
    ($input:expr, $pattern:ident) => {
        match init($input).parse_oneshot::<$pattern>() {
            Ok(_) => panic!("Expected a ParserError!"),
            Err(e) => e,
        }
    };
}

macro_rules! parse {
    ($input:expr, $pattern:ident) => {
        match init($input).parse_oneshot::<$pattern>() {
            Err(e) => panic!("Parser error: {:?}", e),
            Ok(ast) => ast,
        }
    };
}

pub(crate) use expect_error;
pub(crate) use parse;
