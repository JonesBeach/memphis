use std::fmt::{Display, Error, Formatter};

use crate::lexer::Token;

pub type MemphisResult<T> = Result<T, MemphisError>;

#[derive(Debug, PartialEq, Clone)]
pub struct MemphisError(String);

impl MemphisError {
    pub fn new(msg: String) -> Self {
        Self(msg)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexerError {
    UnexpectedCharacter(char),
    InternalError(String),
    InvalidToken(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    ExpectedToken(Token, Token),
    UnexpectedToken(Token),
    ExpectedException(String),
    SyntaxError,
}

impl Display for MemphisError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LexerError::UnexpectedCharacter(c) => write!(f, "Unexpected character: {c}"),
            LexerError::InvalidToken(t) => write!(f, "Invalid token: {t}"),
            LexerError::InternalError(msg) => write!(f, "Internal Error: {msg}"),
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ParserError::ExpectedToken(expected, found) => {
                write!(f, "Expected token {expected:?}, found {found:?}")
            }
            ParserError::UnexpectedToken(token) => {
                write!(f, "Unexpected token \"{token:?}\"")
            }
            ParserError::ExpectedException(s) => {
                write!(f, "Expected exception: \"{s:?}\" is not defined")
            }
            ParserError::SyntaxError => {
                write!(f, "SyntaxError")
            }
        }
    }
}
