use std::fmt::{Display, Error, Formatter};

use crate::{bytecode_vm::types::CompilerError, domain::ExecutionError, lexer::types::Token};

#[derive(Debug, PartialEq, Clone)]
pub enum MemphisError {
    Parser(ParserError),
    Execution(ExecutionError),
    Compiler(CompilerError),
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
        match self {
            MemphisError::Parser(e) => write!(f, "{}", e),
            MemphisError::Execution(e) => write!(f, "{}", e),
            MemphisError::Compiler(e) => write!(f, "{}", e),
        }
    }
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            LexerError::UnexpectedCharacter(c) => write!(f, "Unexpected character: {}", c),
            LexerError::InvalidToken(t) => write!(f, "Invalid token: {}", t),
            LexerError::InternalError(msg) => write!(f, "Internal Error: {}", msg),
        }
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ParserError::ExpectedToken(expected, found) => {
                write!(f, "Expected token {:?}, found {:?}", expected, found)
            }
            ParserError::UnexpectedToken(token) => {
                write!(f, "Unexpected token \"{:?}\"", token)
            }
            ParserError::ExpectedException(s) => {
                write!(f, "Expected exception: \"{:?}\" is not defined", s)
            }
            ParserError::SyntaxError => {
                write!(f, "SyntaxError")
            }
        }
    }
}
