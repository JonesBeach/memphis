use std::fmt::{Display, Error, Formatter};

use crate::{
    bytecode_vm::types::CompilerError,
    core::{log, LogLevel},
    domain::DebugCallStack,
    lexer::types::Token,
    parser::types::ExceptionLiteral,
};

#[derive(Debug, PartialEq, Clone)]
pub enum MemphisError {
    Parser(ParserError),
    Execution(ExecutionError),
    Compiler(CompilerError),
}

#[derive(Debug, PartialEq, Clone)]
pub enum LexerError {
    UnexpectedCharacter(char),
}

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    ExpectedToken(Token, Token),
    UnexpectedToken(Token),
    ExpectedException(String),
    SyntaxError,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExecutionError {
    pub debug_call_stack: DebugCallStack,
    pub execution_error_kind: ExecutionErrorKind,
}

impl ExecutionError {
    pub fn new(debug_call_stack: DebugCallStack, execution_error_kind: ExecutionErrorKind) -> Self {
        Self {
            debug_call_stack,
            execution_error_kind,
        }
    }
}

impl Display for ExecutionError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.debug_call_stack)?;
        match &self.execution_error_kind {
            ExecutionErrorKind::RuntimeError => write!(f, "RuntimeError"),
            ExecutionErrorKind::ImportError(name) => {
                write!(f, "ImportError: No module named {}", name)
            }
            ExecutionErrorKind::TypeError(message) => match message {
                Some(message) => write!(f, "TypeError: {}", message),
                None => write!(f, "TypeError"),
            },
            ExecutionErrorKind::KeyError(key) => write!(f, "KeyError: '{}'", key),
            ExecutionErrorKind::ValueError(message) => write!(f, "ValueError: '{}'", message),
            ExecutionErrorKind::NameError(name) => {
                write!(f, "NameError: name '{}' is not defined", name)
            }
            ExecutionErrorKind::AttributeError(class_name, field) => {
                write!(
                    f,
                    "AttributeError: '{}' object has no attribute '{}'",
                    class_name, field
                )
            }
            ExecutionErrorKind::DivisionByZero(message) => {
                write!(f, "ZeroDivisionError: {}", message)
            }
            ExecutionErrorKind::StopIteration => {
                write!(f, "StopIteration")
            }
            ExecutionErrorKind::AssertionError => {
                write!(f, "AssertionError")
            }
            ExecutionErrorKind::MissingContextManagerProtocol => {
                write!(f, "object does not support the context manager protocol")
            }
            ExecutionErrorKind::SyntaxError => {
                todo!()
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecutionErrorKind {
    RuntimeError,
    ImportError(String),
    TypeError(Option<String>),
    KeyError(String),
    ValueError(String),
    NameError(String),
    AttributeError(String, String),
    DivisionByZero(String),
    StopIteration,
    AssertionError,
    MissingContextManagerProtocol,
    // TODO where this is used should really be moved into the parser but we currently don't have
    // enough scope context during that stage to do so.
    SyntaxError,
}

impl ExecutionError {
    /// When an `InterpreterError` is thrown inside a try-except block, this method is used to
    /// determine whether a given except clause should be run. It does this by mapping
    /// `InterpreterError` variants (from the interpreter) to `ExceptionLiteral` variants from the
    /// parser.
    pub fn matches_except_clause(&self, handled_exception_types: &[ExceptionLiteral]) -> bool {
        if handled_exception_types.is_empty() {
            return true;
        }

        for clause_literal in handled_exception_types {
            // Any types match against Exception, which is considered the parent literal
            if clause_literal == &ExceptionLiteral::Exception {
                return true;
            }

            let found_literal: Result<ExceptionLiteral, _> = self.clone().try_into();

            match &found_literal {
                Ok(found_literal) => {
                    // if we found a matching type, we're good to return, but not finding a match
                    // means we should keep looking.
                    if found_literal == clause_literal {
                        return true;
                    }
                }
                Err(_) => log(LogLevel::Warn, || {
                    format!("Unmatched exception type!\n{}", self)
                }),
            }
        }

        false
    }
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
