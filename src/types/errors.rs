use std::fmt::{Display, Error, Formatter};

use crate::bytecode_vm::types::{CompilerError, VmError};
use crate::core::{log, LogLevel};
use crate::lexer::types::Token;
use crate::{
    parser::types::ExceptionLiteral,
    treewalk::{types::ExprResult, CallStack},
};

#[derive(Debug, PartialEq, Clone)]
pub enum MemphisError {
    Parser(ParserError),
    Interpreter(InterpreterError),
    Compiler(CompilerError),
    Vm(VmError),
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
pub enum InterpreterError {
    Exception(CallStack),
    TypeError(Option<String>, CallStack),
    KeyError(String, CallStack),
    ValueError(String, CallStack),
    NameError(String, CallStack),
    AttributeError(String, String, CallStack),
    FunctionNotFound(String, CallStack),
    MethodNotFound(String, CallStack),
    ClassNotFound(String, CallStack),
    ModuleNotFound(String, CallStack),
    DivisionByZero(String, CallStack),
    ExpectedVariable(CallStack),
    ExpectedString(CallStack),
    ExpectedInteger(CallStack),
    ExpectedList(CallStack),
    ExpectedTuple(CallStack),
    ExpectedRange(CallStack),
    ExpectedSet(CallStack),
    ExpectedDict(CallStack),
    ExpectedFloatingPoint(CallStack),
    ExpectedBoolean(CallStack),
    ExpectedObject(CallStack),
    ExpectedClass(CallStack),
    ExpectedFunction(CallStack),
    ExpectedIterable(CallStack),
    ExpectedCoroutine(CallStack),
    WrongNumberOfArguments(usize, usize, CallStack),
    StopIteration(CallStack),
    AssertionError(CallStack),
    MissingContextManagerProtocol(CallStack),
    // TODO where this is used should really be moved into the parser but we currently don't have
    // enough scope context during that stage to do so.
    SyntaxError(CallStack),
    RuntimeError,
    EncounteredReturn(ExprResult),
    EncounteredRaise,
    EncounteredAwait,
    EncounteredSleep,
    EncounteredBreak,
    EncounteredContinue,
}

impl InterpreterError {
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
            MemphisError::Interpreter(e) => write!(f, "{}", e),
            MemphisError::Vm(_) => unimplemented!(),
            MemphisError::Compiler(_) => unimplemented!(),
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

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            InterpreterError::Exception(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Exception!")
            }
            InterpreterError::TypeError(message, call_stack) => {
                write!(f, "{}", call_stack)?;
                match message {
                    Some(message) => write!(f, "TypeError: {}", message),
                    None => write!(f, "TypeError"),
                }
            }
            InterpreterError::KeyError(key, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "KeyError: '{}'", key)
            }
            InterpreterError::ValueError(message, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "ValueError: '{}'", message)
            }
            InterpreterError::NameError(name, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "NameError: name '{}' is not defined", name)
            }
            InterpreterError::AttributeError(class_name, field, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(
                    f,
                    "AttributeError: '{}' object has no attribute '{}'",
                    class_name, field
                )
            }
            InterpreterError::DivisionByZero(message, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "ZeroDivisionError: {}", message)
            }
            InterpreterError::ClassNotFound(name, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Class \"{}\" not found", name)
            }
            InterpreterError::MethodNotFound(name, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Method \"{}\" not found", name)
            }
            InterpreterError::ModuleNotFound(name, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Module \"{}\" not found", name)
            }
            InterpreterError::FunctionNotFound(name, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Function \"{}\" not found", name)
            }
            InterpreterError::ExpectedVariable(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected variable")
            }
            InterpreterError::ExpectedString(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected string")
            }
            InterpreterError::ExpectedInteger(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected integer")
            }
            InterpreterError::ExpectedList(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected list")
            }
            InterpreterError::ExpectedTuple(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected tuple")
            }
            InterpreterError::ExpectedRange(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected range")
            }
            InterpreterError::ExpectedSet(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected set")
            }
            InterpreterError::ExpectedDict(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected dict")
            }
            InterpreterError::ExpectedFloatingPoint(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected floating point")
            }
            InterpreterError::ExpectedBoolean(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected boolean")
            }
            InterpreterError::ExpectedObject(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected object")
            }
            InterpreterError::ExpectedClass(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected class")
            }
            InterpreterError::ExpectedFunction(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected function")
            }
            InterpreterError::ExpectedIterable(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected iterable")
            }
            InterpreterError::ExpectedCoroutine(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected coroutine")
            }
            InterpreterError::WrongNumberOfArguments(expected, found, call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(
                    f,
                    "Wrong number of arguments, expected {} found {}",
                    expected, found
                )
            }
            InterpreterError::StopIteration(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "Expected object")
            }
            InterpreterError::AssertionError(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "AssertionError")
            }
            InterpreterError::MissingContextManagerProtocol(call_stack) => {
                write!(f, "{}", call_stack)?;
                write!(f, "object does not support the context manager protocol")
            }
            InterpreterError::SyntaxError(call_stack) => {
                write!(f, "{}", call_stack)
            }
            InterpreterError::RuntimeError => {
                write!(f, "RuntimeError")
            }
            InterpreterError::EncounteredReturn(_)
            | InterpreterError::EncounteredRaise
            | InterpreterError::EncounteredAwait
            | InterpreterError::EncounteredSleep
            | InterpreterError::EncounteredBreak
            | InterpreterError::EncounteredContinue => {
                unreachable!()
            }
        }
    }
}
