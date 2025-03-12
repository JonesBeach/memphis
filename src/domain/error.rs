use std::fmt::{Debug, Display, Error, Formatter};

use super::DebugCallStack;

#[derive(PartialEq, Clone)]
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
        write!(f, "{}", self.execution_error_kind)
    }
}

impl Debug for ExecutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
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

        if let Ok(literal) = self.try_into() {
            // Always match `Exception`
            handled_exception_types.contains(&ExceptionLiteral::Exception)
                || handled_exception_types.contains(&literal)
        } else {
            false
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

impl Display for ExecutionErrorKind {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
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
pub enum ExceptionLiteral {
    Exception,
    ZeroDivisionError,
    IOError,
    ImportError,
    StopIteration,
    TypeError,
    AttributeError,
    NameError,
    Custom(String),
}

impl From<String> for ExceptionLiteral {
    fn from(value: String) -> Self {
        match value.as_str() {
            "ZeroDivisionError" => Self::ZeroDivisionError,
            "Exception" => Self::Exception,
            "IOError" => Self::IOError,
            "ImportError" => Self::ImportError,
            "StopIteration" => Self::StopIteration,
            "TypeError" => Self::TypeError,
            "AttributeError" => Self::AttributeError,
            "NameError" => Self::NameError,
            // TODO we don't handle Self::Custom in the interpreter yet
            _ => Self::Custom(value.to_owned()),
        }
    }
}

impl TryFrom<&ExecutionError> for ExceptionLiteral {
    type Error = ();

    fn try_from(value: &ExecutionError) -> Result<Self, Self::Error> {
        match value.execution_error_kind {
            ExecutionErrorKind::DivisionByZero(..) => Ok(Self::ZeroDivisionError),
            ExecutionErrorKind::ImportError(..) => Ok(Self::ImportError),
            ExecutionErrorKind::TypeError(..) => Ok(Self::TypeError),
            ExecutionErrorKind::AttributeError(..) => Ok(Self::AttributeError),
            ExecutionErrorKind::NameError(..) => Ok(Self::NameError),
            _ => Err(()),
        }
    }
}

pub mod test_utils {
    use super::*;

    pub fn assert_error_kind(e: &ExecutionError, expected_kind: ExecutionErrorKind) {
        assert_eq!(e.execution_error_kind, expected_kind);
    }

    pub fn assert_type_error(e: &ExecutionError, expected_message: &str) {
        match &e.execution_error_kind {
            ExecutionErrorKind::TypeError(Some(msg)) => {
                assert_eq!(msg, expected_message, "Unexpected TypeError message");
            }
            _ => panic!("Expected a TypeError, but got {:?}", e.execution_error_kind),
        }
    }

    pub fn assert_type_error_optional_message(e: &ExecutionError, expected_message: Option<&str>) {
        match &e.execution_error_kind {
            ExecutionErrorKind::TypeError(msg) => {
                assert_eq!(
                    msg.as_deref(),
                    expected_message,
                    "Unexpected TypeError message"
                );
            }
            _ => panic!("Expected a TypeError, but got {:?}", e.execution_error_kind),
        }
    }

    pub fn assert_name_error(e: &ExecutionError, expected_name: &str) {
        match &e.execution_error_kind {
            ExecutionErrorKind::NameError(name) => {
                assert_eq!(name, expected_name, "Unexpected NameError message");
            }
            _ => panic!("Expected a NameError, but got {:?}", e.execution_error_kind),
        }
    }

    pub fn assert_key_error(e: &ExecutionError, expected_key: &str) {
        match &e.execution_error_kind {
            ExecutionErrorKind::KeyError(key) => {
                assert_eq!(key, expected_key, "Unexpected KeyError message");
            }
            _ => panic!("Expected a KeyError, but got {:?}", e.execution_error_kind),
        }
    }

    pub fn assert_value_error(e: &ExecutionError, expected_message: &str) {
        match &e.execution_error_kind {
            ExecutionErrorKind::ValueError(message) => {
                assert_eq!(message, expected_message, "Unexpected ValueError message");
            }
            _ => panic!(
                "Expected a ValueError, but got {:?}",
                e.execution_error_kind
            ),
        }
    }

    pub fn assert_attribute_error(e: &ExecutionError, expected_object: &str, expected_attr: &str) {
        match &e.execution_error_kind {
            ExecutionErrorKind::AttributeError(object, attr) => {
                assert_eq!(object, expected_object, "Unexpected AttributeError object");
                assert_eq!(attr, expected_attr, "Unexpected AttributeError attr");
            }
            _ => panic!(
                "Expected a AttributeError, but got {:?}",
                e.execution_error_kind
            ),
        }
    }
}
