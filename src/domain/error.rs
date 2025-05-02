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

#[cfg(test)]
pub mod test_utils {
    macro_rules! assert_error_eq {
        ($error:expr, $expected_kind:expr) => {
            assert_eq!($error.execution_error_kind, $expected_kind);
        };
    }

    macro_rules! assert_div_by_zero_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error_kind {
                $crate::domain::ExecutionErrorKind::DivisionByZero(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected DivisionByZero message");
                }
                _ => panic!(
                    "Expected a DivisionByZero error with message, but got: {:?}",
                    &$error.execution_error_kind
                ),
            }
        }};
    }

    macro_rules! assert_type_error {
        ($error:expr) => {{
            match &$error.execution_error_kind {
                $crate::domain::ExecutionErrorKind::TypeError(msg) => {
                    assert!(msg.is_none(), "Unexpected TypeError message");
                }
                _ => panic!(
                    "Expected a TypeError with message, but got: {:?}",
                    &$error.execution_error_kind
                ),
            }
        }};
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error_kind {
                $crate::domain::ExecutionErrorKind::TypeError(Some(msg)) => {
                    assert_eq!(msg, $expected_message, "Unexpected TypeError message");
                }
                _ => panic!(
                    "Expected a TypeError with message, but got: {:?}",
                    &$error.execution_error_kind
                ),
            }
        }};
    }

    macro_rules! assert_name_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error_kind {
                $crate::domain::ExecutionErrorKind::NameError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected NameError message");
                }
                _ => panic!(
                    "Expected a NameError with message, but got: {:?}",
                    &$error.execution_error_kind
                ),
            }
        }};
    }

    macro_rules! assert_key_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error_kind {
                $crate::domain::ExecutionErrorKind::KeyError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected KeyError message");
                }
                _ => panic!(
                    "Expected a KeyError with message, but got: {:?}",
                    &$error.execution_error_kind
                ),
            }
        }};
    }

    macro_rules! assert_value_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error_kind {
                $crate::domain::ExecutionErrorKind::ValueError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected ValueError message");
                }
                _ => panic!(
                    "Expected a ValueError with message, but got: {:?}",
                    &$error.execution_error_kind
                ),
            }
        }};
    }

    macro_rules! assert_attribute_error {
        ($error:expr, $expected_obj:expr, $expected_attr:expr) => {{
            match &$error.execution_error_kind {
                $crate::domain::ExecutionErrorKind::AttributeError(object, attr) => {
                    assert_eq!(object, $expected_obj, "Unexpected AttributeError object");
                    assert_eq!(attr, $expected_attr, "Unexpected AttributeError attr");
                }
                _ => panic!(
                    "Expected a AttributeError with message, but got: {:?}",
                    &$error.execution_error_kind
                ),
            }
        }};
    }

    pub(crate) use assert_attribute_error;
    pub(crate) use assert_div_by_zero_error;
    pub(crate) use assert_error_eq;
    pub(crate) use assert_key_error;
    pub(crate) use assert_name_error;
    pub(crate) use assert_type_error;
    pub(crate) use assert_value_error;
}
