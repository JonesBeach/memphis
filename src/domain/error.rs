use std::fmt::{Debug, Display, Error, Formatter};

use crate::{bytecode_vm::VmValue, core::Voidable, treewalk::TreewalkValue};

use super::{DebugCallStack, MemphisValue};

#[derive(PartialEq, Clone)]
pub struct RuntimeError {
    pub debug_call_stack: DebugCallStack,
    pub execution_error: ExecutionError,
}

impl RuntimeError {
    pub fn new(debug_call_stack: DebugCallStack, execution_error: ExecutionError) -> Self {
        Self {
            debug_call_stack,
            execution_error,
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.debug_call_stack)?;
        write!(f, "{}", self.execution_error)
    }
}

impl Debug for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self, f)
    }
}

impl RuntimeError {
    /// When an `InterpreterError` is thrown inside a try-except block, this method is used to
    /// determine whether a given except clause should be run. It does this by mapping
    /// `InterpreterError` variants (from the interpreter) to `ExceptionLiteral` variants from the
    /// parser.
    pub fn matches_except_clause(&self, handled_exception_types: &[ExceptionLiteral]) -> bool {
        if handled_exception_types.is_empty() {
            return true;
        }

        if let Ok(literal) = (&self.execution_error).try_into() {
            // Always match `Exception`
            handled_exception_types.contains(&ExceptionLiteral::Exception)
                || handled_exception_types.contains(&literal)
        } else {
            false
        }
    }
}

/// We need a runtime value which can be stored in a StopIteration.
#[allow(clippy::large_enum_variant)]
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    None,
    Treewalk(TreewalkValue),
    Vm(VmValue),
}

impl RuntimeValue {
    pub fn unwrap_treewalk(self) -> TreewalkValue {
        match self {
            RuntimeValue::Treewalk(v) => v,
            _ => panic!("Expected TreewalkValue"),
        }
    }

    pub fn unwrap_vm(self) -> VmValue {
        match self {
            RuntimeValue::Vm(v) => v,
            _ => panic!("Expected VmValue"),
        }
    }
}

impl From<RuntimeValue> for MemphisValue {
    /// NOTE: This is a lossy conversion.
    fn from(value: RuntimeValue) -> Self {
        match value {
            RuntimeValue::None => MemphisValue::None,
            RuntimeValue::Treewalk(v) => MemphisValue::from(v),
            RuntimeValue::Vm(v) => MemphisValue::from(v),
        }
    }
}

impl PartialEq for RuntimeValue {
    /// NOTE: This uses a lossy conversion, and should only be used in the presentation/test layer.
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::None => MemphisValue::None == MemphisValue::from(other.clone()),
            Self::Treewalk(v) => MemphisValue::from(v.clone()) == MemphisValue::from(other.clone()),
            Self::Vm(v) => MemphisValue::from(v.clone()) == MemphisValue::from(other.clone()),
        }
    }
}

impl Voidable for RuntimeValue {
    fn is_none(&self) -> bool {
        match self {
            Self::None => true,
            Self::Treewalk(v) => v.is_none(),
            Self::Vm(v) => v.is_none(),
        }
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => Display::fmt(&MemphisValue::None, f),
            Self::Treewalk(v) => Display::fmt(&v, f),
            Self::Vm(v) => Display::fmt(&v, f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExecutionError {
    RuntimeError(Option<String>),
    ImportError(String),
    TypeError(Option<String>),
    LookupError(String),
    KeyError(String),
    ValueError(String),
    NameError(String),
    AttributeError(String, String),
    DivisionByZero(String),
    StopIteration(Box<RuntimeValue>),
    AssertionError,
    MissingContextManagerProtocol,
    // TODO where this is used should really be moved into the parser but we currently don't have
    // enough scope context during that stage to do so.
    SyntaxError,
}

impl ExecutionError {
    pub fn type_error(msg: impl Into<String>) -> Self {
        Self::TypeError(Some(msg.into()))
    }

    pub fn type_error_empty() -> Self {
        Self::TypeError(None)
    }

    pub fn name_error(name: impl Into<String>) -> Self {
        Self::NameError(name.into())
    }

    pub fn value_error(msg: impl Into<String>) -> Self {
        Self::ValueError(msg.into())
    }

    pub fn lookup_error(msg: impl Into<String>) -> Self {
        Self::LookupError(msg.into())
    }

    pub fn key_error(msg: impl Into<String>) -> Self {
        Self::KeyError(msg.into())
    }

    pub fn import_error(msg: impl Into<String>) -> Self {
        Self::ImportError(msg.into())
    }

    pub fn attribute_error(object_type: impl Into<String>, attr: impl Into<String>) -> Self {
        Self::AttributeError(object_type.into(), attr.into())
    }

    pub fn div_by_zero_error(msg: impl Into<String>) -> Self {
        Self::DivisionByZero(msg.into())
    }

    pub fn runtime_error() -> Self {
        Self::RuntimeError(None)
    }

    pub fn runtime_error_with(msg: impl Into<String>) -> Self {
        Self::RuntimeError(Some(msg.into()))
    }

    pub fn assertion_error() -> Self {
        Self::AssertionError
    }

    pub fn stop_iteration() -> Self {
        Self::StopIteration(Box::new(RuntimeValue::None))
    }

    pub fn stop_iteration_with(value: impl Into<RuntimeValue>) -> Self {
        Self::StopIteration(Box::new(value.into()))
    }

    pub fn unknown_encoding(encoding: impl Into<String>) -> Self {
        Self::lookup_error(format!("unknown encoding: {}", encoding.into()))
    }
}

impl Display for ExecutionError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            ExecutionError::RuntimeError(msg) => match msg {
                Some(msg) => write!(f, "RuntimeError: {msg}"),
                None => write!(f, "RuntimeError"),
            },
            ExecutionError::ImportError(msg) => {
                write!(f, "ImportError: {msg}")
            }
            ExecutionError::TypeError(message) => match message {
                Some(message) => write!(f, "TypeError: {message}"),
                None => write!(f, "TypeError"),
            },
            ExecutionError::LookupError(message) => write!(f, "LookupError: '{message}'"),
            ExecutionError::KeyError(key) => write!(f, "KeyError: '{key}'"),
            ExecutionError::ValueError(message) => write!(f, "ValueError: '{message}'"),
            ExecutionError::NameError(name) => {
                write!(f, "NameError: name '{name}' is not defined")
            }
            ExecutionError::AttributeError(class_name, field) => {
                write!(
                    f,
                    "AttributeError: '{class_name}' object has no attribute '{field}'"
                )
            }
            ExecutionError::DivisionByZero(message) => {
                write!(f, "ZeroDivisionError: {message}")
            }
            ExecutionError::StopIteration(value) => {
                if value.is_none() {
                    write!(f, "StopIteration")
                } else {
                    write!(f, "StopIteration: {value}")
                }
            }
            ExecutionError::AssertionError => {
                write!(f, "AssertionError")
            }
            ExecutionError::MissingContextManagerProtocol => {
                write!(f, "object does not support the context manager protocol")
            }
            ExecutionError::SyntaxError => {
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
        match value {
            ExecutionError::DivisionByZero(..) => Ok(Self::ZeroDivisionError),
            ExecutionError::ImportError(..) => Ok(Self::ImportError),
            ExecutionError::TypeError(..) => Ok(Self::TypeError),
            ExecutionError::AttributeError(..) => Ok(Self::AttributeError),
            ExecutionError::NameError(..) => Ok(Self::NameError),
            ExecutionError::StopIteration(..) => Ok(Self::StopIteration),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    macro_rules! assert_error_eq {
        ($error:expr, $expected_kind:expr) => {
            assert_eq!($error.execution_error, $expected_kind);
        };
    }

    macro_rules! assert_stop_iteration {
        ($error:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::StopIteration(value) => {
                    assert!($crate::core::Voidable::is_none(&**value))
                }
                _ => panic!(
                    "Expected a StopIteration error, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_div_by_zero_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::DivisionByZero(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected DivisionByZero message");
                }
                _ => panic!(
                    "Expected a DivisionByZero error with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_lookup_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::LookupError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected LookupError message");
                }
                _ => panic!(
                    "Expected a LookupError error with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_runtime_error {
        ($error:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::RuntimeError(msg) => {
                    assert!(msg.is_none(), "Unexpected RuntimeError message");
                }
                _ => panic!(
                    "Expected a RuntimeError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::RuntimeError(Some(msg)) => {
                    assert_eq!(msg, $expected_message, "Unexpected RuntimeError message");
                }
                _ => panic!(
                    "Expected a RuntimeError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    // Use this when the error message may vary across platforms (e.g., os error 48 vs 98)
    macro_rules! assert_runtime_error_contains {
        ($error:expr, $expected_substr:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::RuntimeError(Some(msg)) => {
                    assert!(
                        msg.contains($expected_substr),
                        "Expected RuntimeError message to contain {:?}, but got {:?}",
                        $expected_substr,
                        msg
                    );
                }
                _ => panic!(
                    "Expected a RuntimeError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_type_error {
        ($error:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::TypeError(msg) => {
                    assert!(msg.is_none(), "Unexpected TypeError message");
                }
                _ => panic!(
                    "Expected a TypeError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::TypeError(Some(msg)) => {
                    assert_eq!(msg, $expected_message, "Unexpected TypeError message");
                }
                _ => panic!(
                    "Expected a TypeError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_import_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::ImportError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected ImportError message");
                }
                _ => panic!(
                    "Expected an ImportError with a module name, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_name_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::NameError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected NameError message");
                }
                _ => panic!(
                    "Expected a NameError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_key_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::KeyError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected KeyError message");
                }
                _ => panic!(
                    "Expected a KeyError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_value_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::ValueError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected ValueError message");
                }
                _ => panic!(
                    "Expected a ValueError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    macro_rules! assert_attribute_error {
        ($error:expr, $expected_obj:expr, $expected_attr:expr) => {{
            match &$error.execution_error {
                $crate::domain::ExecutionError::AttributeError(object, attr) => {
                    assert_eq!(object, $expected_obj, "Unexpected AttributeError object");
                    assert_eq!(attr, $expected_attr, "Unexpected AttributeError attr");
                }
                _ => panic!(
                    "Expected a AttributeError with message, but got: {:?}",
                    &$error.execution_error
                ),
            }
        }};
    }

    pub(crate) use assert_attribute_error;
    pub(crate) use assert_div_by_zero_error;
    pub(crate) use assert_error_eq;
    pub(crate) use assert_import_error;
    pub(crate) use assert_key_error;
    pub(crate) use assert_lookup_error;
    pub(crate) use assert_name_error;
    pub(crate) use assert_runtime_error;
    pub(crate) use assert_runtime_error_contains;
    pub(crate) use assert_stop_iteration;
    pub(crate) use assert_type_error;
    pub(crate) use assert_value_error;
}
