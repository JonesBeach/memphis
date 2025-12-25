use crate::domain::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum ExceptionKind {
    RuntimeError,
    ImportError,
    TypeError,
    LookupError,
    KeyError,
    ValueError,
    NameError,
    AttributeError,
    DivisionByZero,
    StopIteration,
    AssertionError,
    SyntaxError,
}

impl ExceptionKind {
    pub fn get_type(&self) -> Type {
        match self {
            Self::TypeError => Type::TypeError,
            Self::StopIteration => Type::StopIteration,
            Self::DivisionByZero => Type::ZeroDivisionError,
            Self::RuntimeError => Type::RuntimeError,
            Self::ImportError => Type::ImportError,
            Self::LookupError => Type::LookupError,
            Self::KeyError => Type::KeyError,
            Self::ValueError => Type::ValueError,
            Self::NameError => Type::NameError,
            Self::AttributeError => Type::AttributeError,
            Self::AssertionError => Type::AssertionError,
            Self::SyntaxError => Type::SyntaxError,
        }
    }
}

#[cfg(test)]
pub mod test_utils {
    macro_rules! assert_stop_iteration {
        ($error:expr) => {{
            match &$error {
                $crate::domain::ExecutionError::StopIteration(value) => {
                    assert!($crate::core::Voidable::is_none(&**value))
                }
                _ => panic!("Expected a StopIteration error, but got: {:?}", &$error),
            }
        }};
    }

    // Use this when the error message may vary across platforms (e.g., os error 48 vs 98)
    macro_rules! assert_runtime_error_contains {
        ($error:expr, $expected_substr:expr) => {{
            match &$error {
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
                    &$error
                ),
            }
        }};
    }

    macro_rules! assert_type_error {
        ($error:expr) => {{
            match &$error {
                $crate::domain::ExecutionError::TypeError(msg) => {
                    assert!(msg.is_none(), "Unexpected TypeError message");
                }
                _ => panic!("Expected a TypeError with message, but got: {:?}", &$error),
            }
        }};
        ($error:expr, $expected_message:expr) => {{
            match &$error {
                $crate::domain::ExecutionError::TypeError(Some(msg)) => {
                    assert_eq!(msg, $expected_message, "Unexpected TypeError message");
                }
                _ => panic!("Expected a TypeError with message, but got: {:?}", &$error),
            }
        }};
    }

    macro_rules! assert_import_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error {
                $crate::domain::ExecutionError::ImportError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected ImportError message");
                }
                _ => panic!(
                    "Expected an ImportError with a module name, but got: {:?}",
                    &$error
                ),
            }
        }};
    }

    macro_rules! assert_name_error {
        ($error:expr, $expected_message:expr) => {{
            match &$error {
                $crate::domain::ExecutionError::NameError(msg) => {
                    assert_eq!(msg, $expected_message, "Unexpected NameError message");
                }
                _ => panic!("Expected a NameError with message, but got: {:?}", &$error),
            }
        }};
    }

    pub(crate) use assert_import_error;
    pub(crate) use assert_name_error;
    pub(crate) use assert_runtime_error_contains;
    pub(crate) use assert_stop_iteration;
    pub(crate) use assert_type_error;
}
