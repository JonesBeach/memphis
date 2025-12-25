macro_rules! assert_type_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::TypeError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected TypeError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected TypeError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::TypeError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected TypeError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected TypeError message"
                        );
                    }
                    other => panic!(
                        "Expected TypeError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected TypeError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_value_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::ValueError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected ValueError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected ValueError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::ValueError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected ValueError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected ValueError message"
                        );
                    }
                    other => panic!(
                        "Expected ValueError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected ValueError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_syntax_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::SyntaxError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected SyntaxError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected SyntaxError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::SyntaxError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected SyntaxError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected SyntaxError message"
                        );
                    }
                    other => panic!(
                        "Expected SyntaxError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected SyntaxError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_assertion_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::SyntaxError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected SyntaxError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected SyntaxError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::SyntaxError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected SyntaxError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected SyntaxError message"
                        );
                    }
                    other => panic!(
                        "Expected SyntaxError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected SyntaxError, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_stop_iteration {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::StopIteration,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected StopIteration with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected StopIteration, got: {:?}", &$exc),
        }
    }}; // TODO we don't check for a message here beacuse StopIteration often includes a full object
}

macro_rules! assert_div_by_zero_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::DivisionByZero,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected DivisionByZero with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected DivisionByZero, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::DivisionByZero,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected DivisionByZero with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected DivisionByZero message"
                        );
                    }
                    other => panic!(
                        "Expected DivisionByZero message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected DivisionByZero, got: {:?}", &$exc),
        }
    }};
}

macro_rules! assert_runtime_error {
    ($exc:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::RuntimeError,
                payload,
            } => {
                assert!(
                    payload.is_empty(),
                    "Expected RuntimeError with no message, got payload: {:?}",
                    payload
                );
            }
            _ => panic!("Expected RuntimeError, got: {:?}", &$exc),
        }
    }};
    ($exc:expr, $expected_message:expr) => {{
        match &$exc {
            $crate::treewalk::types::Exception {
                kind: $crate::domain::ExceptionKind::RuntimeError,
                payload,
            } => {
                assert_eq!(
                    payload.len(),
                    1,
                    "Expected RuntimeError with one argument, got payload: {:?}",
                    payload
                );

                match &payload[0] {
                    $crate::treewalk::TreewalkValue::Str(s) => {
                        assert_eq!(
                            s.as_str(),
                            $expected_message,
                            "Unexpected RuntimeError message"
                        );
                    }
                    other => panic!(
                        "Expected RuntimeError message to be a string, got: {:?}",
                        other
                    ),
                }
            }
            _ => panic!("Expected RuntimeError, got: {:?}", &$exc),
        }
    }};
}

pub(crate) use assert_assertion_error;
pub(crate) use assert_stop_iteration;
pub(crate) use assert_syntax_error;
pub(crate) use assert_type_error;
pub(crate) use assert_value_error;
pub(crate) use assert_div_by_zero_error;
pub(crate) use assert_runtime_error;
