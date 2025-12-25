use std::fmt::{Display, Error, Formatter};

use crate::{bytecode_vm::runtime::Reference, domain::{ExceptionKind, Type}};

#[derive(Debug, Clone, PartialEq)]
pub struct Exception {
    kind: ExceptionKind,
    payload: Vec<Reference>,
}

impl Exception {
    pub fn new(kind: ExceptionKind, payload: Vec<Reference>) -> Self {
        Self { kind, payload }
    }

    fn new_from_str(_kind: ExceptionKind, _msg: impl Into<String>) -> Self {
        todo!();
    }

    fn new_empty(kind: ExceptionKind) -> Self {
        Self::new(kind, vec![])
    }

    pub fn get_type(&self) -> Type {
        self.kind.get_type()
    }

    pub fn runtime_error() -> Self {
        Self::new_empty(ExceptionKind::RuntimeError)
    }

    pub fn runtime_error_with(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::RuntimeError, msg)
    }

    pub fn type_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::TypeError, msg)
    }

    pub fn type_error_must_inherit_base_exception() -> Self {
        Self::new_from_str(
            ExceptionKind::TypeError,
            "catching classes that do not inherit from BaseException is not allowed",
        )
    }

    pub fn import_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::ImportError, msg)
    }

    pub fn syntax_error() -> Self {
        Self::new_empty(ExceptionKind::SyntaxError)
    }

    pub fn stop_iteration() -> Self {
        Self::new_empty(ExceptionKind::StopIteration)
    }

    pub fn stop_iteration_with(obj: Reference) -> Self {
        Self::new(ExceptionKind::StopIteration, vec![obj])
    }

    pub fn value_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::ValueError, msg)
    }

    pub fn name_error(name: impl Into<String>) -> Self {
        Self::new_from_str(
            ExceptionKind::ValueError,
            format!("name '{}' is not defined", name.into()),
        )
    }

    pub fn key_error(key: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::KeyError, format!("'{}'", key.into()))
    }

    pub fn attribute_error(object_type: impl Into<String>, attr: impl Into<String>) -> Self {
        Self::new_from_str(
            ExceptionKind::AttributeError,
            format!(
                "'{}' object has no attribute '{}'",
                object_type.into(),
                attr.into()
            ),
        )
    }

    pub fn assertion_error() -> Self {
        Self::new_empty(ExceptionKind::AssertionError)
    }

    pub fn div_by_zero_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::DivisionByZero, msg.into())
    }

    pub fn lookup_error(msg: impl Into<String>) -> Self {
        Self::new_from_str(ExceptionKind::LookupError, msg)
    }

    pub fn unknown_encoding(encoding: impl Into<String>) -> Self {
        Self::lookup_error(format!("unknown encoding: {}", encoding.into()))
    }
}

impl Display for Exception {
    fn fmt(&self, _f: &mut Formatter) -> Result<(), Error> {
        todo!()
    }
}
