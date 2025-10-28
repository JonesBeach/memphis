use crate::domain::RuntimeValue;

use super::*;

impl TreewalkInterpreter {
    pub fn raise(&self, error_kind: ExecutionErrorKind) -> TreewalkDisruption {
        self.state.save_line_number();
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            error_kind,
        ))
    }

    pub fn div_by_zero_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::DivisionByZero(message.into()))
    }

    pub fn type_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.type_error_optional_message(Some(message.into()))
    }

    pub fn type_error_optional_message(&self, message: Option<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::TypeError(message))
    }

    pub fn unknown_encoding(&self, encoding: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::LookupError(format!(
            "unknown encoding: {}",
            encoding.into()
        )))
    }

    pub fn value_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::ValueError(message.into()))
    }

    pub fn key_error(&self, key: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::KeyError(key.into()))
    }

    pub fn name_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::NameError(name.into()))
    }

    pub fn import_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::ImportError(name.into()))
    }

    pub fn runtime_error(&self) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::RuntimeError(None))
    }

    pub fn runtime_error_with(&self, msg: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::RuntimeError(Some(msg.into())))
    }

    pub fn assertion_error(&self) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::AssertionError)
    }

    pub fn stop_iteration(&self) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::StopIteration(Box::new(
            RuntimeValue::Treewalk(TreewalkValue::None),
        )))
    }

    pub fn stop_iteration_with(&self, value: TreewalkValue) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::StopIteration(Box::new(
            RuntimeValue::Treewalk(value),
        )))
    }

    pub fn attribute_error(
        &self,
        object: &TreewalkValue,
        attr: impl Into<String>,
    ) -> TreewalkDisruption {
        self.raise(ExecutionErrorKind::AttributeError(
            object.get_class(self).borrow().name().to_string(),
            attr.into(),
        ))
    }
}
