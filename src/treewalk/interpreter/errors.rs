use super::*;

impl TreewalkInterpreter {
    pub fn error(&self, error_kind: ExecutionErrorKind) -> TreewalkDisruption {
        self.state.save_line_number();
        TreewalkDisruption::Error(ExecutionError::new(
            self.state.debug_call_stack(),
            error_kind,
        ))
    }

    pub fn div_by_zero_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::DivisionByZero(message.into()))
    }

    pub fn type_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.type_error_optional_message(Some(message.into()))
    }

    pub fn type_error_optional_message(&self, message: Option<String>) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::TypeError(message))
    }

    pub fn value_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::ValueError(message.into()))
    }

    pub fn key_error(&self, key: impl Into<String>) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::KeyError(key.into()))
    }

    pub fn name_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::NameError(name.into()))
    }

    pub fn import_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::ImportError(name.into()))
    }

    pub fn runtime_error(&self) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::RuntimeError)
    }

    pub fn assertion_error(&self) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::AssertionError)
    }

    pub fn stop_iteration(&self) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::StopIteration)
    }

    pub fn attribute_error(
        &self,
        object: &TreewalkValue,
        attr: impl Into<String>,
    ) -> TreewalkDisruption {
        self.error(ExecutionErrorKind::AttributeError(
            object.get_class(self).borrow().name().to_string(),
            attr.into(),
        ))
    }
}
