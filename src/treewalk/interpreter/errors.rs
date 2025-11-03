use crate::domain::RuntimeValue;

use super::*;

impl TreewalkInterpreter {
    pub fn raise(&self, error_kind: ExecutionError) -> TreewalkDisruption {
        self.state.save_line_number();
        TreewalkDisruption::Error(RuntimeError::new(self.state.debug_call_stack(), error_kind))
    }

    pub fn div_by_zero_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionError::DivisionByZero(message.into()))
    }

    pub fn type_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.type_error_optional_message(Some(message.into()))
    }

    pub fn type_error_optional_message(&self, message: Option<String>) -> TreewalkDisruption {
        self.raise(ExecutionError::TypeError(message))
    }

    pub fn value_error(&self, message: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionError::ValueError(message.into()))
    }

    pub fn key_error(&self, key: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionError::KeyError(key.into()))
    }

    pub fn name_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionError::NameError(name.into()))
    }

    pub fn import_error(&self, name: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionError::ImportError(name.into()))
    }

    pub fn runtime_error(&self) -> TreewalkDisruption {
        self.raise(ExecutionError::RuntimeError(None))
    }

    pub fn runtime_error_with(&self, msg: impl Into<String>) -> TreewalkDisruption {
        self.raise(ExecutionError::RuntimeError(Some(msg.into())))
    }

    pub fn assertion_error(&self) -> TreewalkDisruption {
        self.raise(ExecutionError::AssertionError)
    }

    pub fn stop_iteration(&self) -> TreewalkDisruption {
        self.raise(ExecutionError::StopIteration(Box::new(
            RuntimeValue::Treewalk(TreewalkValue::None),
        )))
    }

    pub fn stop_iteration_with(&self, value: TreewalkValue) -> TreewalkDisruption {
        self.raise(ExecutionError::StopIteration(Box::new(
            RuntimeValue::Treewalk(value),
        )))
    }

    pub fn attribute_error(
        &self,
        object: &TreewalkValue,
        attr: impl Into<String>,
    ) -> TreewalkDisruption {
        self.raise(ExecutionError::AttributeError(
            object.get_class(self).borrow().name().to_string(),
            attr.into(),
        ))
    }
}
