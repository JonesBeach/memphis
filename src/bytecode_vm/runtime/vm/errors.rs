use crate::domain::{ExecutionError, ExecutionErrorKind};

use super::VirtualMachine;

impl VirtualMachine {
    fn error(&self, kind: ExecutionErrorKind) -> ExecutionError {
        self.error_builder.error(kind)
    }

    pub fn runtime_error(&self) -> ExecutionError {
        self.error(ExecutionErrorKind::RuntimeError)
    }

    pub fn name_error(&self, name: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::NameError(name.to_string()))
    }

    pub fn type_error(&self, msg: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::TypeError(Some(msg.to_string())))
    }

    pub fn attribute_error(&self, attr: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::AttributeError(
            "<TODO obj>".to_string(),
            attr.to_string(),
        ))
    }
}
