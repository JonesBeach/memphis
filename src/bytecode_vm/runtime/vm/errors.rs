use crate::domain::{ExecutionError, ExecutionErrorKind};

use super::VirtualMachine;

impl VirtualMachine {
    fn error(&self, error_kind: ExecutionErrorKind) -> ExecutionError {
        self.state.save_line_number();
        ExecutionError::new(self.state.debug_call_stack(), error_kind)
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
}
