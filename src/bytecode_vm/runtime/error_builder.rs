use crate::{
    core::Container,
    domain::{ExecutionError, ExecutionErrorKind},
    runtime::MemphisState,
};

pub struct ErrorBuilder {
    state: Container<MemphisState>,
}

impl ErrorBuilder {
    pub fn new(state: Container<MemphisState>) -> Self {
        Self { state }
    }

    pub fn error(&self, kind: ExecutionErrorKind) -> ExecutionError {
        self.state.save_line_number();
        ExecutionError::new(self.state.debug_call_stack(), kind)
    }

    pub fn runtime_error(&self) -> ExecutionError {
        self.error(ExecutionErrorKind::RuntimeError)
    }
}
