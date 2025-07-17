use crate::{
    bytecode_vm::VmValue,
    core::Container,
    domain::{ExecutionError, ExecutionErrorKind, RuntimeValue},
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

    pub fn import_error(&self, name: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::ImportError(name.to_string()))
    }

    pub fn name_error(&self, name: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::NameError(name.to_string()))
    }

    pub fn type_error(&self, msg: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::TypeError(Some(msg.to_string())))
    }

    pub fn stop_iteration(&self) -> ExecutionError {
        self.error(ExecutionErrorKind::StopIteration(Box::new(
            RuntimeValue::Vm(VmValue::None),
        )))
    }

    pub fn attribute_error(&self, attr: &str) -> ExecutionError {
        self.error(ExecutionErrorKind::AttributeError(
            "<TODO obj>".to_string(),
            attr.to_string(),
        ))
    }
}
