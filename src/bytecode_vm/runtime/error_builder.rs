use crate::{
    bytecode_vm::VmValue,
    core::Container,
    domain::{ExecutionError, RuntimeError, RuntimeValue},
    runtime::MemphisState,
};

pub struct ErrorBuilder {
    state: Container<MemphisState>,
}

impl ErrorBuilder {
    pub fn new(state: Container<MemphisState>) -> Self {
        Self { state }
    }

    pub fn error(&self, kind: ExecutionError) -> RuntimeError {
        self.state.save_line_number();
        RuntimeError::new(self.state.debug_call_stack(), kind)
    }

    pub fn runtime_error(&self) -> RuntimeError {
        self.error(ExecutionError::RuntimeError(None))
    }

    pub fn import_error(&self, name: &str) -> RuntimeError {
        self.error(ExecutionError::ImportError(name.to_string()))
    }

    pub fn name_error(&self, name: &str) -> RuntimeError {
        self.error(ExecutionError::NameError(name.to_string()))
    }

    pub fn type_error(&self, msg: &str) -> RuntimeError {
        self.error(ExecutionError::TypeError(Some(msg.to_string())))
    }

    pub fn stop_iteration(&self) -> RuntimeError {
        self.error(ExecutionError::StopIteration(Box::new(RuntimeValue::Vm(
            VmValue::None,
        ))))
    }

    pub fn attribute_error(&self, attr: &str) -> RuntimeError {
        self.error(ExecutionError::AttributeError(
            "<TODO obj>".to_string(),
            attr.to_string(),
        ))
    }
}
