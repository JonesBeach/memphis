use crate::{
    bytecode_vm::{
        runtime::types::{Exception, Module},
        RaisedException, Runtime, VmContext, VmResult,
    },
    core::Container,
    domain::ModuleName,
    runtime::MemphisState,
};

pub struct ModuleLoader {
    state: Container<MemphisState>,
    runtime: Container<Runtime>,
}

impl ModuleLoader {
    pub fn new(state: Container<MemphisState>, runtime: Container<Runtime>) -> Self {
        Self { state, runtime }
    }

    /// Check if the module is already present (e.g. Rust-backed or previously imported).
    pub fn resolve_module(&mut self, module_name: &ModuleName) -> VmResult<Container<Module>> {
        if let Some(module) = self.runtime.borrow().read_module(module_name) {
            return Ok(module);
        }

        self.import_from_source(module_name)
    }

    // TODO this is currently duplicated from VM, we should remove this
    fn raise(&self, exception: Exception) -> RaisedException {
        self.state.save_line_number();
        RaisedException::new(self.state.debug_call_stack(), exception)
    }

    fn import_from_source(&mut self, module_name: &ModuleName) -> VmResult<Container<Module>> {
        let source = self
            .state
            .load_source(module_name)
            .map_err(|err| self.raise(Exception::import_error(err.message)))?;

        let module = self.runtime.borrow_mut().create_module(module_name);

        let mut context = VmContext::from_state(
            module_name.clone(),
            source,
            self.state.clone(),
            self.runtime.clone(),
        );

        context.run_inner()?;

        Ok(module)
    }
}
