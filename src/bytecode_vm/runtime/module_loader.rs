use crate::{
    bytecode_vm::{runtime::types::Module, Runtime, VmContext},
    core::Container,
    domain::{DomainResult, ModuleName},
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
    pub fn resolve_module(&mut self, module_name: &ModuleName) -> DomainResult<Container<Module>> {
        if let Some(module) = self.runtime.borrow().read_module(module_name) {
            return Ok(module);
        }

        self.import_from_source(module_name)
    }

    fn import_from_source(&mut self, module_name: &ModuleName) -> DomainResult<Container<Module>> {
        let source = self.state.load_source(module_name)?;

        let module = self.runtime.borrow_mut().create_module(module_name);

        let mut context = VmContext::from_state(
            module_name.clone(),
            source,
            self.state.clone(),
            self.runtime.clone(),
        );

        // TODO we shouldn't squash this error, but it's currently a MemphisError
        let _ = context.run_inner().expect("VM run failed");

        Ok(module)
    }
}
