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
    pub fn resolve_module(&mut self, name: &str) -> DomainResult<Container<Module>> {
        if let Some(module) = self.runtime.borrow().read_module(name) {
            return Ok(module);
        }

        self.import_from_source(name)
    }

    fn import_from_source(&mut self, name: &str) -> DomainResult<Container<Module>> {
        let module_name = ModuleName::from_segments(&[name]);
        let source = self.state.load_source(&module_name)?;
        Ok(VmContext::import(
            source,
            self.state.clone(),
            self.runtime.clone(),
        ))
    }
}
