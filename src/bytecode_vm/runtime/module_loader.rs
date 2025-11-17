use std::path::Path;

use crate::{
    bytecode_vm::{runtime::types::Module, Runtime, VmContext},
    core::Container,
    domain::{DomainResult, ExecutionError, ImportPath, Source},
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

        self.import_from_source(name, None)
    }

    fn import_from_source(
        &mut self,
        name: &str,
        _caller_path: Option<&Path>,
    ) -> DomainResult<Container<Module>> {
        let source = self
            .resolve_source(name)
            .ok_or_else(|| ExecutionError::import_error(name))?;
        Ok(VmContext::import(
            source,
            self.state.clone(),
            self.runtime.clone(),
        ))
    }

    fn resolve_source(&self, name: &str) -> Option<Source> {
        let search_paths = self.state.search_paths();
        // TODO this is a placeholder, to support relative imports, I believe we'll need to get the
        // path of the current Module.
        let current_path = Path::new(".");
        let import_path = ImportPath::Absolute(vec![name.to_string()]);
        Source::from_import_path(&import_path, current_path, &search_paths)
    }
}
