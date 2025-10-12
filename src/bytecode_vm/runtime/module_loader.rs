use std::path::Path;

use crate::{
    bytecode_vm::{
        runtime::{components::ErrorBuilder, types::Module},
        Runtime, VmContext, VmResult,
    },
    core::Container,
    domain::{ImportPath, Source},
    runtime::MemphisState,
};

pub struct ModuleLoader {
    state: Container<MemphisState>,
    runtime: Container<Runtime>,
    error_builder: ErrorBuilder,
}

impl ModuleLoader {
    pub fn new(state: Container<MemphisState>, runtime: Container<Runtime>) -> Self {
        let error_builder = ErrorBuilder::new(state.clone());
        Self {
            state,
            runtime,
            error_builder,
        }
    }

    /// Check if the module is already present (e.g. Rust-backed or previously imported).
    pub fn resolve_module(&mut self, name: &str) -> VmResult<Container<Module>> {
        if let Some(module) = self.runtime.borrow().read_module(name) {
            return Ok(module);
        }

        self.import_from_source(name, None)
    }

    fn import_from_source(
        &mut self,
        name: &str,
        _caller_path: Option<&Path>,
    ) -> VmResult<Container<Module>> {
        let source = self.expect_source(name)?;
        VmContext::import(source, self.state.clone(), self.runtime.clone())
    }

    fn resolve_source(&self, name: &str) -> Option<Source> {
        let search_paths = self.state.search_paths();
        // TODO this is a placeholder, to support relative imports, I believe we'll need to get the
        // path of the current Module.
        let current_path = Path::new(".");
        let import_path = ImportPath::Absolute(vec![name.to_string()]);
        Source::from_import_path(&import_path, current_path, &search_paths)
    }

    fn expect_source(&self, name: &str) -> VmResult<Source> {
        self.resolve_source(name)
            .ok_or_else(|| self.error_builder.import_error(name))
    }
}
