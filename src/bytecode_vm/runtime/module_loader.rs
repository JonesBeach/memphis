use std::path::Path;

use crate::{
    bytecode_vm::{VmContext, VmResult},
    core::Container,
    domain::Source,
    parser::types::ImportPath,
    runtime::MemphisState,
};

use super::{error_builder::ErrorBuilder, Module, Runtime};

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

    pub fn import(
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
