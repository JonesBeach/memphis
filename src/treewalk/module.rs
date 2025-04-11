use std::collections::HashMap;

use crate::{core::Container, parser::types::ImportPath, treewalk::types::Module};

pub struct EvaluatedModuleCache {
    /// A cache of the module after evaluation.
    module_cache: HashMap<ImportPath, Container<Module>>,
}

impl EvaluatedModuleCache {
    pub fn new() -> Self {
        Self {
            module_cache: HashMap::default(),
        }
    }

    pub fn fetch_module(&mut self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.module_cache.get(import_path).cloned()
    }

    pub fn store_module(&mut self, import_path: &ImportPath, module: Container<Module>) {
        self.module_cache.insert(import_path.to_owned(), module);
    }
}
