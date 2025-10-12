use std::collections::HashMap;

use crate::{
    core::Container,
    domain::ImportPath,
    treewalk::{
        modules::{asyncio, net},
        types::Module,
    },
};

/// A store of the modules after loading and evaluation.
pub struct ModuleStore {
    store: HashMap<ImportPath, Container<Module>>,
}

impl ModuleStore {
    pub fn new() -> Self {
        let mut store = Self {
            store: HashMap::default(),
        };

        asyncio::import(&mut store);
        net::import(&mut store);

        store
    }

    pub fn fetch_module(&mut self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.store.get(import_path).cloned()
    }

    pub fn store_module(&mut self, import_path: &ImportPath, module: Container<Module>) {
        self.store.insert(import_path.to_owned(), module);
    }
}
