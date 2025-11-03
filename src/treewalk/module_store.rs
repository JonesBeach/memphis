use std::collections::HashMap;

use crate::{
    core::Container,
    domain::{ImportPath, Source},
    treewalk::{
        modules::{asyncio, net},
        types::Module,
    },
};

use super::TypeRegistry;

/// A store of the modules after loading and evaluation.
pub struct ModuleStore {
    store: HashMap<ImportPath, Container<Module>>,
}

impl ModuleStore {
    pub fn new() -> Self {
        Self {
            store: HashMap::default(),
        }
    }

    pub fn load_native_modules(&mut self, type_registry: &TypeRegistry) {
        asyncio::import(self);
        net::import(self, type_registry);
    }

    pub fn fetch_module(&mut self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.store.get(import_path).cloned()
    }

    pub fn store_module(&mut self, import_path: &ImportPath, module: Container<Module>) {
        self.store.insert(import_path.to_owned(), module);
    }

    pub fn get_or_create_module(&mut self, path: &ImportPath) -> Container<Module> {
        if let Some(existing) = self.fetch_module(path) {
            existing
        } else {
            let new_mod = Container::new(Module::new(Source::default()));
            self.store_module(path, new_mod.clone());
            new_mod
        }
    }
}
