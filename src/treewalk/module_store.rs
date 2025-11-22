use std::collections::HashMap;

use crate::{
    core::Container,
    domain::ModuleName,
    treewalk::{
        modules::{asyncio, net},
        types::Module,
    },
};

use super::TypeRegistry;

/// A store of the modules after loading and evaluation.
pub struct ModuleStore {
    store: HashMap<ModuleName, Container<Module>>,
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

    pub fn fetch_module(&mut self, name: &ModuleName) -> Option<Container<Module>> {
        self.store.get(name).cloned()
    }

    pub fn store_module(&mut self, module: Container<Module>) {
        let name = module.borrow().name().to_owned();
        self.store.insert(name, module);
    }

    pub fn get_or_create_module(&mut self, name: &ModuleName) -> Container<Module> {
        if let Some(existing) = self.fetch_module(name) {
            existing
        } else {
            let new_mod = Container::new(Module::new_builtin(name.clone()));
            self.store_module(new_mod.clone());
            new_mod
        }
    }
}
