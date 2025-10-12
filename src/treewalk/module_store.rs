use std::collections::HashMap;

use crate::{
    core::Container,
    domain::Source,
    parser::types::ImportPath,
    treewalk::{
        executor::{AsyncioCreateTaskBuiltin, AsyncioRunBuiltin, AsyncioSleepBuiltin},
        types::Module,
    },
};

use super::{type_system::CloneableCallable, TreewalkValue};

fn get_asyncio_builtins() -> Vec<Box<dyn CloneableCallable>> {
    vec![
        Box::new(AsyncioRunBuiltin),
        Box::new(AsyncioSleepBuiltin),
        Box::new(AsyncioCreateTaskBuiltin),
    ]
}

/// A store of the modules after loading and evaluation.
pub struct ModuleStore {
    store: HashMap<ImportPath, Container<Module>>,
}

impl ModuleStore {
    pub fn new() -> Self {
        let mut module_cache = HashMap::default();

        let mut asyncio_mod = Module::new(Source::default());
        for builtin in get_asyncio_builtins() {
            asyncio_mod.insert(&builtin.name(), TreewalkValue::BuiltinFunction(builtin));
        }

        module_cache.insert(
            ImportPath::Absolute(vec!["asyncio".to_string()]),
            Container::new(asyncio_mod),
        );

        // This is a stub. We're gonna add some socket utilities in here.
        let net_mod = Module::new(Source::default());
        module_cache.insert(
            ImportPath::Absolute(vec!["memphis".to_string(), "net".to_string()]),
            Container::new(net_mod),
        );

        Self {
            store: module_cache,
        }
    }

    pub fn fetch_module(&mut self, import_path: &ImportPath) -> Option<Container<Module>> {
        self.store.get(import_path).cloned()
    }

    pub fn store_module(&mut self, import_path: &ImportPath, module: Container<Module>) {
        self.store.insert(import_path.to_owned(), module);
    }
}
