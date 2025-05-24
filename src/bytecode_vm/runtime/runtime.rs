use std::collections::HashMap;

use crate::core::Container;

use super::{heap::Heap, Module};

pub struct Runtime {
    /// This is kind of similar to the heap. When an object is created, it will live here and a
    /// reference to it will be placed on the stack. Objects here can be from any function context.
    /// This store retains ownership of the Rust objects throughout the runtime. When non-primitive
    /// objects are pushed onto the stack, a reference is used so as to not take ownership of the
    /// objects.
    pub heap: Heap,

    module_store: HashMap<String, Container<Module>>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            module_store: HashMap::new(),
        }
    }

    pub fn read_module(&self, name: &str) -> Option<Container<Module>> {
        self.module_store.get(name).cloned()
    }

    pub fn store_module(&mut self, module: Container<Module>) {
        let name = module.borrow().name.to_owned();
        self.module_store.insert(name, module);
    }
}

impl Default for Runtime {
    fn default() -> Self {
        Self::new()
    }
}
