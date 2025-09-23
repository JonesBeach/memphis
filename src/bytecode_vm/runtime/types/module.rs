use std::{
    collections::HashMap,
    fmt::{Display, Error, Formatter},
};

use crate::bytecode_vm::runtime::Reference;

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub name: String,

    /// The runtime mapping of global variables to their values.
    global_store: HashMap<String, Reference>,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            global_store: HashMap::new(),
        }
    }

    pub fn read(&self, name: &str) -> Option<Reference> {
        self.global_store.get(name).cloned()
    }

    pub fn write(&mut self, name: &str, value: Reference) {
        self.global_store.insert(name.to_string(), value);
    }

    #[cfg(test)]
    pub fn global_store(&self) -> &HashMap<String, Reference> {
        &self.global_store
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<module '{}'>", self.name)
    }
}
