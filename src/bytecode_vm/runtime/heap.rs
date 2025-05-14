use crate::bytecode_vm::{indices::Index, VmValue};

use super::Reference;

pub struct Heap {
    pub storage: Vec<VmValue>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            storage: Vec::new(),
        }
    }

    pub fn allocate(&mut self, value: VmValue) -> Reference {
        let index = Index::new(self.storage.len());
        self.storage.push(value);
        Reference::ObjectRef(index)
    }

    pub fn get(&self, reference: Reference) -> Option<&VmValue> {
        match reference {
            Reference::ObjectRef(index) => self.storage.get(*index),
            _ => None,
        }
    }

    pub fn get_mut(&mut self, reference: Reference) -> Option<&mut VmValue> {
        match reference {
            Reference::ObjectRef(index) => self.storage.get_mut(*index),
            _ => None,
        }
    }
}
