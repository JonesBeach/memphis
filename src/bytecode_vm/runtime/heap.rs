use crate::bytecode_vm::{indices::Index, VmValue};

use super::Reference;

const NONE_INDEX: usize = 0;

pub struct Heap {
    storage: Vec<VmValue>,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            storage: vec![VmValue::None],
        }
    }

    pub fn none(&self) -> Reference {
        Reference::ObjectRef(Index::new(NONE_INDEX))
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

    pub fn iter(&self) -> impl Iterator<Item = &VmValue> {
        self.storage.iter()
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}
