use std::collections::HashMap;

use crate::{
    bytecode_vm::{runtime::Reference, VirtualMachine, VmValue},
    domain::DomainResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Dict {
    // Python 3.7+ preserves dict insertion order, we could use IndexMap if we want that.
    items: HashMap<Reference, Reference>,
}

impl Dict {
    pub fn new(items: Vec<(Reference, Reference)>) -> Self {
        let mut dict = HashMap::new();
        for (k, v) in items {
            dict.insert(k, v);
        }
        Self { items: dict }
    }

    pub fn resolved_items(&self, vm: &VirtualMachine) -> DomainResult<Vec<(VmValue, VmValue)>> {
        self.items
            .iter()
            .map(|(k, v)| Ok((vm.deref(*k)?, vm.deref(*v)?)))
            .collect()
    }
}
