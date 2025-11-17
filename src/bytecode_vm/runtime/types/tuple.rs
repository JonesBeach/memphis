use crate::{
    bytecode_vm::{runtime::Reference, VirtualMachine, VmValue},
    domain::DomainResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    items: Vec<Reference>,
}

impl Tuple {
    pub fn new(items: Vec<Reference>) -> Self {
        Self { items }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(self) -> TupleIter {
        TupleIter {
            inner: self.items.into_iter(),
        }
    }

    pub fn resolved_items(&self, vm: &VirtualMachine) -> DomainResult<Vec<VmValue>> {
        self.items.iter().map(|r| vm.deref(*r)).collect()
    }
}

#[derive(Clone, Debug)]
pub struct TupleIter {
    inner: std::vec::IntoIter<Reference>,
}

impl Iterator for TupleIter {
    type Item = Reference;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
