use crate::bytecode_vm::{runtime::Reference, VirtualMachine, VmResult, VmValue};

#[derive(Clone, Debug, PartialEq)]
pub struct Tuple {
    // TODO this is currently public because we need it in some tests to call deference on the
    // elements. We'll eventually make this a slice accessor or an iterator or something.
    pub items: Vec<Reference>,
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

    pub fn resolved_items(&self, vm: &VirtualMachine) -> VmResult<Vec<VmValue>> {
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
