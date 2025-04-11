use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    treewalk::{
        protocols::IndexRead, types::Dict, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// A read-only view into a `Dict`. This is used by Python for things like `Dunder::Dict`.
#[derive(Clone, PartialEq)]
pub struct MappingProxy(Container<Dict>);

impl MappingProxy {
    pub fn new(dict: Container<Dict>) -> Self {
        Self(dict)
    }
}

impl IndexRead for MappingProxy {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        self.0.getitem(interpreter, index)
    }
}

impl Display for MappingProxy {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "mappingproxy({})", self.0)
    }
}
