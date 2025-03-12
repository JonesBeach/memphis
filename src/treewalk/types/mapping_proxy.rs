use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    treewalk::{interpreter::TreewalkResult, Interpreter},
};

use super::{domain::traits::IndexRead, Dict, ExprResult};

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
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> TreewalkResult<Option<ExprResult>> {
        self.0.getitem(interpreter, index)
    }
}

impl Display for MappingProxy {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "mappingproxy({})", self.0)
    }
}
