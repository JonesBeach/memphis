use std::fmt::{Display, Error, Formatter};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{domain::traits::IndexRead, Dict, ExprResult};

/// A read-only view into a `Dict`. This is used by Python for things like `Dunder::Dict`.
#[derive(PartialEq)]
pub struct MappingProxy(Container<Dict>);

impl MappingProxy {
    pub fn new(dict: Container<Dict>) -> Self {
        Self(dict)
    }
}

impl IndexRead for Container<MappingProxy> {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        self.borrow().0.getitem(interpreter, index)
    }
}

impl Display for Container<MappingProxy> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "mappingproxy({})", self.borrow().0)
    }
}
