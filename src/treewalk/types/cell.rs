use crate::{
    treewalk::{Interpreter, Scope},
    types::errors::InterpreterError,
};

use super::{traits::MemberAccessor, ExprResult};

/// This corresponds to the Python internal `Cell` class, which is returned for values captured in
/// a closure.
#[derive(Clone)]
pub struct Cell {
    scope: Scope,
}

impl Cell {
    pub fn new(value: ExprResult) -> Self {
        let mut scope = Scope::default();
        scope.insert("cell_contents", value);
        Self {
            scope: scope.to_owned(),
        }
    }
}

impl MemberAccessor for Cell {
    fn set_member(&mut self, _name: &str, _value: ExprResult) {
        unimplemented!();
    }

    fn delete_member(&mut self, _name: &str) -> Option<ExprResult> {
        unimplemented!();
    }

    fn get_member(
        &self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        Ok(self.scope.get(name))
    }
}
