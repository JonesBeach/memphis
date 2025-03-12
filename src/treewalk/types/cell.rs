use crate::treewalk::{interpreter::TreewalkResult, Interpreter, Scope};

use super::{domain::traits::MemberReader, ExprResult};

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

impl MemberReader for Cell {
    fn get_member(
        &self,
        _interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<ExprResult>> {
        Ok(self.scope.get(name))
    }
}
