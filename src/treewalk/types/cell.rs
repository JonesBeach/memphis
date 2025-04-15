use crate::treewalk::{
    protocols::MemberReader, Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue,
};

/// This corresponds to the Python internal `Cell` class, which is returned for values captured in
/// a closure.
#[derive(Clone)]
pub struct Cell {
    scope: Scope,
}

impl Cell {
    pub fn new(value: TreewalkValue) -> Self {
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
        _interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(self.scope.get(name))
    }
}
