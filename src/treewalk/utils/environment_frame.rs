use crate::{
    core::Container,
    treewalk::{Scope, TreewalkValue},
};

/// This implements lexical scoping necessary to support closures.
#[derive(Debug)]
pub struct EnvironmentFrame {
    scope: Container<Scope>,
    parent: Option<Box<Container<EnvironmentFrame>>>,
}

impl EnvironmentFrame {
    pub fn new(scope: Container<Scope>, parent: Option<Box<Container<EnvironmentFrame>>>) -> Self {
        Self { scope, parent }
    }

    /// This reads up the lexical scoping stack (as opposed to the runtime stack) to see if any
    /// enclosing frames contain the value in scope.
    pub fn read(&self, name: &str) -> Option<TreewalkValue> {
        match self.scope.borrow().get(name) {
            Some(value) => Some(value.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().read(name),
                None => None,
            },
        }
    }

    /// Writes a value to the variable in the closest enclosing scope where it is defined.
    /// If the variable is not found in any enclosing scopes, an error is thrown.
    pub fn write(&mut self, name: &str, value: TreewalkValue) {
        if self.scope.borrow().get(name).is_some() {
            self.scope.borrow_mut().insert(name, value);
        } else if let Some(parent) = &mut self.parent {
            parent.borrow_mut().write(name, value);
        } else {
            panic!("not found!");
        }
    }
}
