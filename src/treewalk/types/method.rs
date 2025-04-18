use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    treewalk::{
        protocols::Callable, type_system::CloneableCallable, utils::Args, TreewalkInterpreter,
        TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct Method {
    receiver: TreewalkValue,
    function: Box<dyn CloneableCallable>,
}

impl Method {
    pub fn new(receiver: TreewalkValue, function: Box<dyn CloneableCallable>) -> Self {
        Self { receiver, function }
    }

    pub fn name(&self) -> String {
        format!("{} of {}", self.function.name(), &self.receiver)
    }

    pub fn receiver(&self) -> TreewalkValue {
        self.receiver.clone()
    }
}

impl Callable for Container<Method> {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        interpreter
            .state
            .push_receiver(self.borrow().receiver.clone());
        let result = self.borrow().function.call(interpreter, args);
        interpreter.state.pop_receiver();

        result
    }

    fn name(&self) -> String {
        self.borrow().name()
    }

    fn receiver(&self) -> Option<TreewalkValue> {
        Some(self.borrow().receiver())
    }
}

impl Display for Container<Method> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<bound method {}>", self.name())
    }
}
