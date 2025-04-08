use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    treewalk::{protocols::Callable, utils::Arguments, Interpreter, TreewalkResult, TreewalkValue},
};

#[derive(Debug, Clone)]
pub struct Method {
    receiver: TreewalkValue,
    function: Container<Box<dyn Callable>>,
}

impl Method {
    pub fn new(receiver: TreewalkValue, function: Container<Box<dyn Callable>>) -> Self {
        Self { receiver, function }
    }

    pub fn name(&self) -> String {
        format!("{} of {}", self.function.borrow().name(), &self.receiver)
    }

    pub fn receiver(&self) -> TreewalkValue {
        self.receiver.clone()
    }
}

impl Callable for Container<Method> {
    fn call(&self, interpreter: &Interpreter, args: Arguments) -> TreewalkResult<TreewalkValue> {
        interpreter
            .state
            .push_receiver(self.borrow().receiver.clone());
        let result = self.borrow().function.borrow().call(interpreter, args);
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
