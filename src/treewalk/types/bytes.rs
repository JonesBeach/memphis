use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult,
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Bytes(pub Vec<u8>);

impl Bytes {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }

    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        match args.len() {
            1 => Ok(ExprResult::Bytes(Container::new(Bytes::new("".into())))),
            2 => match args.get_arg(1) {
                ExprResult::String(_) => Err(InterpreterError::TypeError(
                    Some("string argument without an encoding".into()),
                    interpreter.state.call_stack(),
                )),
                ExprResult::Bytes(s) => Ok(ExprResult::Bytes(s)),
                _ => todo!(),
            },
            // TODO support an optional encoding
            3 => todo!(),
            _ => Err(InterpreterError::WrongNumberOfArguments(
                args.len(),
                1,
                interpreter.state.call_stack(),
            )),
        }
    }

    fn name(&self) -> String {
        Dunder::New.value().into()
    }
}
