use crate::{
    core::Container, domain::Dunder, treewalk::Interpreter, types::errors::InterpreterError,
};

use super::{
    domain::{
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult,
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct ByteArray(Vec<u8>);

impl Typed for ByteArray {
    fn get_type() -> Type {
        Type::ByteArray
    }
}

impl MethodProvider for ByteArray {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl ByteArray {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    pub fn raw(&self) -> &[u8] {
        &self.0
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
            1 => Ok(ExprResult::ByteArray(Container::new(ByteArray::new(
                "".into(),
            )))),
            2 => match args.get_arg(1) {
                ExprResult::String(_) => Err(InterpreterError::TypeError(
                    Some("string argument without an encoding".into()),
                    interpreter.state.call_stack(),
                )),
                ExprResult::Bytes(s) => {
                    Ok(ExprResult::ByteArray(Container::new(ByteArray::new(s))))
                }
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
        Dunder::New.into()
    }
}
