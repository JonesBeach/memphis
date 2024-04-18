use crate::{treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult,
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Memoryview;

impl Memoryview {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        unimplemented!()
    }

    fn name(&self) -> String {
        Dunder::New.value().into()
    }
}
