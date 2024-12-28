use crate::{domain::Dunder, treewalk::Interpreter, types::errors::InterpreterError};

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
pub struct Memoryview;

impl Typed for Memoryview {
    fn get_type() -> Type {
        Type::Memoryview
    }
}

impl MethodProvider for Memoryview {
    fn get_methods() -> Vec<Box<dyn Callable>> {
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
        Dunder::New.into()
    }
}
