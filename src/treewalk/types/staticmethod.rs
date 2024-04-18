use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    builtins::utils,
    traits::{Callable, NonDataDescriptor},
    utils::{Dunder, ResolvedArguments},
    Class, ExprResult,
};

#[derive(Clone)]
pub struct Staticmethod(Box<ExprResult>);

impl Staticmethod {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }

    fn new(func: Box<ExprResult>) -> Self {
        Self(func)
    }
}

pub struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        // The first arg is the class itself, the second arg is the function
        utils::validate_args(&args, 2, interpreter.state.call_stack())?;

        let function = args.get_arg(1);

        Ok(ExprResult::Staticmethod(Staticmethod::new(Box::new(
            function,
        ))))
    }

    fn name(&self) -> String {
        Dunder::New.value().into()
    }
}

impl NonDataDescriptor for Staticmethod {
    fn get_attr(
        &self,
        _interpreter: &Interpreter,
        _instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(*self.0.clone())
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
