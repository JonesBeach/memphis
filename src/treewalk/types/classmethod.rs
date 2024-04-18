use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    builtins::utils,
    traits::{Callable, NonDataDescriptor},
    utils::{Dunder, ResolvedArguments},
    Class, ExprResult, Method,
};

#[derive(Clone)]
pub struct Classmethod(Container<Box<dyn Callable>>);

impl Classmethod {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }

    fn new(func: Container<Box<dyn Callable>>) -> Self {
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

        // This is a workaround for Generic type behavior found in _collections_abc.py.
        // _weakrefset.py also uses this.
        //
        // ```
        // GenericAlias = type(list[int])
        // __class_getitem__ = classmethod(GenericAlias)
        // ```
        if args.get_arg(1).as_class().is_some() {
            return Ok(ExprResult::None);
        }

        let function = args
            .get_arg(1)
            .as_callable()
            .ok_or(InterpreterError::ExpectedFunction(
                interpreter.state.call_stack(),
            ))?;
        Ok(ExprResult::Classmethod(Classmethod::new(function)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Classmethod {
    fn get_attr(
        &self,
        _interpreter: &Interpreter,
        _instance: Option<ExprResult>,
        owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Method(Container::new(Method::new(
            ExprResult::Class(owner),
            self.0.clone(),
        ))))
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
