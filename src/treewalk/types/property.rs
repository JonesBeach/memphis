use crate::{
    core::Container, domain::Dunder, resolved_args, treewalk::Interpreter,
    types::errors::InterpreterError,
};

use super::{
    domain::{
        builtins::utils,
        traits::{Callable, MethodProvider, NonDataDescriptor, Typed},
        Type,
    },
    utils::ResolvedArguments,
    Class, ExprResult,
};

#[derive(Clone)]
pub struct Property(Container<Box<dyn Callable>>);

impl Typed for Property {
    fn get_type() -> Type {
        Type::Property
    }
}

impl MethodProvider for Property {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Property {
    fn new(function: Container<Box<dyn Callable>>) -> Self {
        Self(function)
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

        let function = args
            .get_arg(1)
            .as_callable()
            .ok_or(InterpreterError::ExpectedFunction(
                interpreter.state.call_stack(),
            ))?;

        Ok(ExprResult::Property(Property::new(function)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Property {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        let Some(instance) = instance else {
            panic!("No instance for descriptor!");
        };

        interpreter.call(self.0.clone(), &resolved_args![instance])
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
