use crate::treewalk::interpreter::TreewalkResult;
use crate::{core::Container, domain::Dunder, treewalk::Interpreter};

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
pub struct Staticmethod(Box<ExprResult>);

impl Typed for Staticmethod {
    fn get_type() -> Type {
        Type::Staticmethod
    }
}

impl MethodProvider for Staticmethod {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Staticmethod {
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
    ) -> TreewalkResult<ExprResult> {
        // The first arg is the class itself, the second arg is the function
        utils::validate_args(&args, |len| len == 2, interpreter)?;
        let function = args.get_arg(1);
        Ok(ExprResult::Staticmethod(Staticmethod::new(Box::new(
            function,
        ))))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Staticmethod {
    fn get_attr(
        &self,
        _interpreter: &Interpreter,
        _instance: Option<ExprResult>,
        _owner: Container<Class>,
    ) -> TreewalkResult<ExprResult> {
        Ok(*self.0.clone())
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
