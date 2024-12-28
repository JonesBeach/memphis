use crate::{domain::Dunder, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    domain::{
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult,
};

pub struct Bool;

impl Typed for Bool {
    fn get_type() -> Type {
        Type::Bool
    }
}

impl MethodProvider for Bool {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        if args.len() == 1 {
            Ok(ExprResult::Boolean(false))
        } else if args.len() == 2 {
            let input = args.get_arg(1).as_boolean();
            Ok(ExprResult::Boolean(input))
        } else {
            Err(InterpreterError::WrongNumberOfArguments(
                1,
                args.len(),
                interpreter.state.call_stack(),
            ))
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
