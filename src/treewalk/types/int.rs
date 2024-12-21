use crate::{treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    domain::{
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    utils::{Dunder, ResolvedArguments},
    ExprResult,
};

pub struct Int;

impl Typed for Int {
    fn get_type() -> Type {
        Type::Int
    }
}

impl MethodProvider for Int {
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
            Ok(ExprResult::Integer(0))
        } else if args.len() == 2 {
            let input = args
                .get_arg(1)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            Ok(ExprResult::Integer(input))
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
