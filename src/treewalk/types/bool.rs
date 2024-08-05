use crate::{treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult,
};

pub struct Bool;

impl Bool {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
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
        Dunder::New.value().into()
    }
}
