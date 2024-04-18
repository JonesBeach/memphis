use std::fmt::{Display, Error, Formatter};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult,
};

pub struct Int;

impl Int {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin), Box::new(InitBuiltin)]
    }
}

impl Display for Container<i64> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.borrow())
    }
}

struct NewBuiltin;
struct InitBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Integer(Container::new(0)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for InitBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        let output = args
            .get_self()
            .ok_or(InterpreterError::ExpectedInteger(
                interpreter.state.call_stack(),
            ))?
            .as_integer()
            .ok_or(InterpreterError::ExpectedInteger(
                interpreter.state.call_stack(),
            ))?;

        if args.is_empty() {
            Ok(ExprResult::Void)
        } else if args.len() == 1 {
            let input = args
                .get_arg(0)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            *output.borrow_mut() = *input.borrow();
            Ok(ExprResult::Void)
        } else {
            Err(InterpreterError::WrongNumberOfArguments(
                1,
                args.len(),
                interpreter.state.call_stack(),
            ))
        }
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}
