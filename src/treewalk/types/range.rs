use std::fmt::{Display, Error, Formatter};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult,
};

#[derive(Clone, PartialEq)]
pub struct Range {
    pub start: usize,
    pub stop: usize,
    pub step: usize,
}

impl Range {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin), Box::new(InitBuiltin)]
    }
}

impl Default for Range {
    fn default() -> Self {
        Self {
            start: 0,
            stop: 0,
            step: 1,
        }
    }
}

impl IntoIterator for Container<Range> {
    type Item = ExprResult;
    type IntoIter = RangeIterator;

    fn into_iter(self) -> Self::IntoIter {
        RangeIterator::new(self.borrow().clone())
    }
}

impl Display for Range {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if self.step == 1 {
            write!(f, "range({}, {})", self.start, self.stop)
        } else {
            write!(f, "range({}, {}, {})", self.start, self.stop, self.step)
        }
    }
}

#[derive(Clone)]
pub struct RangeIterator(Range);

impl RangeIterator {
    fn new(range: Range) -> Self {
        RangeIterator(range)
    }
}

impl Iterator for RangeIterator {
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.start < self.0.stop {
            let result = self.0.start;
            // Modify the start value in the range itself to prep the state for the next time
            // `next` is called.
            self.0.start += self.0.step;
            Some(ExprResult::Integer(Container::new(result as i64)))
        } else {
            None
        }
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Range(Container::new(Range::default())))
    }

    fn name(&self) -> String {
        Dunder::New.value().into()
    }
}

struct InitBuiltin;

impl Callable for InitBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        let range = args
            .get_self()
            .ok_or(InterpreterError::ExpectedRange(
                interpreter.state.call_stack(),
            ))?
            .as_range()
            .ok_or(InterpreterError::ExpectedRange(
                interpreter.state.call_stack(),
            ))?;

        if args.len() == 1 {
            let stop = args
                .get_arg(0)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            range.borrow_mut().stop = *stop.borrow() as usize;

            Ok(ExprResult::Void)
        } else if args.len() == 2 {
            let start = args
                .get_arg(0)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;
            let stop = args
                .get_arg(1)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            range.borrow_mut().start = *start.borrow() as usize;
            range.borrow_mut().stop = *stop.borrow() as usize;

            Ok(ExprResult::Void)
        } else if args.len() == 3 {
            let start = args
                .get_arg(0)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;
            let stop = args
                .get_arg(1)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;
            let step = args
                .get_arg(2)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            range.borrow_mut().start = *start.borrow() as usize;
            range.borrow_mut().stop = *stop.borrow() as usize;
            range.borrow_mut().step = *step.borrow() as usize;

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
        Dunder::Init.value().into()
    }
}
