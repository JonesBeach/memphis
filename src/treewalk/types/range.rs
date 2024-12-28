use std::fmt::{Display, Error, Formatter};

use crate::{domain::Dunder, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    domain::{
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult,
};

const DEFAULT_START: i64 = 0;
const DEFAULT_STOP: i64 = 0;
const DEFAULT_STEP: i64 = 1;

#[derive(Clone, PartialEq)]
pub struct Range {
    pub start: i64,
    pub stop: i64,
    pub step: i64,
}

impl Typed for Range {
    fn get_type() -> Type {
        Type::Range
    }
}

impl MethodProvider for Range {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Range {
    fn new(start: i64, stop: i64, step: i64) -> Self {
        Self { start, stop, step }
    }

    fn with_stop(stop: i64) -> Self {
        Self::new(DEFAULT_START, stop, DEFAULT_STEP)
    }

    fn with_start_stop(start: i64, stop: i64) -> Self {
        Self::new(start, stop, DEFAULT_STEP)
    }
}

impl Default for Range {
    fn default() -> Self {
        Self {
            start: DEFAULT_START,
            stop: DEFAULT_STOP,
            step: DEFAULT_STEP,
        }
    }
}

impl IntoIterator for Range {
    type Item = ExprResult;
    type IntoIter = RangeIterator;

    fn into_iter(self) -> Self::IntoIter {
        RangeIterator::new(self)
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
            Some(ExprResult::Integer(result))
        } else {
            None
        }
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        if args.len() == 2 {
            let stop = args
                .get_arg(1)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            Ok(ExprResult::Range(Range::with_stop(stop)))
        } else if args.len() == 3 {
            let start = args
                .get_arg(1)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;
            let stop = args
                .get_arg(2)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            Ok(ExprResult::Range(Range::with_start_stop(start, stop)))
        } else if args.len() == 4 {
            let start = args
                .get_arg(1)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;
            let stop = args
                .get_arg(2)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;
            let step = args
                .get_arg(3)
                .as_integer()
                .ok_or(InterpreterError::ExpectedInteger(
                    interpreter.state.call_stack(),
                ))?;

            Ok(ExprResult::Range(Range::new(start, stop, step)))
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
