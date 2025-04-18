use std::fmt::{Display, Error, Formatter};

use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
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

impl_typed!(Range, Type::Range);
impl_method_provider!(Range, [NewBuiltin]);

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
    type Item = TreewalkValue;
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
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.start < self.0.stop {
            let result = self.0.start;
            // Modify the start value in the range itself to prep the state for the next time
            // `next` is called.
            self.0.start += self.0.step;
            Some(TreewalkValue::Integer(result))
        } else {
            None
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [2, 3, 4].contains(&len), interpreter)?;

        let range = match args.len() {
            2 => {
                let stop = args.get_arg(1).expect_integer(interpreter)?;
                Range::with_stop(stop)
            }
            3 => {
                let start = args.get_arg(1).expect_integer(interpreter)?;
                let stop = args.get_arg(2).expect_integer(interpreter)?;
                Range::with_start_stop(start, stop)
            }
            4 => {
                let start = args.get_arg(1).expect_integer(interpreter)?;
                let stop = args.get_arg(2).expect_integer(interpreter)?;
                let step = args.get_arg(3).expect_integer(interpreter)?;
                Range::new(start, stop, step)
            }
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Range(range))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
