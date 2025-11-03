use std::{
    cmp::Ordering,
    fmt::{Display, Error, Formatter},
};

use crate::{
    domain::{Dunder, Type},
    parser::types::{Expr, SliceParams},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct Slice {
    pub start: Option<i64>,
    pub stop: Option<i64>,
    pub step: Option<i64>,
}

impl_typed!(Slice, Type::Slice);
impl_method_provider!(Slice, [NewBuiltin]);

impl Slice {
    pub fn new(start: Option<i64>, stop: Option<i64>, step: Option<i64>) -> Self {
        Self { start, stop, step }
    }

    pub fn resolve(
        interpreter: &TreewalkInterpreter,
        params: &SliceParams,
    ) -> TreewalkResult<Self> {
        let evaluate_to_integer = |expr_option: &Option<Expr>| -> TreewalkResult<Option<i64>> {
            match expr_option {
                Some(expr) => {
                    let integer = interpreter
                        .evaluate_expr(expr)?
                        .as_int()
                        .raise(interpreter)?;
                    Ok(Some(integer))
                }
                None => Ok(None),
            }
        };

        let start = evaluate_to_integer(&params.start)?;
        let stop = evaluate_to_integer(&params.stop)?;
        let step = evaluate_to_integer(&params.step)?;

        Ok(Self::new(start, stop, step))
    }

    /// Adjusting start and stop according to Python's slicing rules of negative indices
    /// wrapping around the iterable.
    fn adjust_params(slice: &Slice, len: i64) -> (i64, i64, i64) {
        let start = slice.start.unwrap_or(0);
        let stop = slice.stop.unwrap_or(len);
        let step = slice.step.unwrap_or(1);

        let start = if start < 0 { len + start } else { start };
        let stop = if stop < 0 { len + stop } else { stop };

        let start = start.clamp(0, len);
        let stop = stop.clamp(0, len);

        (start, stop, step)
    }

    pub fn slice<T>(slice: &Slice, len: i64, fetch: impl Fn(i64) -> Option<T>) -> Vec<T> {
        let (start, stop, step) = Self::adjust_params(slice, len);

        let mut result = Vec::new();
        match step.cmp(&0) {
            Ordering::Greater => {
                let mut i = start;
                while i < stop {
                    if let Some(item) = fetch(i) {
                        result.push(item);
                    }
                    i += step;
                }
            }
            Ordering::Less => {
                let mut i = stop - 1;
                while i >= start {
                    if let Some(item) = fetch(i) {
                        result.push(item);
                    }
                    i += step;
                }
            }
            Ordering::Equal => panic!("slice step cannot be zero"),
        }

        result
    }
}

impl Display for Slice {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let format_val =
            |val: &Option<i64>| val.map_or("None".to_string(), |number| number.to_string());

        let start = format_val(&self.start);
        let stop = format_val(&self.stop);
        let step = format_val(&self.step);

        write!(f, "slice({start}, {stop}, {step})")
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [2, 3, 4].contains(&len), interpreter)?;

        let slice = match args.len() {
            2 => {
                let stop = args.get_arg(1).as_int().raise(interpreter)?;
                Slice::new(None, Some(stop), None)
            }
            3 => {
                let start = args.get_arg(1).as_int().raise(interpreter)?;
                let stop = args.get_arg(2).as_int().raise(interpreter)?;
                Slice::new(Some(start), Some(stop), None)
            }
            4 => {
                let start = args.get_arg(1).as_int().raise(interpreter)?;
                let stop = args.get_arg(2).as_int().raise(interpreter)?;
                let step = args.get_arg(3).as_int().raise(interpreter)?;
                Slice::new(Some(start), Some(stop), Some(step))
            }
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Slice(slice))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
