use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    iterators::{ExprResultIterator, ListIterator},
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult, List, Str, Tuple,
};

#[derive(Clone)]
pub struct ZipIterator(Vec<ExprResultIterator>);

impl ZipIterator {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }

    pub fn new(items: Vec<ExprResultIterator>) -> Self {
        Self(items)
    }
}

impl Default for ZipIterator {
    fn default() -> Self {
        Self(vec![ExprResultIterator::List(ListIterator::new(
            Container::new(List::new(vec![])),
        ))])
    }
}

impl Iterator for ZipIterator {
    type Item = ExprResult;

    /// Return the next item from each of the composite iterators in a tuple until the shortest
    /// iterator has been exhausted, then return `None`.
    fn next(&mut self) -> Option<Self::Item> {
        // Advance all the composite iterators
        let results = self
            .0
            .iter_mut()
            .map(|i| i.next())
            .collect::<Vec<Option<ExprResult>>>();

        if results.iter().all(|r| r.is_some()) {
            let r = results
                .iter()
                .map(|i| i.clone().unwrap())
                .collect::<Vec<ExprResult>>();
            Some(ExprResult::Tuple(Container::new(Tuple::new(r))))
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
        // The default behavior will stop zipping when the shortest iterator is exhausted,
        // which matches default behavior from Python. Using strict=True causes this to throw an
        // exception instead.
        if args.len() == 1 {
            Ok(ExprResult::Zip(ZipIterator::default()))
        } else if args.len() >= 3 {
            // The first arg is the class, so we must consume it before beginning the zip
            // operation.
            let mut iter = args.iter_args();
            iter.next();

            let iters = iter
                .map(|a| a.clone().into_iter())
                .collect::<Vec<ExprResultIterator>>();

            if args
                .get_kwarg(&ExprResult::String(Str::new("strict".to_string())))
                .is_some_and(|k| k == ExprResult::Boolean(true))
            {
                let lengths = iters
                    .iter()
                    .map(|i| i.clone().count())
                    .collect::<Vec<usize>>();
                let all_equal = lengths.is_empty() || lengths.iter().all(|&x| x == lengths[0]);

                if !all_equal {
                    return Err(InterpreterError::RuntimeError);
                }
            }

            Ok(ExprResult::Zip(ZipIterator::new(iters)))
        } else {
            Err(InterpreterError::WrongNumberOfArguments(
                2,
                args.len(),
                interpreter.state.call_stack(),
            ))
        }
    }

    fn name(&self) -> String {
        Dunder::New.value().into()
    }
}
