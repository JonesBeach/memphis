use crate::treewalk::interpreter::TreewalkResult;
use crate::{core::Container, domain::Dunder, treewalk::Interpreter};

use super::domain::builtins::utils;
use super::{
    domain::{
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    iterators::{ExprResultIterator, ListIterator},
    utils::ResolvedArguments,
    ExprResult, List, Str, Tuple,
};

#[derive(Clone)]
pub struct ZipIterator(Vec<ExprResultIterator>);

impl Typed for ZipIterator {
    fn get_type() -> Type {
        Type::Zip
    }
}

impl MethodProvider for ZipIterator {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl ZipIterator {
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
            Some(ExprResult::Tuple(Tuple::new(r)))
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
    ) -> TreewalkResult<ExprResult> {
        // This function cannot be called with 2 args (1 unbound arg) because there would be
        // nothing to zip.
        utils::validate_args(&args, |len| len == 1 || len >= 3, interpreter)?;

        // The default behavior will stop zipping when the shortest iterator is exhausted,
        // which matches default behavior from Python. Using strict=True causes this to throw an
        // exception instead.
        let zip = match args.len() {
            1 => ZipIterator::default(),
            n if n >= 3 => {
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
                        return Err(interpreter.runtime_error());
                    }
                }

                ZipIterator::new(iters)
            }
            _ => unreachable!(),
        };

        Ok(ExprResult::Zip(zip))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
