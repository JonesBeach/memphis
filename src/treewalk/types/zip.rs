use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        types::{iterators::ListIterator, List, Str, Tuple},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkIterator, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct ZipIterator(Vec<TreewalkIterator>);

impl_typed!(ZipIterator, Type::Zip);
impl_method_provider!(ZipIterator, [NewBuiltin]);

impl ZipIterator {
    pub fn new(items: Vec<TreewalkIterator>) -> Self {
        Self(items)
    }
}

impl Default for ZipIterator {
    fn default() -> Self {
        Self(vec![TreewalkIterator::List(ListIterator::new(
            Container::new(List::new(vec![])),
        ))])
    }
}

impl Iterator for ZipIterator {
    type Item = TreewalkValue;

    /// Return the next item from each of the composite iterators in a tuple until the shortest
    /// iterator has been exhausted, then return `None`.
    fn next(&mut self) -> Option<Self::Item> {
        // Advance all the composite iterators
        let results = self
            .0
            .iter_mut()
            .map(|i| i.next())
            .collect::<Vec<Option<TreewalkValue>>>();

        if results.iter().all(|r| r.is_some()) {
            let r = results
                .iter()
                .map(|i| i.clone().unwrap())
                .collect::<Vec<TreewalkValue>>();
            Some(TreewalkValue::Tuple(Tuple::new(r)))
        } else {
            None
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        // This function cannot be called with 2 args (1 unbound arg) because there would be
        // nothing to zip.
        check_args(&args, |len| len == 1 || len >= 3, interpreter)?;

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
                    .collect::<Vec<TreewalkIterator>>();

                if args
                    .get_kwarg(&TreewalkValue::String(Str::new("strict".to_string())))
                    .is_some_and(|k| k == TreewalkValue::Boolean(true))
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

        Ok(TreewalkValue::Zip(zip))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
