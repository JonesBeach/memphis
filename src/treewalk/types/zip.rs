use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        type_system::CloneableIterable,
        types::Tuple,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default)]
pub struct ZipIterator(Vec<Box<dyn CloneableIterable>>);

impl Clone for ZipIterator {
    /// This works similar to the [dyn-clone](https://github.com/dtolnay/dyn-clone) crate.
    fn clone(&self) -> Self {
        Self(self.0.iter().map(safe_clone).collect())
    }
}

impl_typed!(ZipIterator, Type::Zip);
impl_method_provider!(ZipIterator, [NewBuiltin]);
impl_iterable!(ZipIterator);

impl ZipIterator {
    pub fn new(items: Vec<Box<dyn CloneableIterable>>) -> Self {
        Self(items)
    }

    fn lengths(&self) -> Vec<usize> {
        self.0
            .iter()
            .map(|i| safe_clone(i).count())
            .collect::<Vec<usize>>()
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
                    .map(|a| a.expect_iterator(interpreter))
                    .collect::<Result<Vec<Box<dyn CloneableIterable>>, _>>()?;

                let zip = ZipIterator::new(iters);

                if args
                    .get_kwarg("strict")
                    .is_some_and(|k| k == TreewalkValue::Bool(true))
                {
                    let lengths = zip.lengths();
                    let all_equal = lengths.is_empty() || lengths.iter().all(|&x| x == lengths[0]);

                    if !all_equal {
                        return Err(interpreter.runtime_error());
                    }
                }

                zip
            }
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Zip(zip))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

#[allow(clippy::borrowed_box)]
fn safe_clone(i: &Box<dyn CloneableIterable>) -> Box<dyn CloneableIterable> {
    (**i).clone_box()
}
