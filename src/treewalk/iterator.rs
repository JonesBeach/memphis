use crate::{
    core::memphis_utils,
    domain::Type,
    treewalk::{
        protocols::Iterable, type_system::CloneableIterable, TreewalkDisruption, TreewalkResult,
        TreewalkValue,
    },
};

impl Iterable for Box<dyn CloneableIterable> {
    /// This should surface any `StopIteration` errors. Use `Iterator` to swallow them.
    fn try_next(&mut self) -> TreewalkResult<Option<TreewalkValue>> {
        self.as_mut().try_next()
    }
}

impl Iterator for Box<dyn CloneableIterable> {
    type Item = TreewalkValue;

    /// This should stop at any `StopIteration` errors. Use `Iterable` to surface them.
    fn next(&mut self) -> Option<Self::Item> {
        match Iterable::try_next(self) {
            Ok(v) => v,
            Err(TreewalkDisruption::Error(e)) if e.exception.get_type() == Type::StopIteration => {
                None
            }
            Err(TreewalkDisruption::Error(e)) => {
                // We must use the hard exit here because the Iterator trait doesn't give us
                // an interface to surface a runtime error.
                memphis_utils::exit(e.into());
            }
            Err(TreewalkDisruption::Signal(_)) => panic!("Unexpected signal during Iterator eval"),
        }
    }
}

impl IntoIterator for TreewalkValue {
    type Item = TreewalkValue;
    type IntoIter = Box<dyn CloneableIterable>;

    fn into_iter(self) -> Self::IntoIter {
        self.clone()
            .as_iterator()
            .unwrap_or_else(|_| panic!("attempted to call IntoIterator on a {}!", self.get_type()))
    }
}
