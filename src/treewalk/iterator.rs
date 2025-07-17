use crate::{
    domain::ExecutionErrorKind,
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
            Err(TreewalkDisruption::Error(e))
                if matches!(e.execution_error_kind, ExecutionErrorKind::StopIteration(_)) =>
            {
                None
            }
            Err(_) => panic!("Unexpected error during generator run."),
        }
    }
}

impl IntoIterator for TreewalkValue {
    type Item = TreewalkValue;
    type IntoIter = Box<dyn CloneableIterable>;

    fn into_iter(self) -> Self::IntoIter {
        self.clone()
            .into_iterator()
            .unwrap_or_else(|| panic!("attempted to call IntoIterator on a {}!", self.get_type()))
    }
}
