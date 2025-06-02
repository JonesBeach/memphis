use crate::treewalk::{
    protocols::Iterable, type_system::CloneableIterable, TreewalkResult, TreewalkValue,
};

impl Iterable for Box<dyn CloneableIterable> {
    fn next(&mut self) -> TreewalkResult<Option<TreewalkValue>> {
        self.as_mut().next()
    }
}

impl Iterator for Box<dyn CloneableIterable> {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        Iterable::next(self).expect("Iterator evaluation failed.")
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
