use crate::treewalk::{
    types::iterators::{
        DictItemsIter, DictKeysIter, GeneratorIterator, ListIter, RangeIter, ReversedIter, StrIter,
        ZipIterator,
    },
    TreewalkValue,
};

#[derive(Clone)]
pub enum TreewalkIterator {
    List(ListIter),
    Zip(ZipIterator),
    Reversed(ReversedIter),
    Dict(DictKeysIter),
    DictItems(DictItemsIter),
    Generator(GeneratorIterator),
    Range(RangeIter),
    Str(StrIter),
}

impl TreewalkIterator {
    pub fn contains(&mut self, item: TreewalkValue) -> bool {
        for next_item in self.by_ref() {
            if next_item == item {
                return true;
            }
        }

        false
    }
}

impl Iterator for TreewalkIterator {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            TreewalkIterator::List(i) => i.next(),
            TreewalkIterator::Zip(i) => i.next(),
            TreewalkIterator::Reversed(i) => i.next(),
            TreewalkIterator::Dict(i) => i.next(),
            TreewalkIterator::DictItems(i) => i.next(),
            TreewalkIterator::Generator(i) => i.next(),
            TreewalkIterator::Range(i) => i.next(),
            TreewalkIterator::Str(i) => i.next(),
        }
    }
}

impl IntoIterator for TreewalkValue {
    type Item = TreewalkValue;
    type IntoIter = TreewalkIterator;

    fn into_iter(self) -> Self::IntoIter {
        self.clone()
            .try_into_iter()
            .unwrap_or_else(|| panic!("attempted to call IntoIterator on a {}!", self.get_type()))
    }
}
