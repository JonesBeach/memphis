use std::fmt::{Display, Error, Formatter};

use crate::treewalk::{macros::*, utils::format_comma_separated, TreewalkValue};

impl_iterable!(DictValuesIter);

#[derive(Debug, PartialEq, Clone)]
pub struct DictValues {
    items: Vec<TreewalkValue>,
}

impl DictValues {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }
}

impl Display for DictValues {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "[{}]", format_comma_separated(self.clone()))
    }
}

impl IntoIterator for DictValues {
    type Item = TreewalkValue;
    type IntoIter = DictValuesIter;

    fn into_iter(self) -> Self::IntoIter {
        DictValuesIter::new(self)
    }
}

#[derive(Clone)]
pub struct DictValuesIter(DictValues);

impl DictValuesIter {
    fn new(dict_values: DictValues) -> Self {
        DictValuesIter(dict_values)
    }
}

impl Iterator for DictValuesIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            Some(removed)
        }
    }
}
