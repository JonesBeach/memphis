use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    treewalk::{protocols::IndexRead, types::Dict, TreewalkValue},
};

#[derive(Debug, PartialEq, Clone)]
pub struct DictValues {
    items: Vec<TreewalkValue>,
}

impl DictValues {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }
}

pub struct DictValuesError;

impl TryFrom<Dict> for DictValues {
    type Error = DictValuesError;

    fn try_from(dict: Dict) -> Result<Self, Self::Error> {
        let mut items = vec![];
        let stored = Container::new(dict.clone());
        for item in dict.keys() {
            let value = stored
                .getitem(item.interpreter(), (**item).clone())
                .map_err(|_| DictValuesError)?
                .ok_or(DictValuesError)?;
            items.push(value);
        }
        items.sort();
        Ok(DictValues::new(items))
    }
}

impl Display for DictValues {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = DictValuesIter::new(self.clone())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", items)
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
