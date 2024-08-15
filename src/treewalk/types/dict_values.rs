use std::fmt::{Display, Error, Formatter};

use crate::core::Container;

use super::{domain::traits::IndexRead as _, Dict, ExprResult};

#[derive(Debug, PartialEq, Clone)]
pub struct DictValues {
    items: Vec<ExprResult>,
}

impl DictValues {
    pub fn new(items: Vec<ExprResult>) -> Self {
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
        let items = DictValuesIterator::new(self.clone())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", items)
    }
}

impl IntoIterator for DictValues {
    type Item = ExprResult;
    type IntoIter = DictValuesIterator;

    fn into_iter(self) -> Self::IntoIter {
        DictValuesIterator::new(self)
    }
}

#[derive(Clone)]
pub struct DictValuesIterator(DictValues);

impl DictValuesIterator {
    fn new(dict_values: DictValues) -> Self {
        DictValuesIterator(dict_values)
    }
}

impl Iterator for DictValuesIterator {
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            Some(removed)
        }
    }
}
