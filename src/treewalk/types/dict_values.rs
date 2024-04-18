use std::fmt::{Display, Error, Formatter};

use super::{Dict, ExprResult};

#[derive(Debug, PartialEq, Clone)]
pub struct DictValues {
    items: Vec<ExprResult>,
}

impl DictValues {
    pub fn new(items: Vec<ExprResult>) -> Self {
        Self { items }
    }
}

impl From<Dict> for DictValues {
    fn from(dict: Dict) -> Self {
        let mut items: Vec<ExprResult> = vec![];
        for i in dict.items.keys() {
            items.push(dict.items[i].clone());
        }
        // TODO this should support non-integer
        items.sort_by_key(|a| *a.as_integer().unwrap().borrow());
        DictValues::new(items)
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
