use std::fmt::{Display, Error, Formatter};

use super::{Dict, ExprResult};

#[derive(Debug, PartialEq, Clone)]
pub struct DictKeys {
    items: Vec<ExprResult>,
}

impl DictKeys {
    pub fn new(items: Vec<ExprResult>) -> Self {
        Self { items }
    }
}

impl From<Dict> for DictKeys {
    fn from(dict: Dict) -> Self {
        let mut items = vec![];
        for item in dict.keys() {
            // Dereference `i` twice to get `ExprResult` (because `Deref` returns &ExprResult)
            items.push((**item).clone());
        }
        items.sort();
        DictKeys::new(items)
    }
}

impl Display for DictKeys {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = DictKeysIterator::new(self.clone())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", items)
    }
}

impl IntoIterator for DictKeys {
    type Item = ExprResult;
    type IntoIter = DictKeysIterator;

    fn into_iter(self) -> Self::IntoIter {
        DictKeysIterator::new(self)
    }
}

#[derive(Clone)]
pub struct DictKeysIterator(DictKeys);

impl DictKeysIterator {
    pub fn new(dict_keys: DictKeys) -> Self {
        DictKeysIterator(dict_keys)
    }
}

impl Iterator for DictKeysIterator {
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
