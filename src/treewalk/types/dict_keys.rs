use std::fmt::{Display, Error, Formatter};

use crate::treewalk::{types::Dict, TreewalkValue};

#[derive(Debug, PartialEq, Clone)]
pub struct DictKeys {
    items: Vec<TreewalkValue>,
}

impl DictKeys {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }
}

impl From<Dict> for DictKeys {
    fn from(dict: Dict) -> Self {
        let mut items = vec![];
        for item in dict.keys() {
            // Dereference `i` twice to get `TreewalkValue` (because `Deref` returns &TreewalkValue)
            items.push((**item).clone());
        }
        items.sort();
        DictKeys::new(items)
    }
}

impl Display for DictKeys {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = DictKeysIter::new(self.clone())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", items)
    }
}

impl IntoIterator for DictKeys {
    type Item = TreewalkValue;
    type IntoIter = DictKeysIter;

    fn into_iter(self) -> Self::IntoIter {
        DictKeysIter::new(self)
    }
}

#[derive(Clone)]
pub struct DictKeysIter(DictKeys);

impl DictKeysIter {
    pub fn new(dict_keys: DictKeys) -> Self {
        DictKeysIter(dict_keys)
    }
}

impl Iterator for DictKeysIter {
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
