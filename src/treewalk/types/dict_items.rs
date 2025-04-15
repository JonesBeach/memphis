use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    treewalk::{
        protocols::IndexRead,
        types::{Dict, Tuple},
        utils::Contextual,
        TreewalkInterpreter, TreewalkValue,
    },
};

/// A helper to hold a pair of `TreewalkValue` objects but which enforces the first hold a reference
/// to its interpreter via a `ContextualTreewalkValue`. We need this for `DictItems` since the first
/// will become the keys of a `Dict`.
#[derive(Clone, PartialEq, Debug)]
pub struct ContextualPair {
    key: Contextual<TreewalkValue>,
    value: TreewalkValue,
}

impl ContextualPair {
    pub fn new(key: Contextual<TreewalkValue>, value: TreewalkValue) -> Self {
        Self { key, value }
    }

    pub fn first(&self) -> &Contextual<TreewalkValue> {
        &self.key
    }

    pub fn first_resolved(&self) -> &TreewalkValue {
        &self.key
    }

    pub fn second(&self) -> &TreewalkValue {
        &self.value
    }
}

impl Display for ContextualPair {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "contextual({}, {})", self.key, self.value)
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct DictItems {
    items: Vec<ContextualPair>,
}

impl DictItems {
    pub fn new(
        interpreter: &TreewalkInterpreter,
        items: Vec<(TreewalkValue, TreewalkValue)>,
    ) -> Self {
        let mut new_hash = Vec::new();
        for (key, value) in items {
            let new_key = Contextual::new(key, interpreter.clone());
            new_hash.push(ContextualPair::new(new_key, value));
        }

        Self::new_inner(new_hash)
    }

    fn new_inner(items: Vec<ContextualPair>) -> Self {
        Self { items }
    }
}

pub struct DictItemsError;

impl TryFrom<Dict> for DictItems {
    type Error = DictItemsError;

    fn try_from(dict: Dict) -> Result<Self, Self::Error> {
        let mut items = vec![];
        let stored = Container::new(dict.clone());
        for item in dict.keys() {
            let value = stored
                .getitem(item.interpreter(), (**item).clone())
                .map_err(|_| DictItemsError)?
                .ok_or(DictItemsError)?;
            items.push(ContextualPair::new(item.clone(), value));
        }
        items.sort_by(|a, b| a.first_resolved().cmp(b.first_resolved()));
        Ok(DictItems::new_inner(items))
    }
}

impl Display for DictItems {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = DictItemsIterator::new(self.clone())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", items)
    }
}

impl IntoIterator for DictItems {
    type Item = TreewalkValue;
    type IntoIter = DictItemsIterator;

    fn into_iter(self) -> Self::IntoIter {
        DictItemsIterator::new(self)
    }
}

#[derive(Clone)]
pub struct DictItemsIterator(DictItems);

impl DictItemsIterator {
    fn new(dict_items: DictItems) -> Self {
        DictItemsIterator(dict_items)
    }
}

impl Iterator for DictItemsIterator {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            let key = removed.first_resolved().clone();
            let value = removed.second().clone();
            let tuple = TreewalkValue::Tuple(Tuple::new(vec![key, value]));
            Some(tuple)
        }
    }
}

/// An iterator for `DictItems` when we need to return a `Contextual<TreewalkValue>` instead of an
/// `TreewalkValue`. This is useful in the conversation between `DictItems` and `Dict`.
#[derive(Clone)]
pub struct ContextualDictItemsIterator(DictItems);

impl ContextualDictItemsIterator {
    pub fn new(dict_items: DictItems) -> Self {
        ContextualDictItemsIterator(dict_items)
    }
}

impl Iterator for ContextualDictItemsIterator {
    type Item = ContextualPair;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            Some(removed)
        }
    }
}
