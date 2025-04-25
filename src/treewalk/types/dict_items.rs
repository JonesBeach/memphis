use std::{
    collections::HashMap,
    fmt::{Display, Error, Formatter},
};

use crate::treewalk::{
    macros::*,
    types::{Dict, DictKeys, DictValues, Tuple},
    utils::{format_comma_separated_with, Contextual, ContextualPair},
    TreewalkInterpreter, TreewalkResult, TreewalkValue,
};

impl_iterable!(DictItemsIter);

#[derive(Debug, Default, PartialEq, Clone)]
pub struct DictItems {
    items: Vec<ContextualPair>,
}

impl DictItems {
    pub fn from_vec(
        items: Vec<TreewalkValue>,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let mut pairs = vec![];
        for item in items {
            let tuple = item.expect_tuple(interpreter)?;
            pairs.push((tuple.first(), tuple.second()));
        }
        Ok(Self::new(interpreter, pairs))
    }

    pub fn new(
        interpreter: &TreewalkInterpreter,
        items: Vec<(TreewalkValue, TreewalkValue)>,
    ) -> Self {
        let mut pairs = Vec::new();
        for (key, value) in items {
            let new_key = Contextual::new(key, interpreter.clone());
            pairs.push(ContextualPair::new(new_key, value));
        }

        Self::new_inner(pairs)
    }

    pub fn new_inner(items: Vec<ContextualPair>) -> Self {
        Self { items }
    }

    fn keys(&self) -> Vec<TreewalkValue> {
        self.items
            .iter()
            .map(|i| i.first_inner())
            .cloned()
            .collect()
    }

    fn values(&self) -> Vec<TreewalkValue> {
        self.items.iter().map(|i| i.second()).cloned().collect()
    }

    pub fn to_keys(&self) -> DictKeys {
        DictKeys::new(self.keys())
    }

    pub fn to_values(&self) -> DictValues {
        DictValues::new(self.values())
    }

    pub fn to_dict(&self) -> Dict {
        #[allow(clippy::mutable_key_type)]
        let mut items = HashMap::new();
        for pair in ContextualDictItemsIterator::new(self.clone()) {
            items.insert(pair.first().clone(), pair.second().clone());
        }

        Dict::new_inner(items)
    }

    pub fn fmt_as_mapping(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let items = self.items.clone();
        let formatted = format_comma_separated_with(items, |pair| {
            format!("'{}': {}", pair.first_inner(), pair.second())
        });
        write!(f, "{{{}}}", formatted)
    }
}

impl Display for DictItems {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = self.items.clone();
        let formatted = format_comma_separated_with(items, |pair| {
            format!("('{}', {})", pair.first_inner(), pair.second())
        });
        write!(f, "[{}]", formatted)
    }
}

impl IntoIterator for DictItems {
    type Item = TreewalkValue;
    type IntoIter = DictItemsIter;

    fn into_iter(self) -> Self::IntoIter {
        DictItemsIter::new(self)
    }
}

#[derive(Clone)]
pub struct DictItemsIter(DictItems);

impl DictItemsIter {
    pub fn new(dict_items: DictItems) -> Self {
        DictItemsIter(dict_items)
    }
}

impl Iterator for DictItemsIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            let key = removed.first_inner().clone();
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
