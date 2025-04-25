use std::{
    cmp::Ordering,
    fmt::{Display, Error, Formatter},
};

use crate::treewalk::{utils::Contextual, TreewalkValue};

/// A helper to hold a pair of `TreewalkValue` objects but which enforces the first hold a reference
/// to its interpreter via a `ContextualTreewalkValue`. We need this for `DictItems` since the first
/// will become the keys of a `Dict`.
#[derive(Clone, PartialEq, Eq, Debug)]
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

    pub fn first_inner(&self) -> &TreewalkValue {
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

impl Ord for ContextualPair {
    fn cmp(&self, other: &Self) -> Ordering {
        self.key.cmp(&other.key)
    }
}

impl PartialOrd for ContextualPair {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
