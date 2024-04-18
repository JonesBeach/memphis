use std::fmt::{Display, Error, Formatter};

use crate::core::Container;

use super::{Dict, ExprResult, List, Tuple};

#[derive(Debug, PartialEq, Clone)]
pub struct DictItems {
    items: Vec<(ExprResult, ExprResult)>,
}

impl DictItems {
    pub fn new(items: Vec<(ExprResult, ExprResult)>) -> Self {
        Self { items }
    }
}

impl From<Dict> for DictItems {
    fn from(dict: Dict) -> Self {
        let mut items: Vec<(ExprResult, ExprResult)> = vec![];
        for i in dict.items.keys() {
            items.push((i.clone(), dict.items[i].clone()));
        }
        // TODO this should support non-strings
        items.sort_by(|a, b| a.0.as_string().unwrap().cmp(&b.0.as_string().unwrap()));
        DictItems::new(items)
    }
}

impl From<Container<List>> for DictItems {
    fn from(list: Container<List>) -> Self {
        let mut items: Vec<(ExprResult, ExprResult)> = vec![];
        for i in list {
            match i {
                ExprResult::Tuple(tuple) => {
                    items.push((tuple.first(), tuple.second()));
                }
                _ => panic!("expected a tuple!"),
            }
        }
        DictItems::new(items)
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
    type Item = ExprResult;
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
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.items.is_empty() {
            None
        } else {
            let removed = self.0.items.remove(0);
            Some(ExprResult::Tuple(Container::new(Tuple::new(vec![
                removed.0, removed.1,
            ]))))
        }
    }
}
