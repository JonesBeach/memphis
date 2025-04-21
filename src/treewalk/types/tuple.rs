use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead},
        types::{iterators::ListIter, List, Range, Set},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Tuple {
    items: Vec<TreewalkValue>,
}

impl_typed!(Tuple, Type::Tuple);
impl_method_provider!(Tuple, [NewBuiltin]);

impl Tuple {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[TreewalkValue] {
        &self.items
    }

    fn get_item(&self, index: usize) -> Option<TreewalkValue> {
        self.items.get(index).cloned()
    }

    pub fn first(&self) -> TreewalkValue {
        self.get_item(0).expect("No first tuple element!")
    }

    pub fn second(&self) -> TreewalkValue {
        self.get_item(1).expect("No second tuple element!")
    }
}

impl IndexRead for Tuple {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        let i = index.expect_integer(interpreter)?;
        Ok(self.get_item(i as usize))
    }
}

impl From<Container<Set>> for Tuple {
    fn from(set: Container<Set>) -> Tuple {
        // Calling `into_iter()` directly off the `Set` results in a stack overflow.
        //let mut items: Vec<TreewalkValue> = set.into_iter().collect();
        let mut items: Vec<TreewalkValue> = set.borrow().cloned_items().into_iter().collect();
        // TODO remove unwrap
        items.sort_by_key(|x| x.as_integer().unwrap());
        Tuple::new(items)
    }
}

impl From<Container<List>> for Tuple {
    fn from(list: Container<List>) -> Tuple {
        Tuple::new(list.into_iter().collect())
    }
}

impl From<Range> for Tuple {
    fn from(range: Range) -> Tuple {
        Tuple::new(range.into_iter().collect())
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = ListIter::new(self.clone().into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "({})", items)
    }
}

impl IntoIterator for Tuple {
    type Item = TreewalkValue;
    type IntoIter = ListIter;

    fn into_iter(self) -> Self::IntoIter {
        ListIter::new(self.into())
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2, interpreter)?;
        let tuple = args.get_arg(1).expect_tuple(interpreter)?;
        Ok(TreewalkValue::Tuple(tuple))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
