use std::fmt::{Display, Error, Formatter};

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, IndexRead, MethodProvider, Typed},
        types::{iterators::ListIterator, List, Range, Set},
        utils::{check_args, Arguments},
        Interpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Tuple {
    items: Vec<TreewalkValue>,
}

impl Typed for Tuple {
    fn get_type() -> Type {
        Type::Tuple
    }
}

impl MethodProvider for Tuple {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Tuple {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn raw(&self) -> Vec<TreewalkValue> {
        self.items.clone()
    }
}

impl Tuple {
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
        interpreter: &Interpreter,
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
        let mut items: Vec<TreewalkValue> = set.borrow().items.clone().into_iter().collect();
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
        let items = ListIterator::new(self.clone().into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "({})", items)
    }
}

impl IntoIterator for Tuple {
    type Item = TreewalkValue;
    type IntoIter = ListIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self.into())
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &Interpreter, args: Arguments) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2, interpreter)?;
        let tuple = args.get_arg(1).expect_tuple(interpreter)?;
        Ok(TreewalkValue::Tuple(tuple))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
