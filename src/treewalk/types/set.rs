use std::{
    collections::HashSet,
    fmt::{Display, Error, Formatter},
};

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        types::{iterators::ListIterator, FrozenSet, List, Range, Tuple},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Set {
    items: HashSet<TreewalkValue>,
}

impl_typed!(Set, Type::Set);
impl_method_provider!(Set, [AddBuiltin, NewBuiltin]);

impl Set {
    #[allow(clippy::mutable_key_type)]
    pub fn new(items: HashSet<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn add(&mut self, item: TreewalkValue) -> bool {
        self.items.insert(item)
    }

    pub fn subset(&self, other: Set) -> bool {
        self.items.is_subset(&other.items)
    }

    #[allow(clippy::mutable_key_type)]
    pub fn cloned_items(&self) -> HashSet<TreewalkValue> {
        self.items.clone()
    }

    pub fn iter(&self) -> impl Iterator<Item = &TreewalkValue> {
        self.items.iter()
    }
}

impl TryFrom<TreewalkValue> for Container<Set> {
    type Error = ();

    fn try_from(value: TreewalkValue) -> Result<Self, Self::Error> {
        match value {
            TreewalkValue::Set(set) => Ok(set),
            TreewalkValue::List(list) => Ok(list.into()),
            TreewalkValue::Tuple(tuple) => Ok(tuple.into()),
            TreewalkValue::Range(range) => Ok(range.into()),
            _ => Err(()),
        }
    }
}

impl From<Container<List>> for Container<Set> {
    fn from(list: Container<List>) -> Container<Set> {
        Container::new(Set::new(list.into_iter().collect()))
    }
}

impl From<Tuple> for Container<Set> {
    fn from(tuple: Tuple) -> Container<Set> {
        Container::new(Set::new(tuple.into_iter().collect()))
    }
}

impl From<Range> for Container<Set> {
    fn from(range: Range) -> Container<Set> {
        Container::new(Set::new(range.into_iter().collect()))
    }
}

impl From<FrozenSet> for Container<Set> {
    fn from(frozenset: FrozenSet) -> Container<Set> {
        Container::new(Set::new(frozenset.cloned_items()))
    }
}

impl IntoIterator for Container<Set> {
    type Item = TreewalkValue;
    type IntoIter = ListIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self.into())
    }
}

impl Display for Container<Set> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = ListIterator::new(self.clone().into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{{{}}}", items)
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct AddBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let set = match args.len() {
            1 => Container::new(Set::default()),
            2 => args
                .get_arg(1)
                .try_into()
                .map_err(|_| interpreter.type_error("Expected a set".to_string()))?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Set(set))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let set = args.expect_self(interpreter)?.expect_set(interpreter)?;
        let result = set.borrow_mut().add(args.get_arg(0));

        Ok(TreewalkValue::Boolean(result))
    }

    fn name(&self) -> String {
        "add".into()
    }
}
