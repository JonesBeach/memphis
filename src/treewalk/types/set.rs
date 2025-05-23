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
        utils::{check_args, format_comma_separated, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Set {
    items: HashSet<TreewalkValue>,
}

impl_typed!(Set, Type::Set);
impl_method_provider!(Set, [AddBuiltin, NewBuiltin]);
impl_iterable!(SetIter);

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
        value
            .into_iterable()
            .map(|i| Container::new(Set::new(i.collect())))
            .ok_or(())
    }
}

impl Display for Container<Set> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{{{}}}", format_comma_separated(self.clone()))
    }
}

impl IntoIterator for Container<Set> {
    type Item = TreewalkValue;
    type IntoIter = SetIter;

    fn into_iter(self) -> Self::IntoIter {
        let mut items: Vec<TreewalkValue> = self.borrow().cloned_items().into_iter().collect();
        items.sort();
        SetIter::new(items)
    }
}

#[derive(Clone)]
pub struct SetIter {
    items: Vec<TreewalkValue>,
    current_index: usize,
}

impl SetIter {
    pub fn new(list_ref: Vec<TreewalkValue>) -> Self {
        Self {
            items: list_ref,
            current_index: 0,
        }
    }
}

impl Iterator for SetIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.items.len() {
            None
        } else {
            self.current_index += 1;
            self.items.get(self.current_index - 1).cloned()
        }
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

        Ok(TreewalkValue::Bool(result))
    }

    fn name(&self) -> String {
        "add".into()
    }
}
