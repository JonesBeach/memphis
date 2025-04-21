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
        types::{iterators::ListIter, Set},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct FrozenSet {
    items: HashSet<TreewalkValue>,
}

impl_typed!(FrozenSet, Type::FrozenSet);
impl_method_provider!(FrozenSet, [NewBuiltin, ContainsBuiltin]);

impl FrozenSet {
    #[allow(clippy::mutable_key_type)]
    pub fn new(items: HashSet<TreewalkValue>) -> Self {
        Self { items }
    }

    #[allow(clippy::mutable_key_type)]
    pub fn cloned_items(&self) -> HashSet<TreewalkValue> {
        self.items.clone()
    }
}

impl From<Container<Set>> for FrozenSet {
    fn from(set: Container<Set>) -> FrozenSet {
        FrozenSet::new(set.borrow().cloned_items())
    }
}

impl IntoIterator for FrozenSet {
    type Item = TreewalkValue;
    type IntoIter = ListIter;

    fn into_iter(self) -> Self::IntoIter {
        let set: Container<Set> = self.into();
        ListIter::new(set.into())
    }
}

impl Display for FrozenSet {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let set: Container<Set> = self.clone().into();
        let items = ListIter::new(set.into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "frozenset({{{}}})", items)
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct ContainsBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let frozen_set = match args.len() {
            1 => FrozenSet::default(),
            2 => {
                let input_set: Container<Set> = args
                    .get_arg(1)
                    .try_into()
                    .map_err(|_| interpreter.type_error("Expected a set"))?;
                input_set.into()
            }
            _ => unreachable!(),
        };

        Ok(TreewalkValue::FrozenSet(frozen_set))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for ContainsBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!();
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}
