use std::{
    collections::HashSet,
    fmt::{Display, Error, Formatter},
};

use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, TryEvalFrom},
        result::Raise,
        types::iterators::SetIter,
        utils::{check_args, format_comma_separated, Args},
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

impl TryEvalFrom for FrozenSet {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let iter = value.as_iterator().raise(interpreter)?;
        Ok(FrozenSet::new(iter.collect()))
    }
}

impl IntoIterator for FrozenSet {
    type Item = TreewalkValue;
    type IntoIter = SetIter;

    fn into_iter(self) -> Self::IntoIter {
        let mut items: Vec<TreewalkValue> = self.cloned_items().into_iter().collect();
        items.sort();
        SetIter::new(items)
    }
}

impl Display for FrozenSet {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "frozenset({{{}}})", format_comma_separated(self.clone()))
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
            2 => FrozenSet::try_eval_from(args.get_arg(1), interpreter)?,
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
