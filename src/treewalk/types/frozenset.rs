use crate::treewalk::interpreter::TreewalkResult;
use std::{
    collections::HashSet,
    fmt::{Display, Error, Formatter},
};

use crate::{core::Container, domain::Dunder, treewalk::Interpreter};

use super::{
    domain::{
        builtins::utils::validate_args,
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    iterators::ListIterator,
    utils::ResolvedArguments,
    ExprResult, Set,
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct FrozenSet {
    pub items: HashSet<ExprResult>,
}

impl Typed for FrozenSet {
    fn get_type() -> Type {
        Type::FrozenSet
    }
}

impl MethodProvider for FrozenSet {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin), Box::new(ContainsBuiltin)]
    }
}

impl FrozenSet {
    #[allow(clippy::mutable_key_type)]
    pub fn new(items: HashSet<ExprResult>) -> Self {
        Self { items }
    }
}

impl From<Container<Set>> for FrozenSet {
    fn from(set: Container<Set>) -> FrozenSet {
        FrozenSet::new(set.borrow().clone().items)
    }
}

impl IntoIterator for FrozenSet {
    type Item = ExprResult;
    type IntoIter = ListIterator;

    fn into_iter(self) -> Self::IntoIter {
        let set: Container<Set> = self.into();
        ListIterator::new(set.into())
    }
}

impl Display for FrozenSet {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let set: Container<Set> = self.clone().into();
        let items = ListIterator::new(set.into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "frozenset({{{}}})", items)
    }
}

struct NewBuiltin;
struct ContainsBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        validate_args(&args, |len| [1, 2].contains(&len), interpreter)?;

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

        Ok(ExprResult::FrozenSet(frozen_set))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for ContainsBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        unimplemented!();
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}
