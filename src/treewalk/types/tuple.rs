use std::fmt::{Display, Error, Formatter};

use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead, TryEvalFrom},
        utils::{check_args, format_comma_separated, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Tuple {
    items: Vec<TreewalkValue>,
}

impl_typed!(Tuple, Type::Tuple);
impl_method_provider!(Tuple, [NewBuiltin]);
impl_iterable!(TupleIter);

impl Tuple {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn items(&self) -> &[TreewalkValue] {
        &self.items
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
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

impl TryEvalFrom for Tuple {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let iter = value.expect_iterator(interpreter)?;
        Ok(Tuple::new(iter.collect()))
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "({})", format_comma_separated(self.clone()))
    }
}

impl IntoIterator for Tuple {
    type Item = TreewalkValue;
    type IntoIter = TupleIter;

    fn into_iter(self) -> Self::IntoIter {
        TupleIter::new(self)
    }
}

#[derive(Clone)]
pub struct TupleIter {
    list_ref: Tuple,
    current_index: usize,
}

impl TupleIter {
    pub fn new(list_ref: Tuple) -> Self {
        Self {
            list_ref,
            current_index: 0,
        }
    }
}

impl Iterator for TupleIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.list_ref.len() {
            None
        } else {
            self.current_index += 1;
            self.list_ref.items.get(self.current_index - 1).cloned()
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let set = match args.len() {
            1 => Tuple::default(),
            2 => Tuple::try_eval_from(args.get_arg(1), interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Tuple(set))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
