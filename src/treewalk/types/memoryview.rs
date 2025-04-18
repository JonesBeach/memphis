use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*, protocols::Callable, utils::Args, TreewalkInterpreter, TreewalkResult,
        TreewalkValue,
    },
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Memoryview;

impl_typed!(Memoryview, Type::Memoryview);
impl_method_provider!(Memoryview, [NewBuiltin]);

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
