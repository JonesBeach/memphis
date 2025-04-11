use crate::{
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MethodProvider, Typed},
        utils::Arguments,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Memoryview;

impl Typed for Memoryview {
    fn get_type() -> Type {
        Type::Memoryview
    }
}

impl MethodProvider for Memoryview {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
