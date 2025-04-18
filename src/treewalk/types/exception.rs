use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::NonDataDescriptor,
        types::{Class, Traceback},
        TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct Exception;

impl_typed!(Exception, Type::Exception);
impl_descriptor_provider!(Exception, [TracebackAttribute]);

#[derive(Clone)]
struct TracebackAttribute;

impl NonDataDescriptor for TracebackAttribute {
    fn get_attr(
        &self,
        _interpreter: &crate::treewalk::TreewalkInterpreter,
        _instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Traceback(Traceback))
    }

    fn name(&self) -> String {
        Dunder::Traceback.into()
    }
}
