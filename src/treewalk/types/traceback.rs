use crate::{
    core::Container,
    domain::Type,
    treewalk::{
        macros::*, protocols::NonDataDescriptor, types::Class, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct Traceback;

impl_typed!(Traceback, Type::Traceback);
impl_descriptor_provider!(Traceback, [FrameAttribute]);

#[derive(Clone)]
struct FrameAttribute;

impl NonDataDescriptor for FrameAttribute {
    fn get_attr(
        &self,
        _interpreter: &crate::treewalk::TreewalkInterpreter,
        _instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Frame)
    }

    fn name(&self) -> String {
        "tb_frame".into()
    }
}
