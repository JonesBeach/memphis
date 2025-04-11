use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{DescriptorProvider, NonDataDescriptor, Typed},
        types::Class,
        TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, PartialEq, Clone)]
pub struct Exception;

impl Typed for Exception {
    fn get_type() -> Type {
        Type::Exception
    }
}

impl DescriptorProvider for Exception {
    fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
        vec![Box::new(TracebackAttribute)]
    }
}

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

#[derive(Debug, PartialEq, Clone)]
pub struct Traceback;

impl Typed for Traceback {
    fn get_type() -> Type {
        Type::Traceback
    }
}

impl DescriptorProvider for Traceback {
    fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
        vec![Box::new(FrameAttribute)]
    }
}

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
