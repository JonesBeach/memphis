use crate::{
    core::Container,
    domain::Type,
    treewalk::{
        macros::*, protocols::NonDataDescriptor, types::Class, TreewalkInterpreter, TreewalkResult,
        TreewalkValue,
    },
};

impl_typed!(StopIteration, Type::StopIteration);
impl_descriptor_provider!(StopIteration, [ValueAttribute,]);

#[derive(Clone)]
pub struct StopIteration {
    payload: TreewalkValue,
}
impl StopIteration {
    pub fn new(payload: TreewalkValue) -> Self {
        Self { payload }
    }
}
#[derive(Clone)]
struct ValueAttribute;

impl NonDataDescriptor for ValueAttribute {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(match instance {
            Some(TreewalkValue::StopIteration(instance)) => instance.payload,
            None => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
            _ => TreewalkValue::NonDataDescriptor(Box::new(self.clone())),
        })
    }

    fn name(&self) -> String {
        "value".to_string()
    }
}
