use crate::{
    core::{log, Container, LogLevel},
    treewalk::{protocols::MemberReader, types::Class, Interpreter, TreewalkResult, TreewalkValue},
};

/// Objects of builtin types are represented using different [`TreewalkValue`] variants, so we create
/// this abstraction to hold an object instance with its class. When we later find a method off its
/// class, we still need to be able to pass the instance into the descriptor.
///
/// We store this in the utils mod because these objects are shortlived, they really just need to
/// bridge the gap between the creation and resolution of a [`MemberAccessor`].
pub struct BuiltinObject {
    instance: TreewalkValue,
    class: Container<Class>,
}

impl BuiltinObject {
    pub fn new(instance: TreewalkValue, class: Container<Class>) -> Self {
        Self { instance, class }
    }
}

impl MemberReader for BuiltinObject {
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        log(LogLevel::Debug, || {
            format!("Searching for: {}.{}", self.class, name)
        });

        if let Some(attr) = self.class.get_from_class(name) {
            log(LogLevel::Debug, || {
                format!("Found: {}::{} on builtin class", self.class, name)
            });
            return Ok(Some(attr.resolve_nondata_descriptor(
                interpreter,
                Some(self.instance.clone()),
                self.class.clone(),
            )?));
        }

        Ok(None)
    }
}
