use crate::{
    core::{log, LogLevel},
    treewalk::{
        result::Raise, type_system::CloneableCallable, types::Exception, TreewalkInterpreter,
        TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    pub fn load_var(&self, name: &str) -> TreewalkResult<TreewalkValue> {
        self.state
            .read(name)
            .ok_or_else(|| Exception::name_error(name))
            .raise(self)
    }

    pub fn load_callable(&self, name: &str) -> TreewalkResult<Box<dyn CloneableCallable>> {
        self.load_var(name)?.as_callable().raise(self)
    }

    pub fn load_index(
        &self,
        object: &TreewalkValue,
        index: &TreewalkValue,
    ) -> TreewalkResult<TreewalkValue> {
        object
            .clone()
            .into_index_read(self)?
            .ok_or_else(|| {
                Exception::type_error(format!(
                    "'{}' object is not subscriptable",
                    object.get_type()
                ))
            })
            .raise(self)?
            .getitem(self, index.clone())?
            .ok_or_else(|| Exception::key_error(index))
            .raise(self)
    }

    pub fn load_member<S>(&self, result: &TreewalkValue, field: S) -> TreewalkResult<TreewalkValue>
    where
        S: AsRef<str>,
    {
        log(LogLevel::Debug, || {
            format!("Member access {}.{}", result, field.as_ref())
        });
        result
            .clone()
            .into_member_reader(self)
            .get_member(self, field.as_ref())?
            .ok_or_else(|| Exception::attribute_error(result.class_name(self), field.as_ref()))
            .raise(self)
    }

    pub fn load_method<S>(
        &self,
        receiver: &TreewalkValue,
        name: S,
    ) -> TreewalkResult<Box<dyn CloneableCallable>>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        self.load_member(receiver, name)?.as_callable().raise(self)
    }
}
