use crate::{
    core::Container,
    domain::{ExecutionError, Type},
    treewalk::{result::Raise, types::Class, TreewalkInterpreter, TreewalkResult, TreewalkValue},
};

impl TreewalkInterpreter {
    pub fn matches_exception_classes(
        &self,
        raised_type: &Type,
        exception_classes: &[Container<Class>],
    ) -> bool {
        let raised_class = self.state.class_of_type(raised_type);
        exception_classes
            .iter()
            .any(|handled| raised_class.is_subclass_of(handled))
    }

    pub fn exception_classes(
        &self,
        result: TreewalkValue,
    ) -> TreewalkResult<Vec<Container<Class>>> {
        let classes = match &result {
            TreewalkValue::Class(c) => vec![c.clone()],
            TreewalkValue::Tuple(_) => {
                let mut items = vec![];
                for item in result {
                    items.push(
                        item.as_class()
                            .map_err(|_| ExecutionError::type_error_must_inherit_base_exception())
                            .raise(self)?,
                    );
                }
                items
            }
            _ => ExecutionError::type_error_must_inherit_base_exception().raise(self)?,
        };

        let base_exception = self.state.class_of_type(&Type::BaseException);
        let is_base_exception = classes.iter().all(|c| c.is_subclass_of(&base_exception));
        if !is_base_exception {
            ExecutionError::type_error_must_inherit_base_exception().raise(self)?
        }

        Ok(classes)
    }
}
