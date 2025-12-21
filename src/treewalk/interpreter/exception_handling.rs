use crate::{
    core::Container,
    domain::Type,
    treewalk::{types::Class, TreewalkInterpreter},
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
}
