use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        result::Raise,
        types::{Class, Dict, Str, Tuple},
        utils::args,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

impl TreewalkInterpreter {
    /// The primary public interface to create a class. A metaclass will be used if one is found to
    /// have a `Dunder::New` method, falling back to the `Type::Type` metaclass.
    pub fn build_class(
        &self,
        name: &str,
        parent_classes: Vec<Container<Class>>,
        metaclass: Option<Container<Class>>,
    ) -> TreewalkResult<Container<Class>> {
        let type_class = self.state.class_of_type(&Type::Type);
        let metaclass = Class::find_metaclass(metaclass, parent_classes.clone(), type_class);

        let bases = if parent_classes.is_empty() {
            TreewalkValue::Tuple(Tuple::default())
        } else {
            let bases = parent_classes
                .iter()
                .cloned()
                .map(TreewalkValue::Class)
                .collect::<Vec<TreewalkValue>>();
            TreewalkValue::Tuple(Tuple::new(bases))
        };

        let args = args![
            TreewalkValue::Class(metaclass.clone()),
            TreewalkValue::Str(Str::new(name)),
            bases,
            // class bodies start with a new empty dict, not a formal scope
            TreewalkValue::Dict(Container::new(Dict::default()))
        ];

        self.call_method(&TreewalkValue::Class(metaclass), Dunder::New, args)?
            .as_class()
            .raise(self)
    }
}
