use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, NonDataDescriptor},
        types::{Class, MappingProxy, Tuple},
        utils::{check_args, Args},
        Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// This represents the callable class `type` in Python. For an enum of all the builtin types, see
/// `types::interpreter::Type`.
pub struct TypeClass;

impl_typed!(TypeClass, Type::Type);
impl_method_provider!(TypeClass, [NewBuiltin]);
impl_descriptor_provider!(TypeClass, [DictAttribute, MroAttribute]);

#[derive(Clone)]
struct DictAttribute;
#[derive(Clone)]
struct MroAttribute;

impl NonDataDescriptor for DictAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let scope = match instance {
            Some(instance) => instance.expect_class(interpreter)?.borrow().scope.clone(),
            None => owner.borrow().scope.clone(),
        };

        Ok(TreewalkValue::MappingProxy(MappingProxy::new(
            scope.as_dict(interpreter),
        )))
    }

    fn name(&self) -> String {
        Dunder::Dict.into()
    }
}

impl NonDataDescriptor for MroAttribute {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let mro = match instance {
            Some(instance) => instance
                .expect_class(interpreter)?
                .mro()
                .iter()
                .cloned()
                .map(TreewalkValue::Class)
                .collect(),
            None => owner
                .mro()
                .iter()
                .cloned()
                .map(TreewalkValue::Class)
                .collect(),
        };
        Ok(TreewalkValue::Tuple(Tuple::new(mro)))
    }

    fn name(&self) -> String {
        Dunder::Mro.into()
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        if args.len() == 5 {
            unimplemented!("Figure out how to handle kwargs for type::__new__.");
        }
        check_args(&args, |len| len == 4, interpreter)?;

        let mcls = args.get_arg(0).expect_class(interpreter)?;
        let name = args.get_arg(1).expect_string(interpreter)?;
        // Default to the `Type::Object` class.
        let parent_classes = args
            .get_arg(2)
            .expect_tuple(interpreter)?
            .into_iter()
            .map(|c| c.expect_class(interpreter))
            .collect::<Result<Vec<_>, _>>()?;

        let parent_classes = if parent_classes.is_empty() {
            vec![interpreter.state.class_of_type(&Type::Object)]
        } else {
            parent_classes
        };

        let symbol_table = args.get_arg(3).expect_symbol_table(interpreter)?;

        let mut class = Class::new_direct(name, Some(mcls), parent_classes);
        class.scope = Scope::new(symbol_table);
        Ok(TreewalkValue::Class(Container::new(class)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
