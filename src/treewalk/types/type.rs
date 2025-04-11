use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, DescriptorProvider, MethodProvider, NonDataDescriptor, Typed},
        types::{Class, MappingProxy, Tuple},
        utils::{check_args, Arguments},
        Scope, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// This represents the callable class `type` in Python. For an enum of all the builtin types, see
/// `types::interpreter::Type`.
pub struct TypeClass;

impl Typed for TypeClass {
    fn get_type() -> Type {
        Type::Type
    }
}

impl MethodProvider for TypeClass {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl DescriptorProvider for TypeClass {
    fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
        vec![Box::new(DictAttribute), Box::new(MroAttribute)]
    }
}

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
            Some(instance) => instance.as_class().unwrap().borrow().scope.clone(),
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
        _interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let mro = match instance {
            Some(instance) => instance
                .as_class()
                .unwrap()
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

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
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
            .map(|c| c.as_class().unwrap())
            .collect::<Vec<Container<Class>>>();

        let parent_classes = if parent_classes.is_empty() {
            vec![interpreter.state.get_type_class(Type::Object)]
        } else {
            parent_classes
        };

        let namespace = args.get_arg(3).expect_dict(interpreter)?;

        let scope = Scope::try_from(namespace.clone().borrow().clone())
            .map_err(|_| interpreter.type_error("Expected a dict"))?;

        Ok(TreewalkValue::Class(Class::new_base(
            name,
            parent_classes,
            Some(mcls),
            scope,
        )))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
