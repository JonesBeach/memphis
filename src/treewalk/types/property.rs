use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MethodProvider, NonDataDescriptor, Typed},
        types::Class,
        utils::{args, check_args, Arguments},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct Property(Container<Box<dyn Callable>>);

impl Typed for Property {
    fn get_type() -> Type {
        Type::Property
    }
}

impl MethodProvider for Property {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Property {
    fn new(function: Container<Box<dyn Callable>>) -> Self {
        Self(function)
    }
}

pub struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        // The first arg is the class itself, the second arg is the function
        check_args(&args, |len| len == 2, interpreter)?;
        let function = args.get_arg(1).expect_callable(interpreter)?;
        Ok(TreewalkValue::Property(Property::new(function)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Property {
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        let Some(instance) = instance else {
            panic!("No instance for descriptor!");
        };

        interpreter.call(self.0.clone(), &args![instance])
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
