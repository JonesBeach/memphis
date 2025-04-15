use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MethodProvider, NonDataDescriptor, Typed},
        types::Class,
        utils::{check_args, Arguments},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct Staticmethod(Box<TreewalkValue>);

impl Typed for Staticmethod {
    fn get_type() -> Type {
        Type::Staticmethod
    }
}

impl MethodProvider for Staticmethod {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Staticmethod {
    fn new(func: Box<TreewalkValue>) -> Self {
        Self(func)
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
        let function = args.get_arg(1);
        Ok(TreewalkValue::Staticmethod(Staticmethod::new(Box::new(
            function,
        ))))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Staticmethod {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: Option<TreewalkValue>,
        _owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(*self.0.clone())
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
