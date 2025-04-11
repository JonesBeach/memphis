use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MethodProvider, NonDataDescriptor, Typed},
        types::{Class, Method},
        utils::{check_args, Arguments},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct Classmethod(Container<Box<dyn Callable>>);

impl Typed for Classmethod {
    fn get_type() -> Type {
        Type::Classmethod
    }
}

impl MethodProvider for Classmethod {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Classmethod {
    fn new(func: Container<Box<dyn Callable>>) -> Self {
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

        // This is a workaround for Generic type behavior found in _collections_abc.py.
        // _weakrefset.py also uses this.
        //
        // ```
        // GenericAlias = type(list[int])
        // __class_getitem__ = classmethod(GenericAlias)
        // ```
        if args.get_arg(1).as_class().is_some() {
            return Ok(TreewalkValue::None);
        }

        let function = args.get_arg(1).expect_callable(interpreter)?;
        Ok(TreewalkValue::Classmethod(Classmethod::new(function)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl NonDataDescriptor for Classmethod {
    fn get_attr(
        &self,
        _interpreter: &TreewalkInterpreter,
        _instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Method(Container::new(Method::new(
            TreewalkValue::Class(owner),
            self.0.clone(),
        ))))
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}
