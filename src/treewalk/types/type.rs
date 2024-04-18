use crate::{
    core::Container,
    treewalk::{Interpreter, Scope},
    types::errors::InterpreterError,
};

use super::{
    builtins::utils,
    traits::{Callable, NonDataDescriptor},
    utils::{Dunder, ResolvedArguments},
    Class, ExprResult, MappingProxy, Tuple, Type,
};

/// This represents the callable class `type` in Python. For an enum of all the builtin types, see
/// `types::interpreter::Type`.
pub struct TypeClass;

impl TypeClass {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }

    pub fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>> {
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
        _interpreter: &Interpreter,
        instance: Option<ExprResult>,
        owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        let scope = match instance {
            Some(instance) => instance.as_class().unwrap().borrow().scope.clone(),
            None => owner.borrow().scope.clone(),
        };

        Ok(ExprResult::MappingProxy(Container::new(MappingProxy::new(
            scope.as_dict(),
        ))))
    }

    fn name(&self) -> String {
        Dunder::Dict.into()
    }
}

impl NonDataDescriptor for MroAttribute {
    fn get_attr(
        &self,
        _interpreter: &Interpreter,
        instance: Option<ExprResult>,
        owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        let mro = match instance {
            Some(instance) => instance
                .as_class()
                .unwrap()
                .mro()
                .iter()
                .cloned()
                .map(ExprResult::Class)
                .collect(),
            None => owner.mro().iter().cloned().map(ExprResult::Class).collect(),
        };
        Ok(ExprResult::Tuple(Container::new(Tuple::new(mro))))
    }

    fn name(&self) -> String {
        Dunder::Mro.into()
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        if args.len() == 5 {
            unimplemented!("Figure out how to handle kwargs for type::__new__.");
        }
        utils::validate_args(&args, 4, interpreter.state.call_stack())?;

        let mcls = args
            .get_arg(0)
            .as_class()
            .ok_or(InterpreterError::ExpectedClass(
                interpreter.state.call_stack(),
            ))?;
        let name = args
            .get_arg(1)
            .as_string()
            .ok_or(InterpreterError::ExpectedString(
                interpreter.state.call_stack(),
            ))?;
        // Default to the `Type::Object` class.
        let parent_classes = args
            .get_arg(2)
            .as_tuple()
            .ok_or(InterpreterError::ExpectedTuple(
                interpreter.state.call_stack(),
            ))?
            .into_iter()
            .map(|c| c.as_class().unwrap())
            .collect::<Vec<Container<Class>>>();

        let parent_classes = if parent_classes.is_empty() {
            vec![interpreter.state.get_type_class(Type::Object)]
        } else {
            parent_classes
        };

        let namespace = args
            .get_arg(3)
            .as_dict()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        Ok(ExprResult::Class(Class::new_base(
            name,
            parent_classes,
            Some(mcls),
            Scope::from_dict(namespace.clone().borrow().clone().into()),
        )))
    }

    fn name(&self) -> String {
        Dunder::New.value().into()
    }
}
