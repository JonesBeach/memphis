use std::fmt::{Display, Error, Formatter};

use crate::{
    core::{log, Container, LogLevel},
    treewalk::{Interpreter, Scope},
    types::errors::InterpreterError,
};

use super::{
    builtins::utils,
    traits::{Callable, IndexRead, IndexWrite, MemberAccessor, NonDataDescriptor},
    utils::{Dunder, ResolvedArguments},
    Class, ExprResult,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Object {
    pub class: Container<Class>,
    scope: Scope,
}

impl Object {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![
            Box::new(InitBuiltin),
            Box::new(NewBuiltin),
            Box::new(EqBuiltin),
            Box::new(NeBuiltin),
            Box::new(StrBuiltin),
        ]
    }

    /// Create the object with an empty symbol table. This is also called by the [`Dunder::New`]
    /// for [`Type::Object`] builtin.
    fn new_object_base(class: Container<Class>) -> Result<Container<Object>, InterpreterError> {
        Ok(Container::new(Self {
            class,
            scope: Scope::default(),
        }))
    }
}

impl IndexWrite for Container<Object> {
    fn setitem(
        &mut self,
        interpreter: &Interpreter,
        index: ExprResult,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        let _ = interpreter.evaluate_method(
            ExprResult::Object(self.clone()),
            Dunder::SetItem.value(),
            &ResolvedArguments::default().add_arg(index).add_arg(value),
        )?;

        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<(), InterpreterError> {
        let _ = interpreter.evaluate_method(
            ExprResult::Object(self.clone()),
            Dunder::DelItem.value(),
            &ResolvedArguments::default().add_arg(index),
        )?;

        Ok(())
    }
}

impl IndexRead for Container<Object> {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        let result = interpreter.evaluate_method(
            ExprResult::Object(self.clone()),
            Dunder::GetItem.value(),
            &ResolvedArguments::default().add_arg(index),
        )?;

        Ok(Some(result))
    }
}

impl MemberAccessor for Container<Object> {
    fn set_member(&mut self, name: &str, value: ExprResult) {
        self.borrow_mut().scope.insert(name, value);
    }

    fn delete_member(&mut self, name: &str) -> Option<ExprResult> {
        self.borrow_mut().scope.delete(name)
    }

    /// According to Python's rules, when searching for a member of an object, we must look at
    /// itself and its class (following its MRO), but NOT its class' metaclasses.
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        log(LogLevel::Debug, || {
            format!("Searching for: {}.{}", self, name)
        });

        if let Some(attr) = self.borrow().scope.get(name) {
            log(LogLevel::Debug, || {
                format!("Found: {}.{} on instance", self, name)
            });
            return Ok(Some(attr));
        }

        if let Some(attr) = self.borrow().class.get_from_class(name) {
            log(LogLevel::Debug, || {
                format!("Found: {}::{} on class", self.borrow().class, name)
            });
            let instance = ExprResult::Object(self.clone());
            let owner = instance.get_class(interpreter);
            return Ok(Some(attr.resolve_descriptor(
                interpreter,
                Some(instance),
                owner,
            )?));
        }

        Ok(None)
    }

    fn dir(&self) -> Vec<String> {
        let mut symbols = self.borrow().scope.symbols();
        symbols.sort();
        symbols
    }
}

/// This is for when an [`Object`] _is_ a descriptor, not when an object _has_ a descriptor.
impl NonDataDescriptor for Container<Object> {
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        owner: Container<Class>,
    ) -> Result<ExprResult, InterpreterError> {
        interpreter.evaluate_method(
            ExprResult::Object(self.clone()),
            Dunder::Get.value(),
            &ResolvedArguments::default()
                .add_arg(instance.unwrap_or(ExprResult::None))
                .add_arg(ExprResult::Class(owner)),
        )
    }

    fn name(&self) -> String {
        // We will not reach here because we do not insert this descriptor into a scope anywhere.
        // This is confusing but we'll eventually find a better way to combine
        // [`NonDataDescriptor`] and [`Callable`] for structs like this.
        unreachable!()
    }
}

impl Display for Container<Object> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(
            f,
            "<{} object at {:p}>",
            self.borrow().class.borrow().name,
            self
        )
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        // This is builtin for 'object' but the instance is created from the `cls` passed in as the
        // first argument.
        let class = args
            .get_arg(0)
            .as_class()
            .ok_or(InterpreterError::ExpectedClass(
                interpreter.state.call_stack(),
            ))?;

        Ok(ExprResult::Object(Object::new_object_base(class)?))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

struct InitBuiltin;

impl Callable for InitBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Void)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

/// The default behavior in Python for the `==` sign is to compare the object identity. This is
/// only used when `Dunder::Eq` is not overridden by a user-defined class.
struct EqBuiltin;

impl Callable for EqBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let a = args
            .get_self()
            .ok_or(InterpreterError::ExpectedObject(
                interpreter.state.call_stack(),
            ))?
            .as_object()
            .ok_or(InterpreterError::ExpectedObject(
                interpreter.state.call_stack(),
            ))?;

        let b = args
            .get_arg(0)
            .as_object()
            .ok_or(InterpreterError::ExpectedObject(
                interpreter.state.call_stack(),
            ))?;

        Ok(ExprResult::Boolean(a.same_identity(&b)))
    }

    fn name(&self) -> String {
        Dunder::Eq.into()
    }
}

/// The default behavior in Python for the `!=` sign is to call the `Dunder::Eq` and invert the
/// result. This is only used when `Dunder::Ne` is not overridden by a user-defined class.
struct NeBuiltin;

impl Callable for NeBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        let receiver = args.get_self().ok_or(InterpreterError::ExpectedObject(
            interpreter.state.call_stack(),
        ))?;
        let result = interpreter.evaluate_method(
            receiver,
            Dunder::Eq.value(),
            &ResolvedArguments::default().add_arg(args.get_arg(0)),
        )?;

        Ok(result.inverted())
    }

    fn name(&self) -> String {
        Dunder::Ne.into()
    }
}

struct StrBuiltin;

impl Callable for StrBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        unimplemented!()
    }

    fn name(&self) -> String {
        Dunder::Str.into()
    }
}
