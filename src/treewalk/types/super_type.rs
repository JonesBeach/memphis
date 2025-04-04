use crate::treewalk::interpreter::TreewalkResult;
use crate::{
    core::{log, Container, LogLevel},
    domain::Dunder,
    treewalk::Interpreter,
};

use super::{
    domain::{
        traits::{Callable, MemberReader, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult,
};

#[derive(Debug, Clone)]
pub struct Super(ExprResult);

impl Typed for Super {
    fn get_type() -> Type {
        Type::Super
    }
}

impl MethodProvider for Super {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Super {
    pub fn new(receiver: ExprResult) -> Self {
        Self(receiver)
    }

    pub fn receiver(&self) -> ExprResult {
        self.0.clone()
    }
}

impl MemberReader for Container<Super> {
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<ExprResult>> {
        let instance = self.borrow().receiver();
        let class = instance.get_class(interpreter);

        // Retrieve the MRO for the class, excluding the class itself
        let super_mro = class.super_mro();
        let parent_class = super_mro
            .first()
            .ok_or_else(|| interpreter.type_error("Expected a class"))?;

        if let Some(attr) = parent_class.get_member(interpreter, name)? {
            log(LogLevel::Debug, || {
                format!("Found: {}::{} on class via super()", parent_class, name)
            });
            return Ok(Some(attr.resolve_nondata_descriptor(
                interpreter,
                Some(instance),
                class,
            )?));
        }

        Ok(None)
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        match interpreter.state.current_receiver() {
            None => {
                // If we are evaluating a static function, `super()` should just return the class the
                // function belongs to. This should only occur for `Dunder::New`. To my knowledge,
                // that is the only statically-bound function that permits calls to `super()`.
                let function = interpreter.state.current_function().unwrap();
                assert_eq!(function.borrow().name(), String::from(Dunder::New));

                let class = function.borrow().clone().class_context.unwrap();
                Ok(ExprResult::Super(Container::new(Super::new(
                    ExprResult::Class(class),
                ))))
            }
            Some(receiver) => Ok(ExprResult::Super(Container::new(Super::new(receiver)))),
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
