use crate::{
    core::{log, LogLevel},
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, MemberRead},
        result::Raise,
        types::Exception,
        utils::Args,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, Clone)]
pub struct Super(Box<TreewalkValue>);

impl_typed!(Super, Type::Super);
impl_method_provider!(Super, [NewBuiltin]);

impl Super {
    pub fn new(receiver: TreewalkValue) -> Self {
        Self(Box::new(receiver))
    }

    pub fn receiver(&self) -> TreewalkValue {
        *self.0.clone()
    }
}

impl MemberRead for Super {
    fn get_member(
        &self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        let instance = self.receiver();
        let class = instance.get_class(interpreter);

        // Retrieve the MRO for the class, excluding the class itself
        let super_mro = class.super_mro();
        let parent_class = super_mro
            .first()
            .ok_or_else(|| Exception::type_error("Expected a class"))
            .raise(interpreter)?;

        if let Some(attr) = parent_class.get_member(interpreter, name)? {
            log(LogLevel::Debug, || {
                format!("Found: {parent_class}::{name} on class via super()")
            });
            return Ok(Some(attr.resolve_descriptor(
                interpreter,
                Some(instance),
                class,
            )?));
        }

        Ok(None)
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        match interpreter.state.current_receiver() {
            None => {
                // If we are evaluating a static function, `super()` should just return the class the
                // function belongs to. This should only occur for `Dunder::New`. To my knowledge,
                // that is the only statically-bound function that permits calls to `super()`.
                let function = interpreter.state.current_function().unwrap();
                assert_eq!(function.borrow().name(), String::from(Dunder::New));

                let class = function.borrow().clone().class_context.unwrap();
                Ok(TreewalkValue::Super(Super::new(TreewalkValue::Class(
                    class,
                ))))
            }
            Some(receiver) => Ok(TreewalkValue::Super(Super::new(receiver))),
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
