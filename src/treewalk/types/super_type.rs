use crate::{
    core::{log, Container, LogLevel},
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MemberReader, MethodProvider, Typed},
        utils::Arguments,
        Interpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Debug, Clone)]
pub struct Super(TreewalkValue);

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
    pub fn new(receiver: TreewalkValue) -> Self {
        Self(receiver)
    }

    pub fn receiver(&self) -> TreewalkValue {
        self.0.clone()
    }
}

impl MemberReader for Container<Super> {
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>> {
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
    fn call(&self, interpreter: &Interpreter, _args: Arguments) -> TreewalkResult<TreewalkValue> {
        match interpreter.state.current_receiver() {
            None => {
                // If we are evaluating a static function, `super()` should just return the class the
                // function belongs to. This should only occur for `Dunder::New`. To my knowledge,
                // that is the only statically-bound function that permits calls to `super()`.
                let function = interpreter.state.current_function().unwrap();
                assert_eq!(function.borrow().name(), String::from(Dunder::New));

                let class = function.borrow().clone().class_context.unwrap();
                Ok(TreewalkValue::Super(Container::new(Super::new(
                    TreewalkValue::Class(class),
                ))))
            }
            Some(receiver) => Ok(TreewalkValue::Super(Container::new(Super::new(receiver)))),
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
