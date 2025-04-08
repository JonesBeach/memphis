use crate::{
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MethodProvider, Typed},
        utils::Arguments,
        Interpreter, TreewalkResult, TreewalkValue,
    },
};

pub struct Bool;

impl Typed for Bool {
    fn get_type() -> Type {
        Type::Bool
    }
}

impl MethodProvider for Bool {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &Interpreter, args: Arguments) -> TreewalkResult<TreewalkValue> {
        if args.len() == 1 {
            Ok(TreewalkValue::Boolean(false))
        } else if args.len() == 2 {
            let input = args.get_arg(1).as_boolean();
            Ok(TreewalkValue::Boolean(input))
        } else {
            Err(interpreter.type_error(format!("Expected {} found {} args", 1, args.len())))
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
