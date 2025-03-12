use crate::treewalk::interpreter::TreewalkResult;
use crate::{domain::Dunder, treewalk::Interpreter};

use super::{
    domain::{
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult,
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
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        if args.len() == 1 {
            Ok(ExprResult::Boolean(false))
        } else if args.len() == 2 {
            let input = args.get_arg(1).as_boolean();
            Ok(ExprResult::Boolean(input))
        } else {
            Err(interpreter.type_error(format!("Expected {} found {} args", 1, args.len())))
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
