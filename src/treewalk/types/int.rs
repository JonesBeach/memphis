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

pub struct Int;

impl Typed for Int {
    fn get_type() -> Type {
        Type::Int
    }
}

impl MethodProvider for Int {
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
            Ok(ExprResult::Integer(0))
        } else if args.len() == 2 {
            let input = args.get_arg(1).expect_integer(interpreter)?;
            Ok(ExprResult::Integer(input))
        } else {
            Err(interpreter.type_error(format!("Expected {}, found {} args", 1, args.len())))
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
