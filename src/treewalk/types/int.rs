use crate::treewalk::interpreter::TreewalkResult;
use crate::{domain::Dunder, treewalk::Interpreter};

use super::domain::builtins::utils;
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
        utils::validate_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let int = match args.len() {
            1 => 0,
            2 => args.get_arg(1).expect_integer(interpreter)?,
            _ => unreachable!(),
        };

        Ok(ExprResult::Integer(int))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
