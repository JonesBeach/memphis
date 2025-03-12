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

/// A immutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Bytes;

impl Typed for Bytes {
    fn get_type() -> Type {
        Type::Bytes
    }
}

impl MethodProvider for Bytes {
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
        utils::validate_args(&args, |len| [1, 2, 3].contains(&len), interpreter)?;

        let bytes = match args.len() {
            1 => "".into(),
            2 => match args.get_arg(1) {
                ExprResult::Bytes(b) => b,
                ExprResult::String(_) => {
                    return Err(interpreter.type_error("string argument without an encoding"));
                }
                _ => todo!(),
            },
            // TODO support an optional encoding
            3 => todo!(),
            _ => unreachable!(),
        };

        Ok(ExprResult::Bytes(bytes))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
