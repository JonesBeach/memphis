use crate::treewalk::interpreter::{TreewalkDisruption, TreewalkResult};
use crate::{domain::Dunder, treewalk::Interpreter, types::errors::InterpreterError};

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
        match args.len() {
            1 => Ok(ExprResult::Bytes("".into())),
            2 => match args.get_arg(1) {
                ExprResult::String(_) => {
                    Err(interpreter.type_error("string argument without an encoding"))
                }
                ExprResult::Bytes(s) => Ok(ExprResult::Bytes(s)),
                _ => todo!(),
            },
            // TODO support an optional encoding
            3 => todo!(),
            _ => Err(interpreter.type_error(format!("Expected {}, found {} args", args.len(), 1,))),
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
