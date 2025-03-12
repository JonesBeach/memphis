use crate::treewalk::interpreter::TreewalkResult;
use crate::{core::Container, domain::Dunder, treewalk::Interpreter};

use super::domain::builtins::utils;
use super::{
    domain::{
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult,
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct ByteArray(Vec<u8>);

impl Typed for ByteArray {
    fn get_type() -> Type {
        Type::ByteArray
    }
}

impl MethodProvider for ByteArray {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl ByteArray {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    pub fn raw(&self) -> &[u8] {
        &self.0
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

        let byte_array = match args.len() {
            1 => Container::new(ByteArray::new("".into())),
            2 => match args.get_arg(1) {
                ExprResult::String(_) => {
                    return Err(interpreter.type_error("string argument without an encoding"));
                }
                ExprResult::Bytes(s) => Container::new(ByteArray::new(s)),
                _ => todo!(),
            },
            // TODO support an optional encoding
            3 => todo!(),
            _ => unreachable!(),
        };

        Ok(ExprResult::ByteArray(byte_array))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
