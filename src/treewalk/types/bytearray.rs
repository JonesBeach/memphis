use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MethodProvider, Typed},
        utils::{check_args, Arguments},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
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
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2, 3].contains(&len), interpreter)?;

        let byte_array = match args.len() {
            1 => Container::new(ByteArray::new("".into())),
            2 => match args.get_arg(1) {
                TreewalkValue::String(_) => {
                    return Err(interpreter.type_error("string argument without an encoding"));
                }
                TreewalkValue::Bytes(s) => Container::new(ByteArray::new(s)),
                _ => todo!(),
            },
            // TODO support an optional encoding
            3 => todo!(),
            _ => unreachable!(),
        };

        Ok(TreewalkValue::ByteArray(byte_array))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
