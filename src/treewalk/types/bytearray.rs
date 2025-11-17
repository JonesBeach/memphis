use crate::{
    core::Container,
    domain::{Dunder, ExecutionError, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// A mutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct ByteArray(Vec<u8>);

impl_typed!(ByteArray, Type::ByteArray);
impl_method_provider!(ByteArray, [NewBuiltin]);

impl ByteArray {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self(bytes)
    }

    pub fn raw(&self) -> &[u8] {
        &self.0
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2, 3].contains(&len)).raise(interpreter)?;

        let byte_array = match args.len() {
            1 => Container::new(ByteArray::new("".into())),
            2 => match args.get_arg(1) {
                TreewalkValue::Str(_) => {
                    return ExecutionError::type_error("string argument without an encoding")
                        .raise(interpreter);
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
