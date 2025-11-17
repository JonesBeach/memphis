use crate::{
    domain::{Dunder, Encoding, ExecutionError, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        types::Str,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// A immutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Bytes;

impl_typed!(Bytes, Type::Bytes);
impl_method_provider!(Bytes, [NewBuiltin, DecodeBuiltin]);

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct DecodeBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2, 3].contains(&len)).raise(interpreter)?;

        let bytes = match args.len() {
            1 => "".into(),
            2 => match args.get_arg(1) {
                TreewalkValue::Bytes(b) => b,
                TreewalkValue::Str(_) => {
                    return ExecutionError::type_error("string argument without an encoding")
                        .raise(interpreter);
                }
                _ => todo!(),
            },
            // TODO support an optional encoding
            3 => todo!(),
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Bytes(bytes))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for DecodeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [0, 1].contains(&len)).raise(interpreter)?;

        let encoding = match args.len() {
            0 => Encoding::default(),
            1 => {
                let encoding_str = args.get_arg(0).as_str().raise(interpreter)?;
                Encoding::try_from(encoding_str.as_str()).raise(interpreter)?
            }
            _ => unreachable!(),
        };

        let bytes = args
            .get_self()
            .raise(interpreter)?
            .as_bytes()
            .raise(interpreter)?;
        let str_value = Str::decode(&bytes, encoding).raise(interpreter)?;
        Ok(TreewalkValue::Str(str_value))
    }

    fn name(&self) -> String {
        "decode".into()
    }
}
