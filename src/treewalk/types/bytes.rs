use std::fmt::Display;

use crate::{
    domain::{Dunder, Type},
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Encoding {
    Utf8,
}

impl TryFrom<&str> for Encoding {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, ()> {
        match value {
            "utf-8" => Ok(Self::Utf8),
            _ => Err(()),
        }
    }
}

impl Display for Encoding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Utf8 => write!(f, "utf-8"),
        }
    }
}

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2, 3].contains(&len), interpreter)?;

        let bytes = match args.len() {
            1 => "".into(),
            2 => match args.get_arg(1) {
                TreewalkValue::Bytes(b) => b,
                TreewalkValue::Str(_) => {
                    return Err(interpreter.type_error("string argument without an encoding"));
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
        check_args(&args, |len| [0, 1].contains(&len), interpreter)?;

        let encoding = match args.len() {
            0 => Encoding::Utf8,
            1 => {
                let encoding_str = args.get_arg(0).expect_string(interpreter)?;
                Encoding::try_from(encoding_str.as_str())
                    .map_err(|_| interpreter.unknown_encoding(encoding_str))?
            }
            _ => unreachable!(),
        };

        let bytes = args.expect_self(interpreter)?.expect_bytes(interpreter)?;
        // TODO this is an experimental pattern to raise an ExecutionErrorKind after its initial
        // construction. We could use this everywhere.
        // For example:
        // let encoding_str = args.get_arg(0).expect_string().raise(interpreter)?;
        let str_value = Str::decode(&bytes, encoding).raise(interpreter)?;
        Ok(TreewalkValue::Str(str_value))
    }

    fn name(&self) -> String {
        "decode".into()
    }
}
