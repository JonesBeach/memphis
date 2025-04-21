use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

/// A immutable version of a byte string.
#[derive(Debug, Clone, PartialEq)]
pub struct Bytes;

impl_typed!(Bytes, Type::Bytes);
impl_method_provider!(Bytes, [NewBuiltin]);

#[derive(Clone)]
struct NewBuiltin;

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
