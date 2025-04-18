use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

pub struct Int;

impl_typed!(Int, Type::Int);
impl_method_provider!(Int, [NewBuiltin]);

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let int = match args.len() {
            1 => 0,
            2 => args.get_arg(1).expect_integer(interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Integer(int))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
