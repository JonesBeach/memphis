use crate::{
    domain::{Dunder, ExecutionError, Type},
    treewalk::{
        macros::*,
        protocols::Callable,
        result::Raise,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct TypeError;

impl_typed!(TypeError, Type::TypeError);
impl_method_provider!(TypeError, [NewBuiltin,]);

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len >= 1).raise(interpreter)?;

        // TODO We should generalize this better, this actually takes any number of args
        let e = match args.len() {
            0 => unreachable!(),
            1 => ExecutionError::type_error_empty(),
            _ => {
                let msg = args.get_arg(1);
                ExecutionError::type_error(&msg)
            }
        };
        Ok(TreewalkValue::Exception(e))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
