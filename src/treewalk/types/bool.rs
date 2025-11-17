use crate::{
    domain::{Dunder, ExecutionError, Type},
    treewalk::{
        macros::*, protocols::Callable, result::Raise, utils::Args, TreewalkInterpreter,
        TreewalkResult, TreewalkValue,
    },
};

pub struct Bool;

impl_typed!(Bool, Type::Bool);
impl_method_provider!(Bool, [NewBuiltin]);

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        if args.len() == 1 {
            Ok(TreewalkValue::Bool(false))
        } else if args.len() == 2 {
            let input = args.get_arg(1).coerce_to_boolean();
            Ok(TreewalkValue::Bool(input))
        } else {
            ExecutionError::type_error(format!("Expected 1 found {} args", args.len()))
                .raise(interpreter)
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
