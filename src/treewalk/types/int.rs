use crate::{
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, MethodProvider, Typed},
        utils::{check_args, Arguments},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

pub struct Int;

impl Typed for Int {
    fn get_type() -> Type {
        Type::Int
    }
}

impl MethodProvider for Int {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
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
