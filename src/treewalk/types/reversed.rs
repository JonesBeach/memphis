use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead},
        types::List,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct ReversedIter {
    interpreter: TreewalkInterpreter,
    list_ref: Container<List>,
    current_index: usize,
}

impl_typed!(ReversedIter, Type::ReversedIter);
impl_method_provider!(ReversedIter, [NewBuiltin]);
impl_iterable!(ReversedIter);

impl ReversedIter {
    pub fn new(interpreter: TreewalkInterpreter, list_ref: Container<List>) -> Self {
        let current_index = list_ref.borrow().len();
        Self {
            interpreter,
            list_ref,
            current_index,
        }
    }
}

impl Iterator for ReversedIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == 0 {
            None
        } else {
            self.current_index -= 1;
            self.list_ref
                .getitem(
                    &self.interpreter,
                    TreewalkValue::Int(self.current_index as i64),
                )
                .unwrap()
        }
    }
}

#[derive(Clone)]
struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2, interpreter)?;
        let list = args.get_arg(1).expect_list(interpreter)?;
        Ok(TreewalkValue::ReversedIter(ReversedIter::new(
            interpreter.clone(),
            list.clone(),
        )))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
