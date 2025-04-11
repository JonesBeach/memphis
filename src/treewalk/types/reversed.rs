use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, IndexRead, MethodProvider, Typed},
        types::List,
        utils::{check_args, Arguments},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone)]
pub struct ReversedIterator {
    interpreter: TreewalkInterpreter,
    list_ref: Container<List>,
    current_index: usize,
}

impl Typed for ReversedIterator {
    fn get_type() -> Type {
        Type::ReversedIterator
    }
}

impl MethodProvider for ReversedIterator {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl ReversedIterator {
    pub fn new(interpreter: TreewalkInterpreter, list_ref: Container<List>) -> Self {
        let current_index = list_ref.borrow().len();
        Self {
            interpreter,
            list_ref,
            current_index,
        }
    }
}

impl Iterator for ReversedIterator {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == 0 {
            None
        } else {
            self.current_index -= 1;
            self.list_ref
                .getitem(
                    &self.interpreter,
                    TreewalkValue::Integer(self.current_index as i64),
                )
                .unwrap()
        }
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &TreewalkInterpreter,
        args: Arguments,
    ) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 2, interpreter)?;
        let list = args.get_arg(1).expect_list(interpreter)?;
        Ok(TreewalkValue::ReversedIterator(ReversedIterator::new(
            interpreter.clone(),
            list.clone(),
        )))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
