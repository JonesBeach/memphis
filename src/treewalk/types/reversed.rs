use crate::{
    core::Container,
    domain::Dunder,
    treewalk::{interpreter::TreewalkResult, Interpreter},
};

use super::{
    domain::{
        builtins::utils,
        traits::{Callable, IndexRead, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult, List,
};

#[derive(Clone)]
pub struct ReversedIterator {
    interpreter: Interpreter,
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
    pub fn new(interpreter: Interpreter, list_ref: Container<List>) -> Self {
        let current_index = list_ref.borrow().len();
        Self {
            interpreter,
            list_ref,
            current_index,
        }
    }
}

impl Iterator for ReversedIterator {
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == 0 {
            None
        } else {
            self.current_index -= 1;
            self.list_ref
                .getitem(
                    &self.interpreter,
                    ExprResult::Integer(self.current_index as i64),
                )
                .unwrap()
        }
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult> {
        utils::validate_args(&args, |len| len == 2, interpreter)?;
        let list = args.get_arg(1).expect_list(interpreter)?;
        Ok(ExprResult::ReversedIterator(ReversedIterator::new(
            interpreter.clone(),
            list.clone(),
        )))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
