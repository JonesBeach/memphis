use std::fmt::{Display, Error, Formatter};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    domain::{
        builtins::utils,
        traits::{Callable, IndexRead, MethodProvider, Typed},
        Type,
    },
    iterators::ListIterator,
    utils::{Dunder, ResolvedArguments},
    ExprResult, List, Range, Set,
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Tuple {
    items: Vec<ExprResult>,
}

impl Typed for Tuple {
    fn get_type() -> Type {
        Type::Tuple
    }
}

impl MethodProvider for Tuple {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin)]
    }
}

impl Tuple {
    pub fn new(items: Vec<ExprResult>) -> Self {
        Self { items }
    }

    pub fn raw(&self) -> Vec<ExprResult> {
        self.items.clone()
    }
}

impl Tuple {
    fn get_item(&self, index: usize) -> Option<ExprResult> {
        self.items.get(index).cloned()
    }

    pub fn first(&self) -> ExprResult {
        self.get_item(0).expect("No first tuple element!")
    }

    pub fn second(&self) -> ExprResult {
        self.get_item(1).expect("No second tuple element!")
    }
}

impl IndexRead for Tuple {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        let i = index.as_integer().ok_or(InterpreterError::ExpectedInteger(
            interpreter.state.call_stack(),
        ))?;
        Ok(self.get_item(i as usize))
    }
}

impl From<Container<Set>> for Tuple {
    fn from(set: Container<Set>) -> Tuple {
        // Calling `into_iter()` directly off the `Set` results in a stack overflow.
        //let mut items: Vec<ExprResult> = set.into_iter().collect();
        let mut items: Vec<ExprResult> = set.borrow().items.clone().into_iter().collect();
        items.sort_by_key(|x| x.as_integer().unwrap());
        Tuple::new(items)
    }
}

impl From<Container<List>> for Tuple {
    fn from(list: Container<List>) -> Tuple {
        Tuple::new(list.into_iter().collect())
    }
}

impl From<Range> for Tuple {
    fn from(range: Range) -> Tuple {
        Tuple::new(range.into_iter().collect())
    }
}

impl Display for Tuple {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = ListIterator::new(self.clone().into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "({})", items)
    }
}

impl IntoIterator for Tuple {
    type Item = ExprResult;
    type IntoIter = ListIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self.into())
    }
}

struct NewBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 2, interpreter.state.call_stack())?;

        let tuple = args
            .get_arg(1)
            .as_tuple()
            .ok_or(InterpreterError::ExpectedTuple(
                interpreter.state.call_stack(),
            ))?;
        Ok(ExprResult::Tuple(tuple))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}
