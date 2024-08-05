use std::fmt::{Display, Error, Formatter};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    builtins::utils,
    iterators::ListIterator,
    traits::{Callable, IndexRead},
    utils::{Dunder, ResolvedArguments},
    ExprResult, List, Range, Set,
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Tuple {
    items: Vec<ExprResult>,
}

impl Tuple {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(NewBuiltin), Box::new(InitBuiltin)]
    }

    pub fn new(items: Vec<ExprResult>) -> Self {
        Self { items }
    }

    pub fn raw(&self) -> Vec<ExprResult> {
        self.items.clone()
    }
}

impl Container<Tuple> {
    fn get_item(&self, index: usize) -> Option<ExprResult> {
        self.borrow().items.get(index).cloned()
    }

    pub fn first(&self) -> ExprResult {
        self.get_item(0).expect("No first tuple element!")
    }

    pub fn second(&self) -> ExprResult {
        self.get_item(1).expect("No second tuple element!")
    }
}

impl IndexRead for Container<Tuple> {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        let i = index
            .as_integer_val()
            .ok_or(InterpreterError::ExpectedInteger(
                interpreter.state.call_stack(),
            ))?;
        Ok(self.get_item(i as usize))
    }
}

impl From<Container<Set>> for Container<Tuple> {
    fn from(set: Container<Set>) -> Container<Tuple> {
        // Calling `into_iter()` directly off the `Set` results in a stack overflow.
        //let mut items: Vec<ExprResult> = set.into_iter().collect();
        let mut items: Vec<ExprResult> = set.borrow().items.clone().into_iter().collect();
        items.sort_by_key(|x| *x.as_integer().unwrap().borrow());
        Container::new(Tuple::new(items))
    }
}

impl From<Container<List>> for Container<Tuple> {
    fn from(list: Container<List>) -> Container<Tuple> {
        Container::new(Tuple::new(list.into_iter().collect()))
    }
}

impl From<Container<Range>> for Container<Tuple> {
    fn from(range: Container<Range>) -> Container<Tuple> {
        Container::new(Tuple::new(range.into_iter().collect()))
    }
}

impl Display for Container<Tuple> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = ListIterator::new(self.clone().into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "({})", items)
    }
}

impl IntoIterator for Container<Tuple> {
    type Item = ExprResult;
    type IntoIter = ListIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self.into())
    }
}

struct NewBuiltin;
struct InitBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Tuple(Container::new(Tuple::default())))
    }

    fn name(&self) -> String {
        Dunder::New.value().into()
    }
}

impl Callable for InitBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let output = args
            .get_self()
            .ok_or(InterpreterError::ExpectedFunction(
                interpreter.state.call_stack(),
            ))?
            .as_tuple()
            .ok_or(InterpreterError::ExpectedTuple(
                interpreter.state.call_stack(),
            ))?;

        let input = args
            .get_arg(0)
            .as_tuple()
            .ok_or(InterpreterError::ExpectedTuple(
                interpreter.state.call_stack(),
            ))?;

        *output.borrow_mut() = input.borrow().clone();

        Ok(ExprResult::Void)
    }

    fn name(&self) -> String {
        Dunder::Init.value().into()
    }
}
