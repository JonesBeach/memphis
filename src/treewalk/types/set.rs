use std::{
    collections::HashSet,
    fmt::{Display, Error, Formatter},
};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    builtins::utils,
    iterators::ListIterator,
    traits::Callable,
    utils::{Dunder, ResolvedArguments},
    ExprResult, FrozenSet, List, Range, Tuple,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Set {
    pub items: HashSet<ExprResult>,
}

impl Set {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![
            Box::new(NewBuiltin),
            Box::new(InitBuiltin),
            Box::new(AddBuiltin),
        ]
    }

    #[allow(clippy::mutable_key_type)]
    pub fn new(items: HashSet<ExprResult>) -> Self {
        Self { items }
    }

    pub fn default() -> Self {
        Self {
            items: HashSet::new(),
        }
    }

    pub fn add(&mut self, item: ExprResult) -> bool {
        self.items.insert(item)
    }
}

impl From<Container<List>> for Container<Set> {
    fn from(list: Container<List>) -> Container<Set> {
        Container::new(Set::new(list.into_iter().collect()))
    }
}

impl From<Container<Tuple>> for Container<Set> {
    fn from(tuple: Container<Tuple>) -> Container<Set> {
        Container::new(Set::new(tuple.into_iter().collect()))
    }
}

impl From<Container<Range>> for Container<Set> {
    fn from(range: Container<Range>) -> Container<Set> {
        Container::new(Set::new(range.into_iter().collect()))
    }
}

impl From<Container<FrozenSet>> for Container<Set> {
    fn from(frozenset: Container<FrozenSet>) -> Container<Set> {
        Container::new(Set::new(frozenset.borrow().clone().items))
    }
}

impl IntoIterator for Container<Set> {
    type Item = ExprResult;
    type IntoIter = ListIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self.into())
    }
}

impl Display for Container<Set> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = ListIterator::new(self.clone().into())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{{{}}}", items)
    }
}

struct NewBuiltin;
struct InitBuiltin;
struct AddBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Set(Container::new(Set::default())))
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
        let output_set = args
            .get_self()
            .ok_or(InterpreterError::ExpectedFunction(
                interpreter.state.call_stack(),
            ))?
            .as_set()
            .ok_or(InterpreterError::ExpectedSet(
                interpreter.state.call_stack(),
            ))?;

        if args.is_empty() {
            Ok(ExprResult::Void)
        } else if args.len() == 1 {
            let input_set = args
                .get_arg(0)
                .as_set()
                .ok_or(InterpreterError::ExpectedSet(
                    interpreter.state.call_stack(),
                ))?;

            *output_set.borrow_mut() = input_set.borrow().clone();
            Ok(ExprResult::Void)
        } else {
            Err(InterpreterError::WrongNumberOfArguments(
                1,
                args.len(),
                interpreter.state.call_stack(),
            ))
        }
    }

    fn name(&self) -> String {
        Dunder::Init.value().into()
    }
}

impl Callable for AddBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 1, interpreter.state.call_stack())?;

        let set = args
            .get_self()
            .ok_or(InterpreterError::ExpectedSet(
                interpreter.state.call_stack(),
            ))?
            .as_set()
            .ok_or(InterpreterError::ExpectedSet(
                interpreter.state.call_stack(),
            ))?;

        let result = set.borrow_mut().add(args.get_arg(0));

        Ok(ExprResult::Boolean(result))
    }

    fn name(&self) -> String {
        "add".into()
    }
}
