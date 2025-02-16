use std::{
    collections::HashSet,
    fmt::{Display, Error, Formatter},
};

use crate::{
    core::Container, domain::Dunder, treewalk::Interpreter, types::errors::InterpreterError,
};

use super::{
    domain::{
        builtins::utils,
        traits::{Callable, MethodProvider, Typed},
        Type,
    },
    iterators::ListIterator,
    utils::ResolvedArguments,
    ExprResult, FrozenSet, List, Range, Tuple,
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Set {
    pub items: HashSet<ExprResult>,
}

impl Typed for Set {
    fn get_type() -> Type {
        Type::Set
    }
}

impl MethodProvider for Set {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![
            Box::new(NewBuiltin),
            Box::new(InitBuiltin),
            Box::new(AddBuiltin),
        ]
    }
}

impl Set {
    #[allow(clippy::mutable_key_type)]
    pub fn new(items: HashSet<ExprResult>) -> Self {
        Self { items }
    }

    pub fn add(&mut self, item: ExprResult) -> bool {
        self.items.insert(item)
    }

    pub fn subset(&self, other: Set) -> bool {
        self.items.is_subset(&other.items)
    }
}

impl TryFrom<ExprResult> for Container<Set> {
    type Error = InterpreterError;

    fn try_from(value: ExprResult) -> Result<Self, Self::Error> {
        match value {
            ExprResult::Set(set) => Ok(set.clone()),
            ExprResult::List(list) => Ok(list.clone().into()),
            ExprResult::Tuple(tuple) => Ok(tuple.clone().into()),
            ExprResult::Range(range) => Ok(range.clone().into()),
            _ => Err(InterpreterError::RuntimeError),
        }
    }
}

impl From<Container<List>> for Container<Set> {
    fn from(list: Container<List>) -> Container<Set> {
        Container::new(Set::new(list.into_iter().collect()))
    }
}

impl From<Tuple> for Container<Set> {
    fn from(tuple: Tuple) -> Container<Set> {
        Container::new(Set::new(tuple.into_iter().collect()))
    }
}

impl From<Range> for Container<Set> {
    fn from(range: Range) -> Container<Set> {
        Container::new(Set::new(range.into_iter().collect()))
    }
}

impl From<FrozenSet> for Container<Set> {
    fn from(frozenset: FrozenSet) -> Container<Set> {
        Container::new(Set::new(frozenset.items))
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
        Dunder::New.into()
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
            Ok(ExprResult::None)
        } else if args.len() == 1 {
            let input_set: Container<Set> = args
                .get_arg(0)
                .try_into()
                .map_err(|_| InterpreterError::ExpectedSet(interpreter.state.call_stack()))?;

            *output_set.borrow_mut() = input_set.borrow().clone();
            Ok(ExprResult::None)
        } else {
            Err(InterpreterError::WrongNumberOfArguments(
                1,
                args.len(),
                interpreter.state.call_stack(),
            ))
        }
    }

    fn name(&self) -> String {
        Dunder::Init.into()
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
