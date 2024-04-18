use std::{
    collections::HashMap,
    fmt::{Display, Error, Formatter},
};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    builtins::utils,
    iterators::DictKeysIterator,
    traits::{Callable, IndexRead, IndexWrite},
    utils::{Dunder, ResolvedArguments},
    DictItems, ExprResult,
};

#[derive(Debug, PartialEq, Clone)]
pub struct Dict {
    pub items: HashMap<ExprResult, ExprResult>,
}

impl Dict {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![
            Box::new(NewBuiltin),
            Box::new(InitBuiltin),
            Box::new(DictKeysBuiltin),
            Box::new(DictValuesBuiltin),
            Box::new(DictItemsBuiltin),
            Box::new(FromKeysBuiltin),
        ]
    }

    pub fn new(items: HashMap<ExprResult, ExprResult>) -> Self {
        Self { items }
    }

    pub fn default() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub fn raw(&self) -> HashMap<ExprResult, ExprResult> {
        self.items.clone()
    }
}

impl IndexRead for Container<Dict> {
    fn getitem(
        &self,
        _interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        Ok(self.borrow().items.get(&index).cloned())
    }
}

impl IndexWrite for Container<Dict> {
    fn setitem(
        &mut self,
        _interpreter: &Interpreter,
        index: ExprResult,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        self.borrow_mut().items.insert(index, value);
        Ok(())
    }

    fn delitem(
        &mut self,
        _interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<(), InterpreterError> {
        self.borrow_mut().items.remove(&index);
        Ok(())
    }
}

impl From<DictItems> for Dict {
    fn from(dict: DictItems) -> Self {
        let mut items: HashMap<ExprResult, ExprResult> = HashMap::new();

        for i in dict {
            match i {
                ExprResult::Tuple(tuple) => {
                    items.insert(tuple.first(), tuple.second());
                }
                _ => panic!("expected a tuple!"),
            }
        }

        Dict::new(items)
    }
}

impl Display for Container<Dict> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = self
            .borrow()
            .items
            .iter()
            .map(|x| x.0.to_string() + ": " + &x.1.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{{{}}}", items)
    }
}

/// We can reuse `DictKeysIterator` here because an iterator over a `Dict` will just return its
/// keys by default.
impl IntoIterator for Container<Dict> {
    type Item = ExprResult;
    type IntoIter = DictKeysIterator;

    fn into_iter(self) -> Self::IntoIter {
        DictKeysIterator::new(self.borrow().clone().into())
    }
}

struct NewBuiltin;
struct InitBuiltin;
struct DictItemsBuiltin;
struct DictKeysBuiltin;
struct DictValuesBuiltin;
struct FromKeysBuiltin;

impl Callable for DictItemsBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 0, interpreter.state.call_stack())?;

        let dict = args
            .get_self()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?
            .as_dict()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        Ok(ExprResult::DictItems(dict.clone().borrow().clone().into()))
    }

    fn name(&self) -> String {
        "items".into()
    }
}

impl Callable for DictKeysBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 0, interpreter.state.call_stack())?;

        let dict = args
            .get_self()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?
            .as_dict()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        Ok(ExprResult::DictKeys(dict.clone().borrow().clone().into()))
    }

    fn name(&self) -> String {
        "keys".into()
    }
}

impl Callable for DictValuesBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        utils::validate_args(&args, 0, interpreter.state.call_stack())?;

        let dict = args
            .get_self()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?
            .as_dict()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        Ok(ExprResult::DictValues(dict.clone().borrow().clone().into()))
    }

    fn name(&self) -> String {
        "values".into()
    }
}

impl Callable for FromKeysBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        unimplemented!()
    }

    fn name(&self) -> String {
        "fromkeys".into()
    }
}

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        Ok(ExprResult::Dict(Container::new(Dict::default())))
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
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?
            .as_dict()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        let input = args
            .get_arg(0)
            .as_dict()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        *output.borrow_mut() = input.borrow().clone();

        Ok(ExprResult::Void)
    }

    fn name(&self) -> String {
        Dunder::Init.value().into()
    }
}
