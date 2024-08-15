use std::{
    collections::{hash_map::Keys, HashMap},
    fmt::{Debug, Display, Error, Formatter},
};

use crate::{core::Container, treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    dict_items::ContextualDictItemsIterator,
    domain::{
        builtins::utils,
        traits::{Callable, IndexRead, IndexWrite, MethodProvider, Typed},
        Type,
    },
    iterators::DictKeysIterator,
    utils::{Contextual, Dunder, ResolvedArguments},
    DictItems, DictValues, ExprResult,
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Dict {
    items: HashMap<Contextual<ExprResult>, ExprResult>,
}

impl Typed for Dict {
    fn get_type() -> Type {
        Type::Dict
    }
}

impl MethodProvider for Dict {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![
            Box::new(NewBuiltin),
            Box::new(InitBuiltin),
            Box::new(GetBuiltin),
            Box::new(DictKeysBuiltin),
            Box::new(DictValuesBuiltin),
            Box::new(DictItemsBuiltin),
            Box::new(FromKeysBuiltin),
        ]
    }
}

impl Dict {
    #[allow(clippy::mutable_key_type)]
    fn new_inner(items: HashMap<Contextual<ExprResult>, ExprResult>) -> Self {
        Self { items }
    }

    #[allow(clippy::mutable_key_type)]
    pub fn new(interpreter: Interpreter, items: HashMap<ExprResult, ExprResult>) -> Self {
        let mut new_hash = HashMap::default();
        for (key, value) in items {
            let new_key = Contextual::new(key, interpreter.clone());
            new_hash.insert(new_key, value);
        }

        Self::new_inner(new_hash)
    }

    pub fn keys(&self) -> Keys<Contextual<ExprResult>, ExprResult> {
        self.items.keys()
    }

    fn get(
        &self,
        interpreter: Interpreter,
        key: ExprResult,
        default: Option<ExprResult>,
    ) -> ExprResult {
        let default = default.unwrap_or(ExprResult::None);
        let key = Contextual::new(key, interpreter);
        self.items.get(&key).unwrap_or(&default).clone()
    }

    pub fn has(&self, interpreter: Interpreter, key: &ExprResult) -> bool {
        let key = Contextual::new(key.clone(), interpreter);
        self.items.contains_key(&key)
    }
}

impl From<Dict> for HashMap<Contextual<ExprResult>, ExprResult> {
    fn from(value: Dict) -> Self {
        value.items
    }
}

impl IndexRead for Container<Dict> {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        if self.borrow().has(interpreter.clone(), &index) {
            Ok(Some(self.borrow().get(interpreter.clone(), index, None)))
        } else {
            Ok(None)
        }
    }
}

impl IndexWrite for Container<Dict> {
    fn setitem(
        &mut self,
        interpreter: &Interpreter,
        index: ExprResult,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        let index = Contextual::new(index, interpreter.clone());
        self.borrow_mut().items.insert(index, value);
        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<(), InterpreterError> {
        let index = Contextual::new(index, interpreter.clone());
        self.borrow_mut().items.remove(&index);
        Ok(())
    }
}

impl From<DictItems> for Dict {
    fn from(dict_items: DictItems) -> Self {
        #[allow(clippy::mutable_key_type)]
        let mut items = HashMap::new();
        for pair in ContextualDictItemsIterator::new(dict_items) {
            items.insert(pair.first().clone(), pair.second().clone());
        }

        Dict::new_inner(items)
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
struct GetBuiltin;
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
            .as_dict(interpreter)
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        let dict_items = DictItems::try_from(dict.clone().borrow().clone())
            .map_err(|_| InterpreterError::ExpectedDict(interpreter.state.call_stack()))?;
        Ok(ExprResult::DictItems(dict_items))
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
            .as_dict(interpreter)
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
            .as_dict(interpreter)
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        let dict_values = DictValues::try_from(dict.clone().borrow().clone())
            .map_err(|_| InterpreterError::ExpectedDict(interpreter.state.call_stack()))?;
        Ok(ExprResult::DictValues(dict_values))
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
        Dunder::New.into()
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
            .as_dict(interpreter)
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        let input = args
            .get_arg(0)
            .as_dict(interpreter)
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        *output.borrow_mut() = input.borrow().clone();

        Ok(ExprResult::Void)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

impl Callable for GetBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        if ![1, 2].contains(&args.len()) {
            utils::validate_args(&args, 1, interpreter.state.call_stack())?;
        }

        let dict = args
            .get_self()
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?
            .as_dict(interpreter)
            .ok_or(InterpreterError::ExpectedDict(
                interpreter.state.call_stack(),
            ))?;

        let key = args.get_arg(0);
        let default = args.get_arg_optional(1);

        let d = dict.borrow().clone();
        Ok(d.get(interpreter.clone(), key, default))
    }

    fn name(&self) -> String {
        "get".into()
    }
}
