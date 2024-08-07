use std::{
    collections::HashMap,
    fmt::{Debug, Display, Error, Formatter},
    hash::{Hash, Hasher},
};

use crate::{
    core::Container, resolved_args, treewalk::Interpreter, types::errors::InterpreterError,
};

use super::{
    builtins::utils,
    iterators::DictKeysIterator,
    traits::{Callable, IndexRead, IndexWrite},
    utils::{Dunder, ResolvedArguments},
    DictItems, ExprResult,
};

#[derive(Clone)]
pub struct HashableKey {
    pub key: ExprResult,
    interpreter: Interpreter, // pointer to the interpreter (because of Hash/Eq traits)
}

impl HashableKey {
    pub fn new(key: ExprResult, interpreter: Interpreter) -> Self {
        Self { key, interpreter }
    }

    /// Use the interpreter to evaluate equality
    fn equals(&self, other: &Self) -> bool {
        self.interpreter
            .evaluate_method(
                self.key.clone(),
                Dunder::Eq.value(),
                &resolved_args!(other.key.clone()),
            )
            .unwrap()
            == ExprResult::Boolean(true)
    }

    /// Use the interpreter to evaluate the hash
    fn hash(&self) -> u64 {
        if let ExprResult::Integer(hash_val) = self
            .interpreter
            .evaluate_method(self.key.clone(), Dunder::Hash.value(), &resolved_args!())
            .unwrap()
        {
            *hash_val.borrow() as u64
        } else {
            panic!("__hash__ method did not return an integer");
        }
    }
}

impl Display for HashableKey {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.key)
    }
}

impl Debug for HashableKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.key)
    }
}

impl PartialEq for HashableKey {
    fn eq(&self, other: &Self) -> bool {
        self.equals(other)
    }
}

impl Eq for HashableKey {}

impl Hash for HashableKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash());
    }
}

pub struct HashablePair((HashableKey, ExprResult));

impl HashablePair {
    pub fn new(key: HashableKey, value: ExprResult) -> Self {
        Self((key, value))
    }

    pub fn first(&self) -> &HashableKey {
        &self.0 .0
    }

    pub fn first_resolved(&self) -> &ExprResult {
        &self.0 .0.key
    }

    pub fn second(&self) -> &ExprResult {
        &self.0 .1
    }
}

impl Display for HashablePair {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.0 .0)
    }
}

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Dict {
    pub items: HashMap<HashableKey, ExprResult>,
}

impl Dict {
    pub fn get_methods() -> Vec<Box<dyn Callable>> {
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

    #[allow(clippy::mutable_key_type)]
    fn new_inner(items: HashMap<HashableKey, ExprResult>) -> Self {
        Self { items }
    }

    #[allow(clippy::mutable_key_type)]
    pub fn new(interpreter: Interpreter, items: HashMap<ExprResult, ExprResult>) -> Self {
        let mut new_hash = HashMap::default();
        for (key, value) in items {
            let new_key = HashableKey::new(key.clone(), interpreter.clone());
            new_hash.insert(new_key, value);
        }

        Self::new_inner(new_hash)
    }

    fn get(
        &self,
        interpreter: Interpreter,
        key: ExprResult,
        default: Option<ExprResult>,
    ) -> ExprResult {
        let default = default.unwrap_or(ExprResult::None);
        let key = HashableKey::new(key.clone(), interpreter);
        self.items.get(&key).unwrap_or(&default).clone()
    }

    pub fn has(&self, interpreter: Interpreter, key: &ExprResult) -> bool {
        let key = HashableKey::new(key.clone(), interpreter);
        self.items.contains_key(&key)
    }

    #[allow(clippy::mutable_key_type)]
    pub fn raw(&self) -> HashMap<HashableKey, ExprResult> {
        self.items.clone()
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
        let index = HashableKey::new(index.clone(), interpreter.clone());
        self.borrow_mut().items.insert(index, value);
        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<(), InterpreterError> {
        let index = HashableKey::new(index.clone(), interpreter.clone());
        self.borrow_mut().items.remove(&index);
        Ok(())
    }
}

impl From<DictItems> for Dict {
    fn from(dict: DictItems) -> Self {
        let mut items = HashMap::new();

        for pair in dict {
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
            .as_dict()
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
