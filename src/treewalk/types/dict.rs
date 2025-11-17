use std::{
    collections::{hash_map::Keys, HashMap},
    fmt::{Debug, Display, Error, Formatter},
};

use crate::{
    core::Container,
    domain::{DomainResult, Dunder, ExecutionError, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead, IndexWrite, TryEvalFrom},
        result::Raise,
        types::{iterators::DictKeysIter, DictItems},
        utils::{check_args, Args, Contextual, ContextualPair},
        SymbolTable, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct Dict {
    items: HashMap<Contextual<TreewalkValue>, TreewalkValue>,
}

impl_typed!(Dict, Type::Dict);
impl_method_provider!(
    Dict,
    [
        NewBuiltin,
        InitBuiltin,
        GetBuiltin,
        DictKeysBuiltin,
        DictValuesBuiltin,
        DictItemsBuiltin,
    ]
);

impl Dict {
    #[allow(clippy::mutable_key_type)]
    pub fn new_inner(items: HashMap<Contextual<TreewalkValue>, TreewalkValue>) -> Self {
        Self { items }
    }

    #[allow(clippy::mutable_key_type)]
    pub fn new(
        interpreter: &TreewalkInterpreter,
        items: HashMap<TreewalkValue, TreewalkValue>,
    ) -> Self {
        let mut new_hash = HashMap::default();
        for (key, value) in items {
            let new_key = Contextual::new(key, interpreter.clone());
            new_hash.insert(new_key, value);
        }

        Self::new_inner(new_hash)
    }

    pub fn keys(&self) -> Keys<'_, Contextual<TreewalkValue>, TreewalkValue> {
        self.items.keys()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    fn get(
        &self,
        interpreter: TreewalkInterpreter,
        key: TreewalkValue,
        default: Option<TreewalkValue>,
    ) -> TreewalkValue {
        let default = default.unwrap_or(TreewalkValue::None);
        let key = Contextual::new(key, interpreter);
        self.items.get(&key).unwrap_or(&default).clone()
    }

    pub fn has(&self, interpreter: TreewalkInterpreter, key: &TreewalkValue) -> bool {
        let key = Contextual::new(key.clone(), interpreter);
        self.items.contains_key(&key)
    }

    /// Convert this to `DictItems`, which can subsequently become `DictKeys` or `DictValues`. This
    /// currently sorts the items before returning the object, which doesn't technically match
    /// Python's implementation, but makes our lives way easier.
    pub fn to_items(&self) -> DictItems {
        let mut items = Vec::with_capacity(self.items.len());

        for (ctx_key, value) in &self.items {
            items.push(ContextualPair::new(ctx_key.clone(), value.clone()));
        }

        items.sort();
        DictItems::new_inner(items)
    }

    /// Turn this `Dict` into a `SymbolTable`, which is another key-value store but where the keys
    /// are all confirmed to be valid Python identifiers.
    pub fn to_symbol_table(&self) -> DomainResult<SymbolTable> {
        let mut table = HashMap::new();

        let dict_items = self.to_items();
        for pair in dict_items {
            let tuple = pair.as_tuple()?;
            let key = tuple.first().as_str()?;
            let value = tuple.second();
            table.insert(key, value);
        }

        Ok(table)
    }

    pub fn extend(&mut self, other: &Dict) {
        for (key, value) in &other.items {
            self.items.insert(key.clone(), value.clone());
        }
    }
}

impl TryEvalFrom for Container<Dict> {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        match value {
            TreewalkValue::Dict(i) => Ok(i.clone()),
            val if val.clone().as_iterable().is_ok() => {
                let iter = val.as_iterator().raise(interpreter)?;
                let dict_items = DictItems::from_iterable(iter, interpreter).raise(interpreter)?;
                Ok(Container::new(dict_items.to_dict()))
            }
            _ => ExecutionError::type_error("Expected a dict").raise(interpreter),
        }
    }
}

impl Display for Container<Dict> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.borrow().to_items().fmt_as_mapping(f)
    }
}

/// We can reuse `DictKeysIterator` here because an iterator over a `Dict` will just return its
/// keys by default.
impl IntoIterator for Container<Dict> {
    type Item = TreewalkValue;
    type IntoIter = DictKeysIter;

    fn into_iter(self) -> Self::IntoIter {
        let dict_items = self.borrow().to_items();
        DictKeysIter::new(dict_items.to_keys())
    }
}

impl IndexRead for Container<Dict> {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
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
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        let index = Contextual::new(index, interpreter.clone());
        self.borrow_mut().items.insert(index, value);
        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<()> {
        let index = Contextual::new(index, interpreter.clone());
        self.borrow_mut().items.remove(&index);
        Ok(())
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct InitBuiltin;
#[derive(Clone)]
struct GetBuiltin;
#[derive(Clone)]
struct DictItemsBuiltin;
#[derive(Clone)]
struct DictKeysBuiltin;
#[derive(Clone)]
struct DictValuesBuiltin;

impl Callable for DictItemsBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;
        let dict_items = dict.borrow().to_items();
        Ok(TreewalkValue::DictItems(dict_items))
    }

    fn name(&self) -> String {
        "items".into()
    }
}

impl Callable for DictKeysBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;
        let dict_items = dict.borrow().to_items();
        Ok(TreewalkValue::DictKeys(dict_items.to_keys()))
    }

    fn name(&self) -> String {
        "keys".into()
    }
}

impl Callable for DictValuesBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0).raise(interpreter)?;
        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;
        let dict_items = dict.borrow().to_items();
        Ok(TreewalkValue::DictValues(dict_items.to_values()))
    }

    fn name(&self) -> String {
        "values".into()
    }
}

impl Callable for NewBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        Ok(TreewalkValue::Dict(Container::new(Dict::default())))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for InitBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [0, 1].contains(&len)).raise(interpreter)?;

        let output = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;

        if let Some(pos_arg) = args.get_arg_optional(0) {
            let input = Container::<Dict>::try_eval_from(pos_arg, interpreter)?;
            output.borrow_mut().extend(&input.borrow());
        }

        if args.has_kwargs() {
            let kwargs = args.get_kwargs_dict(interpreter);
            output.borrow_mut().extend(&kwargs);
        }

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

impl Callable for GetBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len)).raise(interpreter)?;

        let dict = args
            .get_self()
            .raise(interpreter)?
            .as_dict()
            .raise(interpreter)?;

        let key = args.get_arg(0);
        let default = args.get_arg_optional(1);

        let d = dict.borrow().clone();
        Ok(d.get(interpreter.clone(), key, default))
    }

    fn name(&self) -> String {
        "get".into()
    }
}
