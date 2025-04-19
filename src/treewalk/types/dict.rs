use std::{
    collections::{hash_map::Keys, HashMap},
    fmt::{Debug, Display, Error, Formatter},
};

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead, IndexWrite},
        types::{
            dict_items::ContextualDictItemsIterator, iterators::DictKeysIter, DictItems, DictValues,
        },
        utils::{check_args, Args, Contextual},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
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
        FromKeysBuiltin
    ]
);

impl Dict {
    #[allow(clippy::mutable_key_type)]
    fn new_inner(items: HashMap<Contextual<TreewalkValue>, TreewalkValue>) -> Self {
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

    pub fn keys(&self) -> Keys<Contextual<TreewalkValue>, TreewalkValue> {
        self.items.keys()
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
}

impl From<Dict> for HashMap<Contextual<TreewalkValue>, TreewalkValue> {
    fn from(value: Dict) -> Self {
        value.items
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
    type Item = TreewalkValue;
    type IntoIter = DictKeysIter;

    fn into_iter(self) -> Self::IntoIter {
        DictKeysIter::new(self.borrow().clone().into())
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
#[derive(Clone)]
struct FromKeysBuiltin;

impl Callable for DictItemsBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;

        let dict = args.expect_self(interpreter)?.expect_dict(interpreter)?;
        let dict_items = DictItems::try_from(dict.clone().borrow().clone())
            .map_err(|_| interpreter.type_error("Expected a dict"))?;

        Ok(TreewalkValue::DictItems(dict_items))
    }

    fn name(&self) -> String {
        "items".into()
    }
}

impl Callable for DictKeysBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;
        let dict = args.expect_self(interpreter)?.expect_dict(interpreter)?;
        Ok(TreewalkValue::DictKeys(
            dict.clone().borrow().clone().into(),
        ))
    }

    fn name(&self) -> String {
        "keys".into()
    }
}

impl Callable for DictValuesBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;

        let dict = args.expect_self(interpreter)?.expect_dict(interpreter)?;

        let dict_values = DictValues::try_from(dict.clone().borrow().clone())
            .map_err(|_| interpreter.type_error("Expected a dict"))?;
        Ok(TreewalkValue::DictValues(dict_values))
    }

    fn name(&self) -> String {
        "values".into()
    }
}

impl Callable for FromKeysBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        "fromkeys".into()
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
        check_args(&args, |len| len == 1, interpreter)?;

        let output = args.expect_self(interpreter)?.expect_dict(interpreter)?;

        let input = args.get_arg(0).expect_dict(interpreter)?;

        *output.borrow_mut() = input.borrow().clone();

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        Dunder::Init.into()
    }
}

impl Callable for GetBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let dict = args.expect_self(interpreter)?.expect_dict(interpreter)?;

        let key = args.get_arg(0);
        let default = args.get_arg_optional(1);

        let d = dict.borrow().clone();
        Ok(d.get(interpreter.clone(), key, default))
    }

    fn name(&self) -> String {
        "get".into()
    }
}
