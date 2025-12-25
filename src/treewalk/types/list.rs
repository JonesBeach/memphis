use std::{
    collections::VecDeque,
    fmt::{Display, Error, Formatter},
    ops::Add,
};

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead, IndexWrite, TryEvalFrom},
        result::Raise,
        type_system::CloneableIterable,
        types::Slice,
        utils::{check_args, format_comma_separated, Args},
        DomainResult, TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct List {
    items: Vec<TreewalkValue>,
}

impl_typed!(List, Type::List);
impl_method_provider!(
    List,
    [NewBuiltin, AddBuiltin, AppendBuiltin, ExtendBuiltin,]
);
impl_iterable!(ListIter);

impl List {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn append(&mut self, item: TreewalkValue) {
        self.items.push(item)
    }

    pub fn join(&self, delim: &str) -> DomainResult<String> {
        Ok(self
            .items
            .iter()
            .map(|v| v.as_str())
            .collect::<DomainResult<Vec<_>>>()?
            .join(delim))
    }

    pub fn extend(&mut self, items: Box<dyn CloneableIterable>) {
        self.items.extend(items)
    }

    /// Use this when you need a `pop_front` method.
    pub fn as_queue(&self) -> VecDeque<TreewalkValue> {
        self.items.clone().into()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn slice(&self, interpreter: &TreewalkInterpreter, slice: &Slice) -> Self {
        let len = self.items.len() as i64;
        let receiver = Container::new(self.clone());

        let sliced_items = Slice::slice(slice, len, |i| {
            receiver
                .getitem(interpreter, TreewalkValue::Int(i))
                .unwrap()
        });

        List::new(sliced_items)
    }
}

impl IndexRead for Container<List> {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        key: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(match key {
            TreewalkValue::Int(i) => self.borrow().items.get(i as usize).cloned(),
            TreewalkValue::Slice(s) => Some(TreewalkValue::List(Container::new(
                self.borrow().slice(interpreter, &s),
            ))),
            _ => None,
        })
    }
}

impl IndexWrite for Container<List> {
    fn setitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        let i = index.as_int().raise(interpreter)?;
        self.borrow_mut().items[i as usize] = value;
        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<()> {
        let i = index.as_int().raise(interpreter)?;
        self.borrow_mut().items.remove(i as usize);
        Ok(())
    }
}

impl Add for List {
    type Output = List;

    fn add(self, other: List) -> List {
        List {
            items: [self.items, other.items].concat(),
        }
    }
}

impl TryEvalFrom for List {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self> {
        let iter = value.as_iterator().raise(interpreter)?;
        Ok(List::new(iter.collect()))
    }
}

impl Display for Container<List> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "[{}]", format_comma_separated(self.clone()))
    }
}

impl IntoIterator for Container<List> {
    type Item = TreewalkValue;
    type IntoIter = ListIter;

    fn into_iter(self) -> Self::IntoIter {
        ListIter::new(self)
    }
}

#[derive(Clone)]
pub struct ListIter {
    list_ref: Container<List>,
    current_index: Container<usize>,
}

impl ListIter {
    pub fn new(list_ref: Container<List>) -> Self {
        Self {
            list_ref,
            current_index: Container::new(0),
        }
    }
}

impl Iterator for ListIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if *self.current_index.borrow() == self.list_ref.borrow().len() {
            None
        } else {
            *self.current_index.borrow_mut() += 1;
            self.list_ref
                .borrow()
                .items
                .get(*self.current_index.borrow() - 1)
                .cloned()
        }
    }
}

impl ExactSizeIterator for ListIter {
    fn len(&self) -> usize {
        self.list_ref.borrow().len() - *self.current_index.borrow()
    }
}

#[derive(Clone)]
struct NewBuiltin;
#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct AppendBuiltin;
#[derive(Clone)]
struct ExtendBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len)).raise(interpreter)?;

        let list = match args.len() {
            1 => List::default(),
            2 => List::try_eval_from(args.get_arg(1), interpreter)?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::List(Container::new(list)))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let left_list = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        let right_list = args.get_arg(0).as_list().raise(interpreter)?;
        let l = left_list.borrow().clone();
        let r = right_list.borrow().clone();

        Ok(TreewalkValue::List(Container::new(l + r)))
    }

    fn name(&self) -> String {
        Dunder::Add.into()
    }
}

impl Callable for AppendBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let list = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        list.borrow_mut().append(args.get_arg(0).clone());

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "append".into()
    }
}

impl Callable for ExtendBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1).raise(interpreter)?;

        let list = args
            .get_self()
            .raise(interpreter)?
            .as_list()
            .raise(interpreter)?;
        list.borrow_mut().extend(
            args.get_arg(0)
                .as_iterable()
                .raise(interpreter)?
                .into_iter(),
        );

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "extend".into()
    }
}
