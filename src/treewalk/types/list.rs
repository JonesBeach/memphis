use std::{
    collections::VecDeque,
    fmt::{Display, Error, Formatter},
    ops::Add,
};

use crate::{
    core::Container,
    domain::{Dunder, Type},
    treewalk::{
        protocols::{Callable, IndexRead, IndexWrite, MethodProvider, Typed},
        types::{iterators::GeneratorIterator, Range, Set, Slice, Tuple},
        utils::{check_args, Arguments},
        Interpreter, TreewalkResult, TreewalkValue, TreewalkValueIterator,
    },
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct List {
    items: Vec<TreewalkValue>,
}

impl Typed for List {
    fn get_type() -> Type {
        Type::List
    }
}

impl MethodProvider for List {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![
            Box::new(NewBuiltin),
            Box::new(AppendBuiltin),
            Box::new(ExtendBuiltin),
        ]
    }
}

impl List {
    pub fn new(items: Vec<TreewalkValue>) -> Self {
        Self { items }
    }

    pub fn append(&mut self, item: TreewalkValue) {
        self.items.push(item)
    }

    pub fn extend(&mut self, items: TreewalkValueIterator) {
        self.items.extend(items)
    }

    /// Use this when you need a `pop_front` method.
    pub fn as_queue(&self) -> VecDeque<TreewalkValue> {
        self.items.clone().into()
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn slice(&self, interpreter: &Interpreter, slice: &Slice) -> Self {
        let len = self.items.len() as i64;
        let receiver = Container::new(self.clone());

        let sliced_items = Slice::slice(slice, len, |i| {
            receiver
                .getitem(interpreter, TreewalkValue::Integer(i))
                .unwrap()
        });

        List::new(sliced_items)
    }
}

impl IndexRead for Container<List> {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        key: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(match key {
            TreewalkValue::Integer(i) => self.borrow().items.get(i as usize).cloned(),
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
        interpreter: &Interpreter,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()> {
        let i = index.expect_integer(interpreter)?;
        self.borrow_mut().items[i as usize] = value;
        Ok(())
    }

    fn delitem(&mut self, interpreter: &Interpreter, index: TreewalkValue) -> TreewalkResult<()> {
        let i = index.expect_integer(interpreter)?;
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

impl TryFrom<TreewalkValue> for Container<List> {
    type Error = ();

    fn try_from(value: TreewalkValue) -> Result<Self, Self::Error> {
        match value {
            TreewalkValue::List(list) => Ok(list),
            TreewalkValue::Set(set) => Ok(set.into()),
            TreewalkValue::Tuple(tuple) => Ok(tuple.into()),
            TreewalkValue::Range(range) => Ok(range.into()),
            TreewalkValue::Generator(g) => Ok(g.into()),
            _ => Err(()),
        }
    }
}

impl From<Range> for Container<List> {
    fn from(range: Range) -> Container<List> {
        let items = (range.start..range.stop)
            .map(TreewalkValue::Integer)
            .collect();
        Container::new(List::new(items))
    }
}

impl From<Container<Set>> for Container<List> {
    fn from(set: Container<Set>) -> Container<List> {
        // Calling `into_iter()` directly off the `Set` results in a stack overflow.
        //let mut items: Vec<TreewalkValue> = set.into_iter().collect();
        let mut items: Vec<TreewalkValue> = set.borrow().items.clone().into_iter().collect();

        items.sort_by_key(|x| {
            match x {
                TreewalkValue::Integer(i) => *i,
                // TODO how should we sort strings here?
                //TreewalkValue::String(s) => s.0,
                _ => 0,
            }
        });

        Container::new(List::new(items))
    }
}

impl From<Tuple> for Container<List> {
    fn from(tuple: Tuple) -> Container<List> {
        Container::new(List::new(tuple.raw()))
    }
}

impl From<Container<GeneratorIterator>> for Container<List> {
    fn from(g: Container<GeneratorIterator>) -> Container<List> {
        let items = g.borrow().clone().collect::<Vec<TreewalkValue>>();
        Container::new(List::new(items))
    }
}

impl Display for Container<List> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        let items = ListIterator::new(self.clone())
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", items)
    }
}

impl IntoIterator for Container<List> {
    type Item = TreewalkValue;
    type IntoIter = ListIterator;

    fn into_iter(self) -> Self::IntoIter {
        ListIterator::new(self)
    }
}

#[derive(Clone)]
pub struct ListIterator {
    list_ref: Container<List>,
    current_index: usize,
}

impl ListIterator {
    pub fn new(list_ref: Container<List>) -> Self {
        Self {
            list_ref,
            current_index: 0,
        }
    }
}

impl Iterator for ListIterator {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.list_ref.borrow().len() {
            None
        } else {
            self.current_index += 1;
            self.list_ref
                .borrow()
                .items
                .get(self.current_index - 1)
                .cloned()
        }
    }
}

impl ExactSizeIterator for ListIterator {
    fn len(&self) -> usize {
        self.list_ref.borrow().len() - self.current_index
    }
}

struct NewBuiltin;
struct AppendBuiltin;
struct ExtendBuiltin;

impl Callable for NewBuiltin {
    fn call(&self, interpreter: &Interpreter, args: Arguments) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let list = match args.len() {
            1 => Container::new(List::default()),
            2 => args
                .get_arg(1)
                .try_into()
                .map_err(|_| interpreter.type_error("Expected a list"))?,
            _ => unreachable!(),
        };

        Ok(TreewalkValue::List(list))
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AppendBuiltin {
    fn call(&self, interpreter: &Interpreter, args: Arguments) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let list = args.expect_self(interpreter)?.expect_list(interpreter)?;
        list.borrow_mut().append(args.get_arg(0).clone());

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "append".into()
    }
}

impl Callable for ExtendBuiltin {
    fn call(&self, interpreter: &Interpreter, args: Arguments) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let list = args.expect_self(interpreter)?.expect_list(interpreter)?;
        list.borrow_mut().extend(args.get_arg(0).into_iter());

        Ok(TreewalkValue::None)
    }

    fn name(&self) -> String {
        "extend".into()
    }
}
