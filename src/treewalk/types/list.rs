use std::{
    collections::VecDeque,
    fmt::{Display, Error, Formatter},
    ops::Add,
};

use crate::{
    core::{Container, Storable},
    treewalk::Interpreter,
    types::errors::InterpreterError,
};

use super::{
    domain::{
        builtins::utils::validate_args,
        traits::{Callable, IndexRead, IndexWrite, MethodProvider, Typed},
        Type,
    },
    generator::GeneratorIterator,
    result::ExprResultIterator,
    utils::{Dunder, ResolvedArguments},
    ExprResult, Range, Set, Slice, Tuple,
};

#[derive(Default, Debug, PartialEq, Clone)]
pub struct List {
    items: Vec<ExprResult>,
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
    pub fn new(items: Vec<ExprResult>) -> Self {
        Self { items }
    }

    pub fn append(&mut self, item: ExprResult) {
        self.items.push(item)
    }

    pub fn extend(&mut self, items: ExprResultIterator) {
        self.items.extend(items)
    }

    /// Use this when you need a `pop_front` method.
    pub fn as_queue(&self) -> VecDeque<ExprResult> {
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
                .getitem(interpreter, ExprResult::Integer(i.store()))
                .unwrap()
        });

        List::new(sliced_items)
    }
}

impl IndexRead for Container<List> {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        key: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        Ok(match key {
            ExprResult::Integer(i) => self.borrow().items.get(*i.borrow() as usize).cloned(),
            ExprResult::Slice(s) => Some(ExprResult::List(Container::new(
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
        index: ExprResult,
        value: ExprResult,
    ) -> Result<(), InterpreterError> {
        let i = index
            .as_integer_val()
            .ok_or(InterpreterError::ExpectedInteger(
                interpreter.state.call_stack(),
            ))?;
        self.borrow_mut().items[i as usize] = value;
        Ok(())
    }

    fn delitem(
        &mut self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> Result<(), InterpreterError> {
        let i = index
            .as_integer_val()
            .ok_or(InterpreterError::ExpectedInteger(
                interpreter.state.call_stack(),
            ))?;
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

impl From<Container<Range>> for Container<List> {
    fn from(range: Container<Range>) -> Container<List> {
        let start = range.borrow().start;
        let stop = range.borrow().stop;
        let items = (start..stop)
            .map(|x| ExprResult::Integer(Container::new(x as i64)))
            .collect();
        Container::new(List::new(items))
    }
}

impl From<Container<Set>> for Container<List> {
    fn from(set: Container<Set>) -> Container<List> {
        // Calling `into_iter()` directly off the `Set` results in a stack overflow.
        //let mut items: Vec<ExprResult> = set.into_iter().collect();
        let mut items: Vec<ExprResult> = set.borrow().items.clone().into_iter().collect();

        items.sort_by_key(|x| {
            match x {
                ExprResult::Integer(i) => *i.borrow(),
                // TODO how should we sort strings here?
                //ExprResult::String(s) => s.0,
                _ => 0,
            }
        });

        Container::new(List::new(items))
    }
}

impl From<Container<Tuple>> for Container<List> {
    fn from(tuple: Container<Tuple>) -> Container<List> {
        Container::new(List::new(tuple.borrow().raw()))
    }
}

impl From<Container<GeneratorIterator>> for Container<List> {
    fn from(g: Container<GeneratorIterator>) -> Container<List> {
        let items = g.borrow().clone().collect::<Vec<ExprResult>>();
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
    type Item = ExprResult;
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
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_index == self.list_ref.borrow().items.len() {
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

struct NewBuiltin;
struct AppendBuiltin;
struct ExtendBuiltin;

impl Callable for NewBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        if args.len() == 2 {
            let output = args
                .get_arg(1)
                .as_list()
                .ok_or(InterpreterError::ExpectedList(
                    interpreter.state.call_stack(),
                ))?;
            Ok(ExprResult::List(output))
        } else {
            validate_args(&args, 1, interpreter.state.call_stack())?;
            Ok(ExprResult::List(Container::new(List::default())))
        }
    }

    fn name(&self) -> String {
        Dunder::New.into()
    }
}

impl Callable for AppendBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        validate_args(&args, 1, interpreter.state.call_stack())?;

        let list = args
            .get_self()
            .ok_or(InterpreterError::ExpectedList(
                interpreter.state.call_stack(),
            ))?
            .as_list()
            .ok_or(InterpreterError::ExpectedList(
                interpreter.state.call_stack(),
            ))?;

        list.borrow_mut().append(args.get_arg(0).clone());

        Ok(ExprResult::None)
    }

    fn name(&self) -> String {
        "append".into()
    }
}

impl Callable for ExtendBuiltin {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        validate_args(&args, 1, interpreter.state.call_stack())?;

        let list = args
            .get_self()
            .ok_or(InterpreterError::ExpectedList(
                interpreter.state.call_stack(),
            ))?
            .as_list()
            .ok_or(InterpreterError::ExpectedList(
                interpreter.state.call_stack(),
            ))?;

        list.borrow_mut().extend(args.get_arg(0).into_iter());

        Ok(ExprResult::None)
    }

    fn name(&self) -> String {
        "extend".into()
    }
}
