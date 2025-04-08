use std::{
    fmt::{Display, Error, Formatter},
    ops::Deref,
};

use crate::{
    domain::Type,
    treewalk::{
        protocols::{Callable, IndexRead, MethodProvider, Typed},
        types::Slice,
        utils::Arguments,
        Interpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone, PartialEq)]
pub struct Str(String);

impl Typed for Str {
    fn get_type() -> Type {
        Type::Str
    }
}

impl MethodProvider for Str {
    fn get_methods() -> Vec<Box<dyn Callable>> {
        vec![Box::new(JoinBuiltin), Box::new(MaketransBuiltin)]
    }
}

impl Str {
    pub fn new(str: String) -> Self {
        Self(str)
    }

    pub fn slice(&self, slice: &Slice) -> Self {
        let len = self.0.chars().count() as i64;

        let sliced_string = Slice::slice(slice, len, |i| {
            self.0.chars().nth(i as usize).map(|c| c.to_string())
        })
        .join("");

        Str::new(sliced_string)
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.0)
    }
}

impl Deref for Str {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl IndexRead for Str {
    fn getitem(
        &self,
        _interpreter: &Interpreter,
        key: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(match key {
            TreewalkValue::Integer(i) => self
                .0
                .chars()
                .nth(i as usize)
                .map(|c| c.to_string())
                .map(Str::new)
                .map(TreewalkValue::String),
            TreewalkValue::Slice(s) => Some(TreewalkValue::String(self.slice(&s))),
            _ => None,
        })
    }
}

struct JoinBuiltin;

impl Callable for JoinBuiltin {
    fn call(&self, _interpreter: &Interpreter, _args: Arguments) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        "join".into()
    }
}

struct MaketransBuiltin;

impl Callable for MaketransBuiltin {
    fn call(&self, _interpreter: &Interpreter, _args: Arguments) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        "maketrans".into()
    }
}

#[derive(Clone)]
pub struct StringIterator {
    string: String,
    position: usize,
}

impl StringIterator {
    pub fn new(string: Str) -> Self {
        Self {
            string: string.0.clone(),
            position: 0,
        }
    }
}

impl Iterator for StringIterator {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.string[self.position..].chars().next()?;
        self.position += result.len_utf8();
        Some(TreewalkValue::String(Str::new(result.to_string())))
    }
}
