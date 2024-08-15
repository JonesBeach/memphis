use std::{
    fmt::{Display, Error, Formatter},
    ops::Deref,
};

use crate::{treewalk::Interpreter, types::errors::InterpreterError};

use super::{
    domain::{
        traits::{Callable, IndexRead, MethodProvider, Typed},
        Type,
    },
    utils::ResolvedArguments,
    ExprResult, Slice,
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
        key: ExprResult,
    ) -> Result<Option<ExprResult>, InterpreterError> {
        Ok(match key {
            ExprResult::Integer(i) => self
                .0
                .chars()
                .nth(*i.borrow() as usize)
                .map(|c| c.to_string())
                .map(Str::new)
                .map(ExprResult::String),
            ExprResult::Slice(s) => Some(ExprResult::String(self.slice(&s))),
            _ => None,
        })
    }
}

struct JoinBuiltin;

impl Callable for JoinBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
        unimplemented!()
    }

    fn name(&self) -> String {
        "join".into()
    }
}

struct MaketransBuiltin;

impl Callable for MaketransBuiltin {
    fn call(
        &self,
        _interpreter: &Interpreter,
        _args: ResolvedArguments,
    ) -> Result<ExprResult, InterpreterError> {
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
    type Item = ExprResult;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.string[self.position..].chars().next()?;
        self.position += result.len_utf8();
        Some(ExprResult::String(Str::new(result.to_string())))
    }
}
