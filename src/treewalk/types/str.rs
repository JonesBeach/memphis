use std::{
    fmt::{Display, Error, Formatter},
    ops::Deref,
};

use crate::{
    domain::{Dunder, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead},
        types::Slice,
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

#[derive(Clone, PartialEq)]
pub struct Str(String);

impl_typed!(Str, Type::Str);
impl_method_provider!(
    Str,
    [
        AddBuiltin,
        MulBuiltin,
        ContainsBuiltin,
        JoinBuiltin,
        MaketransBuiltin
    ]
);
impl_iterable!(StrIter);

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
        _interpreter: &TreewalkInterpreter,
        key: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>> {
        Ok(match key {
            TreewalkValue::Int(i) => self
                .0
                .chars()
                .nth(i as usize)
                .map(|c| c.to_string())
                .map(Str::new)
                .map(TreewalkValue::Str),
            TreewalkValue::Slice(s) => Some(TreewalkValue::Str(self.slice(&s))),
            _ => None,
        })
    }
}

#[derive(Clone)]
struct AddBuiltin;
#[derive(Clone)]
struct MulBuiltin;
#[derive(Clone)]
struct ContainsBuiltin;
#[derive(Clone)]
struct JoinBuiltin;
#[derive(Clone)]
struct MaketransBuiltin;

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        // implements a + b
        let a = args.expect_self(interpreter)?.expect_string(interpreter)?;
        let b = args.get_arg(0).expect_string(interpreter)?;

        Ok(TreewalkValue::Str(Str::new(a + &b)))
    }

    fn name(&self) -> String {
        Dunder::Add.into()
    }
}

impl Callable for MulBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args.expect_self(interpreter)?.expect_string(interpreter)?;
        let n = args.get_arg(0).expect_integer(interpreter)?;

        Ok(TreewalkValue::Str(Str::new(a.repeat(n as usize))))
    }

    fn name(&self) -> String {
        Dunder::Mul.into()
    }
}

impl Callable for ContainsBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        // implements b in a
        let b = args.expect_self(interpreter)?.expect_string(interpreter)?;
        let a = args.get_arg(0).expect_string(interpreter)?;

        Ok(TreewalkValue::Bool(a.contains(&b)))
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}

impl Callable for JoinBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        "join".into()
    }
}

impl Callable for MaketransBuiltin {
    fn call(
        &self,
        _interpreter: &TreewalkInterpreter,
        _args: Args,
    ) -> TreewalkResult<TreewalkValue> {
        unimplemented!()
    }

    fn name(&self) -> String {
        "maketrans".into()
    }
}

impl IntoIterator for Str {
    type Item = TreewalkValue;
    type IntoIter = StrIter;

    fn into_iter(self) -> Self::IntoIter {
        StrIter::new(self)
    }
}

#[derive(Clone)]
pub struct StrIter {
    string: String,
    position: usize,
}

impl StrIter {
    pub fn new(string: Str) -> Self {
        Self {
            string: string.0.clone(),
            position: 0,
        }
    }
}

impl Iterator for StrIter {
    type Item = TreewalkValue;

    fn next(&mut self) -> Option<Self::Item> {
        let result = self.string[self.position..].chars().next()?;
        self.position += result.len_utf8();
        Some(TreewalkValue::Str(Str::new(result.to_string())))
    }
}
