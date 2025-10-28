use std::{
    fmt::{Display, Error, Formatter},
    ops::Deref,
    str,
};

use crate::{
    core::Container,
    domain::{Dunder, ExecutionErrorKind, Type},
    treewalk::{
        macros::*,
        protocols::{Callable, IndexRead},
        result::Raise,
        types::{List, Slice},
        utils::{check_args, Args},
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

use super::bytes::Encoding;

#[derive(Debug, Clone, PartialEq)]
pub struct Str(String);

impl_typed!(Str, Type::Str);
impl_method_provider!(
    Str,
    [
        AddBuiltin,
        MulBuiltin,
        ContainsBuiltin,
        JoinBuiltin,
        SplitBuiltin,
        LowerBuiltin,
        EncodeBuiltin,
    ]
);
impl_iterable!(StrIter);

impl Str {
    pub fn new(str: &str) -> Self {
        Self(str.to_string())
    }

    pub fn decode(bytes: &[u8], encoding: Encoding) -> Result<Self, ExecutionErrorKind> {
        let str = match encoding {
            Encoding::Utf8 => str::from_utf8(bytes).map_err(|_| {
                ExecutionErrorKind::ValueError(format!(
                    "failed to decode with encoding '{encoding}'"
                ))
            })?,
        };

        Ok(Self::new(str))
    }

    pub fn encode(&self, encoding: Encoding) -> Vec<u8> {
        if encoding != Encoding::Utf8 {
            unimplemented!("Rust only supports utf-8 in std");
        }

        self.0.as_bytes().to_vec()
    }

    pub fn slice(&self, slice: &Slice) -> Self {
        let len = self.0.chars().count() as i64;

        let sliced_string = Slice::slice(slice, len, |i| {
            self.0.chars().nth(i as usize).map(|c| c.to_string())
        })
        .join("");

        Str::from(sliced_string)
    }
}

impl From<String> for Str {
    fn from(s: String) -> Self {
        Str(s)
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
                .map(Str::from)
                .map(TreewalkValue::Str),
            TreewalkValue::Slice(s) => Some(TreewalkValue::Str(self.slice(&s))),
            _ => None,
        })
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
        Some(TreewalkValue::Str(Str::from(result.to_string())))
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
struct SplitBuiltin;
#[derive(Clone)]
struct LowerBuiltin;
#[derive(Clone)]
struct EncodeBuiltin;

impl Callable for AddBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        // implements a + b
        let a = args.expect_self(interpreter)?.expect_string(interpreter)?;
        let b = args.get_arg(0).expect_string(interpreter)?;

        Ok(TreewalkValue::Str(Str::from(a + &b)))
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

        Ok(TreewalkValue::Str(Str::from(a.repeat(n as usize))))
    }

    fn name(&self) -> String {
        Dunder::Mul.into()
    }
}

impl Callable for ContainsBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let a = args.expect_self(interpreter)?.expect_string(interpreter)?;
        let b = args.get_arg(0).expect_string(interpreter)?;

        Ok(TreewalkValue::Bool(a.contains(&b)))
    }

    fn name(&self) -> String {
        Dunder::Contains.into()
    }
}

impl Callable for JoinBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 1, interpreter)?;

        let delim = args.expect_self(interpreter)?.expect_string(interpreter)?;
        let items = args.get_arg(0).expect_list(interpreter)?;
        let joined = items.borrow().join(&delim).raise(interpreter)?;

        Ok(TreewalkValue::Str(Str::from(joined)))
    }

    fn name(&self) -> String {
        "join".into()
    }
}

impl Callable for SplitBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [1, 2].contains(&len), interpreter)?;

        let text = args.expect_self(interpreter)?.expect_string(interpreter)?;
        let delim = args.get_arg(0).expect_string(interpreter)?;

        // We must use dynamic dispatch because split and splitn return different types.
        let iter: Box<dyn Iterator<Item = &str>> = match args.len() {
            1 => Box::new(text.split(&delim)),
            2 => {
                let max_split = args.get_arg(1).expect_integer(interpreter)?;
                // Python's value for maxsplit is the number of splits done, while Rust interprets
                // it as the number of items in the resulting list. Therefore, we add one.
                Box::new(text.splitn((max_split as usize) + 1, &delim))
            }
            _ => unreachable!(),
        };

        let parts: Vec<_> = iter.map(|i| TreewalkValue::Str(Str::new(i))).collect();

        Ok(TreewalkValue::List(Container::new(List::new(parts))))
    }

    fn name(&self) -> String {
        "split".into()
    }
}

impl Callable for LowerBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| len == 0, interpreter)?;
        let text = args.expect_self(interpreter)?.expect_string(interpreter)?;
        Ok(TreewalkValue::Str(Str::from(text.to_lowercase())))
    }

    fn name(&self) -> String {
        "lower".into()
    }
}

impl Callable for EncodeBuiltin {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue> {
        check_args(&args, |len| [0, 1].contains(&len), interpreter)?;
        let text = args.expect_self(interpreter)?.expect_string(interpreter)?;

        let encoding = match args.len() {
            0 => Encoding::Utf8,
            1 => {
                let encoding_str = args.get_arg(0).expect_string(interpreter)?;
                Encoding::try_from(encoding_str.as_str())
                    .map_err(|_| interpreter.unknown_encoding(encoding_str))?
            }
            _ => unreachable!(),
        };

        Ok(TreewalkValue::Bytes(Str::from(text).encode(encoding)))
    }

    fn name(&self) -> String {
        "encode".into()
    }
}
