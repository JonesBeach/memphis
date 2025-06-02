use std::{
    any::Any,
    fmt::{Debug, Error, Formatter},
};

use crate::{
    core::Container,
    treewalk::{
        types::{function::FunctionType, Class},
        utils::Args,
        TreewalkInterpreter, TreewalkResult, TreewalkValue,
    },
};

pub trait TryEvalFrom: Sized {
    fn try_eval_from(
        value: TreewalkValue,
        interpreter: &TreewalkInterpreter,
    ) -> TreewalkResult<Self>;
}

pub trait Callable {
    fn call(&self, interpreter: &TreewalkInterpreter, args: Args) -> TreewalkResult<TreewalkValue>;

    fn name(&self) -> String;

    fn function_type(&self) -> FunctionType {
        FunctionType::Regular
    }

    /// This stub exists so that we can downcast to `Container<Function>`.
    fn as_any(&self) -> &dyn Any {
        unreachable!()
    }

    /// A callable will not have a receiver by default, but certain types (`Method`) can have them.
    fn receiver(&self) -> Option<TreewalkValue> {
        None
    }
}

impl Debug for dyn Callable {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<{}>", self.name())
    }
}

// This probably isn't right, this is just so we can store these in a hash inside a class.
impl PartialEq for dyn Callable {
    fn eq(&self, other: &Self) -> bool {
        self.name() == other.name()
    }
}

/// This represents the Python `__iter__` protocol. It differs from the Rust `Iterator` because it
/// is falliable (i.e.e certain calls to `next()` may throw a runtime error).
///
/// For ergonomics, we implement both `Iterable` and `Iterator` for our builitin types.
pub trait Iterable {
    fn next(&mut self) -> TreewalkResult<Option<TreewalkValue>>;
}

pub trait MemberRead {
    /// A pointer to the [`Interpreter`] is sometimes not needed, but is required to evalute method
    /// calls for descriptors.
    fn get_member(
        &self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<Option<TreewalkValue>>;

    /// Returns a sorted list of the symbols available.
    fn dir(&self) -> Vec<String> {
        unimplemented!()
    }
}

pub trait MemberWrite {
    fn set_member(
        &mut self,
        interpreter: &TreewalkInterpreter,
        name: &str,
        value: TreewalkValue,
    ) -> TreewalkResult<()>;
    fn delete_member(
        &mut self,
        interpreter: &TreewalkInterpreter,
        name: &str,
    ) -> TreewalkResult<()>;
}

pub trait NonDataDescriptor {
    fn name(&self) -> String;
    fn get_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: Option<TreewalkValue>,
        owner: Container<Class>,
    ) -> TreewalkResult<TreewalkValue>;
}

/// All data descriptors in Python, which provide write access, can be assumed to also be non-data
/// descriptors, which provide read access (and, for our implementation, their name).
pub trait DataDescriptor: NonDataDescriptor {
    fn set_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()>;
    fn delete_attr(
        &self,
        interpreter: &TreewalkInterpreter,
        instance: TreewalkValue,
    ) -> TreewalkResult<()>;
}

pub trait IndexRead {
    fn getitem(
        &self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<Option<TreewalkValue>>;
}

pub trait IndexWrite {
    fn setitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
        value: TreewalkValue,
    ) -> TreewalkResult<()>;
    fn delitem(
        &mut self,
        interpreter: &TreewalkInterpreter,
        index: TreewalkValue,
    ) -> TreewalkResult<()>;
}

// pub trait IndexAccessor: IndexRead + IndexWrite {}
