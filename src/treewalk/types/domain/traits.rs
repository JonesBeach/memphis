use crate::treewalk::interpreter::TreewalkResult;
use std::{
    any::Any,
    fmt::{Debug, Error, Formatter},
};

use crate::{
    core::Container,
    treewalk::{
        types::{
            domain::Type, function::FunctionType, utils::ResolvedArguments, Class, ExprResult,
        },
        Interpreter,
    },
};

pub trait Callable {
    fn call(
        &self,
        interpreter: &Interpreter,
        args: ResolvedArguments,
    ) -> TreewalkResult<ExprResult>;

    fn name(&self) -> String;

    fn function_type(&self) -> FunctionType {
        FunctionType::Regular
    }

    /// This stub exists so that we can downcast to `Container<Function>`.
    fn as_any(&self) -> &dyn Any {
        unreachable!()
    }

    /// A callable will not have a receiver by default, but certain types (`Method`) can have them.
    fn receiver(&self) -> Option<ExprResult> {
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

pub trait MemberReader {
    /// A pointer to the [`Interpreter`] is sometimes not needed, but is required to evalute method
    /// calls for descriptors.
    fn get_member(
        &self,
        interpreter: &Interpreter,
        name: &str,
    ) -> TreewalkResult<Option<ExprResult>>;

    /// Returns a sorted list of the symbols available.
    fn dir(&self) -> Vec<String> {
        unimplemented!()
    }
}

pub trait MemberWriter {
    fn set_member(
        &mut self,
        interpreter: &Interpreter,
        name: &str,
        value: ExprResult,
    ) -> TreewalkResult<()>;
    fn delete_member(&mut self, interpreter: &Interpreter, name: &str) -> TreewalkResult<()>;
}

pub trait NonDataDescriptor {
    fn name(&self) -> String;
    fn get_attr(
        &self,
        interpreter: &Interpreter,
        instance: Option<ExprResult>,
        owner: Container<Class>,
    ) -> TreewalkResult<ExprResult>;
}

/// All data descriptors in Python, which provide write access, can be assumed to also be non-data
/// descriptors, which provide read access (and, for our implementation, their name).
pub trait DataDescriptor: NonDataDescriptor {
    fn set_attr(
        &self,
        interpreter: &Interpreter,
        instance: ExprResult,
        value: ExprResult,
    ) -> TreewalkResult<()>;
    fn delete_attr(&self, interpreter: &Interpreter, instance: ExprResult) -> TreewalkResult<()>;
}

pub trait IndexRead {
    fn getitem(
        &self,
        interpreter: &Interpreter,
        index: ExprResult,
    ) -> TreewalkResult<Option<ExprResult>>;
}

pub trait IndexWrite {
    fn setitem(
        &mut self,
        interpreter: &Interpreter,
        index: ExprResult,
        value: ExprResult,
    ) -> TreewalkResult<()>;
    fn delitem(&mut self, interpreter: &Interpreter, index: ExprResult) -> TreewalkResult<()>;
}

// pub trait IndexAccessor: IndexRead + IndexWrite {}

pub trait Typed {
    fn get_type() -> Type;
}

pub trait MethodProvider: Typed {
    fn get_methods() -> Vec<Box<dyn Callable>>;
}

pub trait DescriptorProvider: Typed {
    fn get_descriptors() -> Vec<Box<dyn NonDataDescriptor>>;
}

pub trait DataDescriptorProvider: Typed {
    fn get_data_descriptors() -> Vec<Box<dyn DataDescriptor>>;
}
