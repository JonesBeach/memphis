use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Display, Error, Formatter},
};

use crate::bytecode_vm::{
    compiler::types::CodeObject,
    indices::{ConstantIndex, ObjectTableIndex},
    types::Value,
};

pub type Namespace = HashMap<String, Reference>;

/// Primitive values live directly on the stack.
/// [`Reference::ObjectRef`] items reference an object in the object table.
/// [`Reference::ConstantRef`] items reference an immutable object in the constant pool.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reference {
    Int(i64),
    Bool(bool),
    ObjectRef(ObjectTableIndex),
    ConstantRef(ConstantIndex),
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

impl From<Value> for Reference {
    fn from(value: Value) -> Self {
        match value {
            Value::Integer(i) => Reference::Int(i),
            Value::Boolean(i) => Reference::Bool(i),
            _ => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: String,
    namespace: Namespace,
}

impl Class {
    pub fn new(name: String, namespace: Namespace) -> Self {
        Self { name, namespace }
    }

    pub fn read(&self, name: &str) -> Option<Reference> {
        self.namespace.get(name).cloned()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    class: Reference,
    namespace: Namespace,
}

impl<'a> Object {
    pub fn new(class: Reference) -> Self {
        Self {
            class,
            namespace: HashMap::new(),
        }
    }

    #[cfg(test)]
    pub fn class_ref(&self) -> Reference {
        self.class
    }

    pub fn read<T>(&self, name: &str, deref: T) -> Option<Reference>
    where
        T: FnOnce(Reference) -> Cow<'a, Value>,
    {
        if let Some(result) = self.namespace.get(name) {
            return Some(*result);
        }

        let class = deref(self.class);
        class.as_class().namespace.get(name).cloned()
    }

    pub fn write(&mut self, name: &str, value: Reference) {
        self.namespace.insert(name.to_string(), value);
    }
}

/// This encapsulates a [`CodeObject`] along with the execution environment in which the function
/// was defined (global variables, closure/free variables). This is what gets created when you
/// define a function in Python. This is not bound to any particular instance of a class when
/// defined at the class level.
#[derive(Clone, PartialEq, Debug)]
pub struct FunctionObject {
    pub code_object: CodeObject,
}

impl FunctionObject {
    pub fn new(code_object: CodeObject) -> Self {
        Self { code_object }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Method {
    pub receiver: Reference,
    pub function: FunctionObject,
}

impl Method {
    pub fn new(receiver: Reference, function: FunctionObject) -> Self {
        Self { receiver, function }
    }
}
