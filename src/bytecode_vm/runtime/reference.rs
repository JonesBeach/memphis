use std::{
    collections::HashMap,
    fmt::{Display, Error, Formatter},
};

use crate::bytecode_vm::{indices::ObjectTableIndex, VmResult};

use super::{heap::Heap, VirtualMachine};

pub type Namespace = HashMap<String, Reference>;

pub type BuiltinFn = fn(&mut VirtualMachine, Vec<Reference>) -> VmResult<Reference>;

#[derive(Clone, Debug)]
pub struct BuiltinFunction {
    name: String,
    func: BuiltinFn,
}

impl BuiltinFunction {
    pub fn new(name: &str, func: BuiltinFn) -> Self {
        Self {
            name: name.to_string(),
            func,
        }
    }

    pub fn call(&self, vm: &mut VirtualMachine, args: Vec<Reference>) -> VmResult<Reference> {
        (self.func)(vm, args)
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Display for BuiltinFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<builtin function '{}'>", self.name())
    }
}

/// Primitive values live directly on the stack.
/// [`Reference::ObjectRef`] items reference an object in the object table.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reference {
    Int(i64),
    Float(f64),
    ObjectRef(ObjectTableIndex),
}

impl Reference {
    pub fn display_annotated(&self, heap: &Heap) -> String {
        match self {
            Self::ObjectRef(index) => format!(
                "ObjectRef({}) => {}",
                index,
                heap.get(*self).expect("Heap lookup failed")
            ),
            _ => format!("{}", self),
        }
    }
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{self:?}")
    }
}
