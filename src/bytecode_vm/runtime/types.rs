use std::{
    collections::HashMap,
    fmt::{Display, Error, Formatter},
};

use crate::{
    bytecode_vm::{
        compiler::CodeObject,
        indices::{ConstantIndex, ObjectTableIndex},
        VirtualMachine, VmResult,
    },
    domain::FunctionType,
};

use super::frame::Frame;

pub type Namespace = HashMap<String, Reference>;

/// Primitive values live directly on the stack.
/// [`Reference::ObjectRef`] items reference an object in the object table.
/// [`Reference::ConstantRef`] items reference an immutable object in the constant pool.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Reference {
    Int(i64),
    Float(f64),
    ObjectRef(ObjectTableIndex),
    ConstantRef(ConstantIndex),
}

impl Display for Reference {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct List {
    // TODO this is currently public because we need it in some tests to call deference on the
    // elements. We'll eventually make this a slice accessor or an iterator or something.
    pub items: Vec<Reference>,
}

impl List {
    pub fn new(items: Vec<Reference>) -> Self {
        Self { items }
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(self) -> ListIter {
        ListIter {
            inner: self.items.into_iter(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ListIter {
    inner: std::vec::IntoIter<Reference>,
}

impl Iterator for ListIter {
    type Item = Reference;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Range {
    pub start: i64,
    pub stop: i64,
    pub step: i64,
}

impl Range {
    const DEFAULT_START: i64 = 0;
    const DEFAULT_STEP: i64 = 1;

    pub fn new(start: i64, stop: i64, step: i64) -> Self {
        Self { start, stop, step }
    }

    pub fn with_stop(stop: i64) -> Self {
        Self::new(Self::DEFAULT_START, stop, Self::DEFAULT_STEP)
    }

    pub fn with_start_stop(start: i64, stop: i64) -> Self {
        Self::new(start, stop, Self::DEFAULT_STEP)
    }

    // Do not take ownership so we can reuse this Range if we like.
    pub fn iter(&self) -> RangeIter {
        RangeIter {
            current: self.start,
            stop: self.stop,
            step: self.step,
        }
    }
}

#[derive(Clone, Debug)]
pub struct RangeIter {
    current: i64,
    stop: i64,
    step: i64,
}

impl Iterator for RangeIter {
    type Item = i64;

    fn next(&mut self) -> Option<Self::Item> {
        if (self.step > 0 && self.current >= self.stop)
            || (self.step < 0 && self.current <= self.stop)
        {
            None
        } else {
            let result = self.current;
            self.current += self.step;
            Some(result)
        }
    }
}

pub type BuiltinFunc = fn(&mut VirtualMachine, Vec<Reference>) -> VmResult<Reference>;

#[derive(Clone, Debug)]
pub struct BuiltinFunction {
    name: String,
    func: BuiltinFunc,
}

impl BuiltinFunction {
    pub fn new(name: &str, func: BuiltinFunc) -> Self {
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
        write!(f, "<builtin function {}>", self.name())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    name: String,
    namespace: Namespace,
}

impl Class {
    pub fn new(name: String, namespace: Namespace) -> Self {
        Self { name, namespace }
    }

    pub fn read<S>(&self, name: S) -> Option<Reference>
    where
        S: AsRef<str>,
    {
        self.namespace.get(name.as_ref()).cloned()
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Object {
    class: Reference,
    namespace: Namespace,
}

impl Object {
    pub fn new(class: Reference) -> Self {
        Self {
            class,
            namespace: HashMap::new(),
        }
    }

    /// `deref` is needed to get the actual class in case the symbol is not found on the object.
    pub fn read(&self, name: &str, vm: &VirtualMachine) -> VmResult<Option<Reference>> {
        if let Some(result) = self.namespace.get(name) {
            return Ok(Some(*result));
        }

        let class = vm.deref(self.class)?;
        Ok(class.expect_class(vm)?.namespace.get(name).cloned())
    }

    pub fn write(&mut self, name: &str, value: Reference) {
        self.namespace.insert(name.to_string(), value);
    }
}

#[derive(Clone, Debug)]
pub struct Generator {
    pub frame: Frame,
    #[allow(dead_code)]
    done: bool,
}

impl Generator {
    pub fn new(frame: Frame) -> Self {
        Self { frame, done: false }
    }
}

/// This encapsulates a [`CodeObject`] along with the execution environment in which the function
/// was defined (closure/free variables). This is what gets created when you define a function in
/// Python. This is not bound to any particular instance of a class when defined inside a class.
#[derive(Clone, PartialEq, Debug)]
pub struct FunctionObject {
    pub code_object: CodeObject,
    pub freevars: Vec<Reference>,
}

impl FunctionObject {
    pub fn new(code_object: CodeObject) -> Self {
        Self {
            code_object,
            freevars: vec![],
        }
    }

    pub fn new_with_free(code_object: CodeObject, freevars: Vec<Reference>) -> Self {
        Self {
            code_object,
            freevars,
        }
    }

    pub fn function_type(&self) -> &FunctionType {
        &self.code_object.function_type
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

#[derive(Debug, Clone, Default)]
pub struct Module {
    pub name: String,

    /// The runtime mapping of global variables to their values.
    pub global_store: HashMap<String, Reference>,
}

impl Module {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            global_store: HashMap::new(),
        }
    }

    pub fn read(&self, name: &str) -> Option<Reference> {
        self.global_store.get(name).cloned()
    }
}
