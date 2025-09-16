use std::fmt::{Display, Formatter};

use crate::bytecode_vm::{runtime::Reference, VirtualMachine, VmResult};

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
