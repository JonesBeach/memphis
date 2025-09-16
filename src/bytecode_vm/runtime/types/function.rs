use std::fmt::{Display, Error, Formatter};

use crate::{
    bytecode_vm::{compiler::CodeObject, runtime::Reference},
    domain::FunctionType,
};

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

    pub fn name(&self) -> &str {
        self.code_object.name()
    }
}

impl Display for FunctionObject {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "<function '{}'>", self.name())
    }
}
