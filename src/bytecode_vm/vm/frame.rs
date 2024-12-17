use std::collections::HashMap;

use crate::bytecode_vm::opcode::Opcode;

use super::types::{FunctionObject, Namespace, Reference};

#[derive(Debug)]
pub struct Frame {
    pub function: FunctionObject,

    /// The program counter indicating the current point of execution for the immutable block of
    /// bytecode held by this [`Frame`].
    pub pc: usize,

    /// Analogous to CPython's co_varnames, which is the names of the local variables beginning
    /// with the function arguments.
    pub varnames: Vec<String>,

    /// The stack which holds all the local variables themselves, beginning with the function
    /// arguments.
    pub locals: Vec<Reference>,
}

impl Frame {
    pub fn new(function: FunctionObject, args: Vec<Reference>) -> Self {
        let varnames = function.code_object.varnames.clone();
        Frame {
            function,
            pc: 0,
            varnames,
            locals: args,
        }
    }

    pub fn get_inst(&self) -> Opcode {
        self.function.code_object.bytecode[self.pc]
    }

    pub fn is_finished(&self) -> bool {
        self.pc == self.function.code_object.bytecode.len()
    }

    pub fn namespace(&self) -> Namespace {
        let mut namespace = HashMap::new();
        for (index, varname) in self.varnames.iter().enumerate() {
            namespace.insert(varname.to_owned(), self.locals[index]);
        }
        namespace
    }
}
