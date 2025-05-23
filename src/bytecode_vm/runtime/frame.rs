use std::collections::HashMap;

use crate::{
    bytecode_vm::compiler::Opcode,
    core::Container,
    domain::{DebugStackFrame, ToDebugStackFrame},
};

use super::{
    types::{FunctionObject, Namespace, Reference},
    Module,
};

#[derive(Debug)]
pub struct Frame {
    pub function: FunctionObject,

    pub module: Container<Module>,

    /// The program counter indicating the current point of execution for the immutable block of
    /// bytecode held by this [`Frame`].
    pub pc: usize,

    /// The stack which holds all the local variables themselves, beginning with the function
    /// arguments.
    pub locals: Vec<Reference>,

    /// This will be set by any return instructions.
    pub return_val: Option<Reference>,

    pub finished: bool,
}

impl Frame {
    pub fn new(function: FunctionObject, args: Vec<Reference>, module: Container<Module>) -> Self {
        Frame {
            function,
            pc: 0,
            locals: args,
            module,
            return_val: None,
            finished: false,
        }
    }

    pub fn get_inst(&self) -> Opcode {
        self.function.code_object.bytecode[self.pc]
    }

    pub fn is_finished(&self) -> bool {
        self.finished || self.pc >= self.function.code_object.bytecode.len()
    }

    pub fn namespace(&self) -> Namespace {
        let varnames = &self.function.code_object.varnames;
        let mut namespace = HashMap::new();
        for (index, varname) in varnames.iter().enumerate() {
            namespace.insert(varname.to_owned(), self.locals[index]);
        }
        namespace
    }

    pub fn current_line(&self) -> usize {
        self.function.code_object.get_line_number(self.pc)
    }
}

impl ToDebugStackFrame for Frame {
    fn to_stack_frame(&self) -> DebugStackFrame {
        DebugStackFrame::new(
            self.function.code_object.context(),
            self.function.code_object.path().to_path_buf(),
            self.current_line(),
        )
    }
}
