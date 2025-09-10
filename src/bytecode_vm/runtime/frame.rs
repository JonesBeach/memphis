use std::collections::HashMap;

use crate::{
    bytecode_vm::{compiler::Opcode, VmResult},
    core::Container,
    domain::{DebugStackFrame, ToDebugStackFrame},
};

use super::{
    types::{FunctionObject, Namespace, Reference},
    Method, Module, VirtualMachine,
};

#[derive(Clone, Debug)]
enum YieldFromState {
    /// No sub-generator is active
    None,
    /// A `Reference` to any sub-generator currently yielding values
    Delegating(Reference),
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub function: FunctionObject,

    pub module: Container<Module>,

    /// The program counter indicating the current point of execution for the immutable block of
    /// bytecode held by this [`Frame`].
    pub pc: usize,

    /// The stack which holds all the local variables themselves, beginning with the function
    /// arguments.
    pub stack: Vec<Reference>,

    /// The local variables themselves, beginning with the function arguments.
    pub locals: Vec<Reference>,

    yield_from: YieldFromState,
}

impl Frame {
    /// Create a new `Frame` for a top-level function call.
    /// This associates the frame with the correct module so it can read/write globals.
    pub fn from_function(
        vm: &mut VirtualMachine,
        function: FunctionObject,
        args: Vec<Reference>,
    ) -> VmResult<Self> {
        // We must associate this Frame with its Module in order to read and write global variables.
        let module_name = function.code_object.source.name();
        let module = vm.resolve_module(module_name)?;

        Ok(Frame::new(function, args, module))
    }

    pub fn from_method(
        vm: &mut VirtualMachine,
        method: Method,
        args: Vec<Reference>,
    ) -> VmResult<Self> {
        let mut bound_args = vec![method.receiver];
        bound_args.extend(args);

        Self::from_function(vm, method.function, bound_args)
    }

    fn new(function: FunctionObject, args: Vec<Reference>, module: Container<Module>) -> Self {
        Frame {
            function,
            pc: 0,
            stack: vec![],
            locals: args,
            module,
            yield_from: YieldFromState::None,
        }
    }

    pub fn current_inst(&self) -> Opcode {
        self.function.code_object.bytecode[self.pc]
    }

    pub fn current_inst_annotated(&self) -> String {
        let code = &self.function.code_object;
        let code_name = &code.name();
        let op = self.current_inst().display_annotated(code);
        format!("{code_name}: {op}")
    }

    pub fn is_finished(&self) -> bool {
        self.pc >= self.function.code_object.bytecode.len()
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

    pub fn has_subgenerator(&self) -> bool {
        matches!(self.yield_from, YieldFromState::Delegating(_))
    }

    pub fn set_subgenerator(&mut self, iterator_ref: Reference) {
        self.yield_from = YieldFromState::Delegating(iterator_ref);
    }

    pub fn clear_subgenerator(&mut self) {
        self.yield_from = YieldFromState::None;
    }

    pub fn subgenerator_ref(&self) -> Option<Reference> {
        match &self.yield_from {
            YieldFromState::Delegating(sub_iter_ref) => Some(*sub_iter_ref),
            _ => None,
        }
    }

    pub fn name(&self) -> String {
        let func_name = self.function.name();
        let arg_count = self.function.code_object.arg_count;

        // Turn the first few locals into strings for display.
        let args_preview: Vec<String> = self
            .locals
            .iter()
            .take(arg_count) // only the actual args, not all locals
            .map(|r| format!("{}", r))
            .collect();

        format!("{}({})", func_name, args_preview.join(", "))
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
