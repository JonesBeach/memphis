use std::{path::PathBuf, process};

use crate::{
    bytecode_vm::{compiler::types::CompiledProgram, types::Value, VmInterpreter},
    core::{Container, InterpreterEntrypoint},
    lexer::Lexer,
    parser::{types::ParseNode, Parser},
    treewalk::{types::ExprResult, Interpreter, LoadedModule, StackFrame, State},
    types::errors::{MemphisError, ParserError},
};

pub struct MemphisContext {
    state: Container<State>,
    lexer: Option<Lexer>,
    interpreter: Option<Interpreter>,
    vm_interpreter: Option<VmInterpreter>,
}

impl Default for MemphisContext {
    fn default() -> Self {
        Self::with_state(None)
    }
}

impl MemphisContext {
    pub fn from_path_with_state(filename: &str, state: Option<Container<State>>) -> Self {
        let mut context = Self::with_state(state);
        let module = context
            .state
            .load_root(PathBuf::from(filename))
            .unwrap_or_else(|| {
                eprintln!("Error reading file: {}", filename);
                process::exit(1);
            });
        context.init_lexer(module.text());

        let stack_frame = StackFrame::new_root(module.path());
        context.state.push_context(stack_frame);
        context
    }

    pub fn from_text_with_state(text: &str, state: Option<Container<State>>) -> Self {
        let mut context = Self::with_state(state);
        context.init_lexer(text.to_string());

        let stack_frame = StackFrame::new_module(LoadedModule::new_virtual(text));
        context.state.push_context(stack_frame);
        context
    }

    pub fn from_module_with_state(module: LoadedModule, state: Option<Container<State>>) -> Self {
        let mut context = Self::with_state(state);
        context.init_lexer(module.text());

        let stack_frame = StackFrame::new_module(module);
        context.state.push_context(stack_frame);
        context
    }

    pub fn from_path(filename: &str) -> Self {
        Self::from_path_with_state(filename, None)
    }

    pub fn from_text(text: &str) -> Self {
        Self::from_text_with_state(text, None)
    }

    pub fn from_module(module: LoadedModule) -> Self {
        Self::from_module_with_state(module, None)
    }

    /// Parse a single [`ParseNode`]. This cannot be used for multiple parse calls.
    pub fn parse_oneshot<T>(&self) -> Result<T, ParserError>
    where
        T: ParseNode,
    {
        let parser = self.init_parser();
        T::parse_oneshot(parser)
    }

    /// Parse and evaluate a single expression using the treewalk interpreter.
    pub fn evaluate_oneshot(&self) -> Result<ExprResult, MemphisError> {
        let mut parser = self.init_parser();
        let interpreter = self.init_interpreter();
        let expr = parser.parse_expr().map_err(MemphisError::Parser)?;
        interpreter
            .evaluate_expr(&expr)
            .map_err(MemphisError::Interpreter)
    }

    /// Run the treewalk interpreter to completion and return a reference to the [`Interpreter`].
    pub fn run_and_return_interpreter(&mut self) -> Result<&Interpreter, MemphisError> {
        let _ = self.run_inner()?;
        Ok(self.ensure_treewalk())
    }

    /// Run the treewalk interpreter to completion.
    pub fn run(&mut self) -> Result<ExprResult, MemphisError> {
        self.run_inner()
    }

    pub fn compile(&mut self) -> Result<CompiledProgram, MemphisError> {
        let mut parser = self.init_parser();
        let mut vm_interpreter = Self::init_vm_interpreter();
        vm_interpreter.compile(&mut parser)
    }

    pub fn run_vm_and_return(&mut self) -> Result<&mut VmInterpreter, MemphisError> {
        let _ = self.run_vm_inner()?;
        Ok(self.ensure_vm())
    }

    pub fn run_vm(&mut self) -> Result<Value, MemphisError> {
        self.run_vm_inner()
    }

    pub fn ensure_treewalk(&self) -> &Interpreter {
        self.interpreter
            .as_ref()
            .expect("Failed to initialize Interpreter")
    }

    pub fn ensure_treewalk_mut(&mut self) -> &mut Interpreter {
        self.interpreter
            .as_mut()
            .expect("Failed to initialize Interpreter")
    }

    pub fn ensure_vm(&mut self) -> &mut VmInterpreter {
        self.vm_interpreter
            .as_mut()
            .expect("Failed to initialize VmInterpreter")
    }

    fn ensure_lexer(&self) -> &Lexer {
        self.lexer.as_ref().expect("Failed to initialize Lexer")
    }

    /// This is the base constructor but it isn't public because there are other entry points for
    /// that.
    fn with_state(state: Option<Container<State>>) -> Self {
        Self {
            state: state.unwrap_or_else(|| Container::new(State::default())),
            lexer: None,
            interpreter: None,
            vm_interpreter: None,
        }
    }

    pub fn init_parser(&self) -> Parser {
        Parser::new(self.ensure_lexer().tokens(), self.state.clone())
    }

    fn init_lexer(&mut self, text: String) {
        if self.lexer.is_none() {
            self.lexer = Some(Lexer::new(text));
        } else {
            panic!("Lexer has already been initialized!");
        }
    }

    fn init_interpreter(&self) -> Interpreter {
        Interpreter::new(self.state.clone())
    }

    fn init_vm_interpreter() -> VmInterpreter {
        VmInterpreter::new()
    }

    fn run_inner(&mut self) -> Result<ExprResult, MemphisError> {
        let mut parser = self.init_parser();
        let mut interpreter = self.init_interpreter();
        let result = interpreter.run(&mut parser);

        self.interpreter = Some(interpreter);
        result
    }

    fn run_vm_inner(&mut self) -> Result<Value, MemphisError> {
        let mut parser = self.init_parser();
        let mut vm_interpreter = Self::init_vm_interpreter();

        let result = vm_interpreter.run(&mut parser);

        self.vm_interpreter = Some(vm_interpreter);
        result
    }
}