use std::{fmt::Display, path::Path, process};

use crate::{
    bytecode_vm::{compiler::types::CompiledProgram, types::Value, VmInterpreter},
    core::{Container, InterpreterEntrypoint},
    lexer::Lexer,
    parser::{types::ParseNode, Parser},
    treewalk::{
        interpreter::TreewalkDisruption,
        module_loader,
        types::{ExprResult, Module},
        Interpreter, ModuleSource, State,
    },
    types::errors::{MemphisError, ParserError},
};

pub struct MemphisContext {
    // TODO this shouldn't need to be public, we're using it in a few tests atm
    pub state: Container<State>,
    lexer: Option<Lexer>,
    interpreter: Option<Interpreter>,
    vm_interpreter: Option<VmInterpreter>,
}

impl Default for MemphisContext {
    fn default() -> Self {
        Self::from_module(ModuleSource::default())
    }
}

impl MemphisContext {
    pub fn from_path<P>(filepath: P) -> Self
    where
        P: AsRef<Path> + Display,
    {
        let module_source = module_loader::load_root_module_source(filepath.as_ref())
            .unwrap_or_else(|| {
                eprintln!("Error reading file: {}", filepath);
                process::exit(1);
            });
        Self::from_module(module_source)
    }

    pub fn from_text(text: &str) -> Self {
        let module_source = ModuleSource::from_text(text);
        Self::from_module(module_source)
    }

    pub fn from_module_with_state(module_source: ModuleSource, state: Container<State>) -> Self {
        state.push_module_source(module_source.clone());
        let mut context = Self::new(state);
        context.init_lexer(module_source.text());
        context
    }

    fn from_module(module_source: ModuleSource) -> Self {
        let state = State::from_source(module_source.clone());
        Self::from_module_with_state(module_source, state)
    }

    /// This is the base constructor but it isn't public because there are other entry points for
    /// that.
    fn new(state: Container<State>) -> Self {
        Self {
            state,
            lexer: None,
            interpreter: None,
            vm_interpreter: None,
        }
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
        match interpreter.evaluate_expr(&expr) {
            Ok(result) => Ok(result),
            Err(TreewalkDisruption::Error(e)) => Err(MemphisError::Execution(e)),
            Err(TreewalkDisruption::Signal(_)) => todo!(),
        }
    }

    /// Run the treewalk interpreter to completion.
    pub fn run_treewalk(&mut self) -> Result<ExprResult, MemphisError> {
        let mut parser = self.init_parser();
        let mut interpreter = self.init_interpreter();
        let result = interpreter.run(&mut parser);

        self.interpreter = Some(interpreter);
        result
    }

    pub fn compile(&mut self) -> Result<CompiledProgram, MemphisError> {
        let mut parser = self.init_parser();
        let mut vm_interpreter = self.init_vm_interpreter();
        vm_interpreter.compile(&mut parser)
    }

    pub fn run_vm(&mut self) -> Result<Value, MemphisError> {
        let mut parser = self.init_parser();
        let mut vm_interpreter = self.init_vm_interpreter();

        let result = vm_interpreter.run(&mut parser);

        self.vm_interpreter = Some(vm_interpreter);
        result
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

    pub fn init_parser(&self) -> Parser {
        Parser::new(self.ensure_lexer().tokens())
    }

    fn init_lexer(&mut self, text: &str) {
        if self.lexer.is_none() {
            let mut l = Lexer::new();
            // TODO we should really handle this error
            let _ = l.tokenize(text);
            self.lexer = Some(l);
        } else {
            panic!("Lexer has already been initialized!");
        }
    }

    fn init_interpreter(&self) -> Interpreter {
        let module_source = self.state.current_module_source();
        self.state.push_stack_frame(&*module_source);
        self.state
            .push_module(Container::new(Module::new(*module_source)));
        Interpreter::new(self.state.clone())
    }

    fn init_vm_interpreter(&self) -> VmInterpreter {
        VmInterpreter::new(self.state.clone())
    }
}
