use std::{fmt::Display, path::Path, process};

use crate::{
    ast,
    bytecode_vm::{compiler::types::CompiledProgram, types::Value, VmInterpreter},
    core::{Container, InterpreterEntrypoint},
    lexer::Lexer,
    parser::{
        types::{Ast, ParseNode},
        Parser,
    },
    treewalk::{
        module_loader,
        types::{ExprResult, Module},
        Interpreter, ModuleSource, State,
    },
    types::errors::{MemphisError, ParserError},
};

pub struct MemphisContext {
    // TODO this shouldn't need to be public, we're using it in a few tests atm
    pub state: Container<State>,
    lexer: Lexer,
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
        let mut lexer = Lexer::default();
        // empty ModuleSource can occur in REPL mode
        if module_source.has_text() {
            lexer
                .add_line(module_source.text())
                .expect("Failed to add line to lexer");
        }
        Self::new(state, lexer)
    }

    fn from_module(module_source: ModuleSource) -> Self {
        let state = State::from_source(module_source.clone());
        Self::from_module_with_state(module_source, state)
    }

    /// This is the base constructor but it isn't public because there are other entry points for
    /// that.
    fn new(state: Container<State>, lexer: Lexer) -> Self {
        Self {
            state,
            lexer,
            interpreter: None,
            vm_interpreter: None,
        }
    }

    /// Parse a single [`ParseNode`]. This cannot be used for multiple parse calls.
    pub fn parse_oneshot<T>(&mut self) -> Result<T, ParserError>
    where
        T: ParseNode,
    {
        let parser = self.init_parser();
        T::parse_oneshot(parser)
    }

    pub fn parse_all_statements(&mut self) -> Result<Ast, ParserError> {
        let mut parser = self.init_parser();
        let mut statements = ast![];

        while !parser.is_finished() {
            statements.push(parser.parse_statement()?);
        }

        Ok(statements)
    }

    pub fn add_line(&mut self, line: &str) {
        self.lexer
            .add_line(line)
            .expect("Failed to add line to lexer");
    }

    pub fn evaluate(&mut self) -> Result<ExprResult, MemphisError> {
        self.ensure_treewalk_initialized();

        // Destructure to break the borrow into disjoint pieces
        let MemphisContext {
            lexer, interpreter, ..
        } = self;

        let interpreter = interpreter
            .as_mut()
            .expect("Interpreter must be initialized before calling evaluate");

        // We create a new Parser each time because our parser just wraps a streaming view over the
        // current lexer.
        let mut parser = Parser::new(lexer);
        interpreter.run(&mut parser)
    }

    pub fn compile(&mut self) -> Result<CompiledProgram, MemphisError> {
        let mut vm_interpreter = self.init_vm_interpreter();
        let mut parser = self.init_parser();
        vm_interpreter.compile(&mut parser)
    }

    pub fn run_vm(&mut self) -> Result<Value, MemphisError> {
        let mut vm_interpreter = self.init_vm_interpreter();
        let mut parser = self.init_parser();

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

    fn init_parser(&mut self) -> Parser {
        Parser::new(&mut self.lexer)
    }

    fn ensure_treewalk_initialized(&mut self) {
        if self.interpreter.is_none() {
            let module_source = self.state.current_module_source();
            self.state.push_stack_frame(&*module_source);
            self.state
                .push_module(Container::new(Module::new(*module_source)));
            let interpreter = Interpreter::new(self.state.clone());
            self.interpreter = Some(interpreter);
        }
    }

    fn init_vm_interpreter(&self) -> VmInterpreter {
        VmInterpreter::new(self.state.clone())
    }
}
