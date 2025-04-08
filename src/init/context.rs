use std::{fmt::Display, path::Path, process};

use crate::{
    ast,
    bytecode_vm::{compiler::types::CompiledProgram, VmInterpreter, VmValue},
    core::{Container, InterpreterEntrypoint},
    domain::Source,
    lexer::Lexer,
    parser::{
        types::{Ast, ParseNode},
        Parser,
    },
    runtime::MemphisState,
    treewalk::{module_loader, Interpreter, TreewalkState, TreewalkValue},
    types::errors::{MemphisError, ParserError},
};

pub struct MemphisContext {
    source: Source,
    state: Container<MemphisState>,
    lexer: Lexer,
    interpreter: Option<Interpreter>,
    vm_interpreter: Option<VmInterpreter>,
}

impl Default for MemphisContext {
    fn default() -> Self {
        Self::from_module(Source::default())
    }
}

impl MemphisContext {
    pub fn from_path<P>(filepath: P) -> Self
    where
        P: AsRef<Path> + Display,
    {
        let source = module_loader::load_root_source(filepath.as_ref()).unwrap_or_else(|| {
            eprintln!("Error reading file: {}", filepath);
            process::exit(1);
        });
        Self::from_module(source)
    }

    pub fn from_text(text: &str) -> Self {
        let source = Source::from_text(text);
        Self::from_module(source)
    }

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_module_from_treewalk(
        source: Source,
        memphis_state: Container<MemphisState>,
        treewalk_state: Container<TreewalkState>,
    ) -> Self {
        let lexer = Self::init_lexer(&source);

        Self {
            source,
            state: memphis_state,
            lexer,
            interpreter: Some(Interpreter::new(treewalk_state)),
            vm_interpreter: None,
        }
    }

    /// Given a [`Source`], setup a context which can be used for any execution engines.
    fn from_module(source: Source) -> Self {
        let lexer = Self::init_lexer(&source);
        let state = MemphisState::from_source(&source);

        Self {
            source,
            state,
            lexer,
            interpreter: None,
            vm_interpreter: None,
        }
    }

    fn init_lexer(source: &Source) -> Lexer {
        let mut lexer = Lexer::default();

        // empty Source can occur in REPL mode
        if source.has_text() {
            lexer
                .add_line(source.text())
                .expect("Failed to add line to lexer");
        }

        lexer
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

    pub fn evaluate(&mut self) -> Result<TreewalkValue, MemphisError> {
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

    pub fn run_vm(&mut self) -> Result<VmValue, MemphisError> {
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
            let treewalk_state =
                TreewalkState::from_source_state(self.state.clone(), self.source.clone());
            let interpreter = Interpreter::new(treewalk_state);
            self.interpreter = Some(interpreter);
        }
    }

    fn init_vm_interpreter(&self) -> VmInterpreter {
        VmInterpreter::new(self.state.clone(), self.source.clone())
    }
}
