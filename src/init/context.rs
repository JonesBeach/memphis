use crate::{
    bytecode_vm::{VmInterpreter, VmValue},
    core::{Container, InterpreterEntrypoint},
    domain::Source,
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{Interpreter, TreewalkState, TreewalkValue},
    types::errors::MemphisError,
};

#[cfg(feature = "repl")]
use crate::{domain::MemphisValue, Engine};

pub struct MemphisContext {
    source: Source,
    state: Container<MemphisState>,
    lexer: Lexer,
    interpreter: Option<Interpreter>,
    vm_interpreter: Option<VmInterpreter>,
}

impl Default for MemphisContext {
    fn default() -> Self {
        Self::new(Source::default())
    }
}

impl MemphisContext {
    /// Given a [`Source`], setup a context which can be used for any execution engines.
    pub fn new(source: Source) -> Self {
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

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_treewalk(
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

    pub fn run_treewalk(&mut self) -> Result<TreewalkValue, MemphisError> {
        self.ensure_treewalk_initialized();

        // Destructure to break the borrow into disjoint pieces
        let MemphisContext {
            lexer, interpreter, ..
        } = self;

        let interpreter = interpreter
            .as_mut()
            .expect("Interpreter must be initialized before use");

        // We create a new Parser each time because our parser just wraps a streaming view over the
        // current lexer.
        let mut parser = Parser::new(lexer);
        interpreter.run(&mut parser)
    }

    pub fn run_vm(&mut self) -> Result<VmValue, MemphisError> {
        self.ensure_vm_initialized();

        // Destructure to break the borrow into disjoint pieces
        let MemphisContext {
            lexer,
            vm_interpreter,
            ..
        } = self;

        let interpreter = vm_interpreter
            .as_mut()
            .expect("Interpreter must be initialized before use");

        let mut parser = Parser::new(lexer);
        interpreter.run(&mut parser)
    }

    pub fn ensure_treewalk(&self) -> &Interpreter {
        self.interpreter
            .as_ref()
            .expect("Failed to initialize Interpreter")
    }

    pub fn ensure_vm(&mut self) -> &mut VmInterpreter {
        self.vm_interpreter
            .as_mut()
            .expect("Failed to initialize VmInterpreter")
    }

    fn ensure_treewalk_initialized(&mut self) {
        if self.interpreter.is_none() {
            let treewalk_state =
                TreewalkState::from_source_state(self.state.clone(), self.source.clone());
            self.interpreter = Some(Interpreter::new(treewalk_state));
        }
    }

    fn ensure_vm_initialized(&mut self) {
        if self.vm_interpreter.is_none() {
            self.vm_interpreter = Some(VmInterpreter::new(self.state.clone(), self.source.clone()))
        }
    }

    #[cfg(any(test, feature = "repl"))]
    pub fn add_line(&mut self, line: &str) {
        self.lexer
            .add_line(line)
            .expect("Failed to add line to lexer");
    }

    #[cfg(feature = "repl")]
    pub fn run(&mut self, engine: Engine) -> Result<MemphisValue, MemphisError> {
        match engine {
            Engine::Treewalk => Ok(self.run_treewalk()?.into()),
            Engine::BytecodeVm => Ok(self.run_vm()?.into()),
            #[cfg(feature = "llvm_backend")]
            _ => unimplemented!(),
        }
    }
}

#[cfg(test)]
use crate::{
    ast,
    bytecode_vm::compiler::CodeObject,
    parser::types::{Ast, ParseNode},
    types::errors::ParserError,
};

#[cfg(test)]
impl MemphisContext {
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

    fn init_parser(&mut self) -> Parser {
        Parser::new(&mut self.lexer)
    }

    pub fn compile(&mut self) -> Result<CodeObject, MemphisError> {
        self.ensure_vm_initialized();

        // Destructure to break the borrow into disjoint pieces
        let MemphisContext {
            lexer,
            vm_interpreter,
            ..
        } = self;

        let interpreter = vm_interpreter
            .as_mut()
            .expect("Interpreter must be initialized before use");

        let mut parser = Parser::new(lexer);
        interpreter.compile(&mut parser)
    }
}
