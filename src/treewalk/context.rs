use crate::{
    core::{Container, Interpreter},
    domain::{MemphisValue, ModuleName, Source},
    errors::MemphisResult,
    lexer::Lexer,
    parser::Parser,
    runtime::MemphisState,
    treewalk::{types::Module, TreewalkInterpreter, TreewalkState, TreewalkValue},
};

pub struct TreewalkContext {
    lexer: Lexer,
    interpreter: TreewalkInterpreter,
}

impl TreewalkContext {
    pub fn new(source: Source) -> Self {
        let lexer = Lexer::new(&source);
        let state = MemphisState::from_source(&source);
        let treewalk_state = Container::new(TreewalkState::new(state));
        let module = Module::new(ModuleName::main(), source);
        treewalk_state.enter_module(module);
        let interpreter = TreewalkInterpreter::new(treewalk_state);

        Self { lexer, interpreter }
    }

    /// Initialize a context from a [`Source`] and existing treewalk state.
    pub fn from_state(source: Source, treewalk_state: Container<TreewalkState>) -> Self {
        let lexer = Lexer::new(&source);

        Self {
            lexer,
            interpreter: TreewalkInterpreter::new(treewalk_state),
        }
    }

    pub fn run_inner(&mut self) -> MemphisResult<TreewalkValue> {
        // Destructure to break the borrow into disjoint pieces
        let TreewalkContext {
            lexer, interpreter, ..
        } = self;

        let mut parser = Parser::new(lexer);
        interpreter.execute(&mut parser)
    }

    pub fn read_inner(&self, name: &str) -> Option<TreewalkValue> {
        self.interpreter.load_var(name).ok()
    }

    pub fn add_line_inner(&mut self, line: &str) {
        self.lexer.add_line(line);
    }

    #[cfg(test)]
    pub fn interpreter(&self) -> &TreewalkInterpreter {
        &self.interpreter
    }
}

impl Interpreter for TreewalkContext {
    fn run(&mut self) -> MemphisResult<MemphisValue> {
        self.run_inner().map(Into::into)
    }

    fn read(&mut self, name: &str) -> Option<MemphisValue> {
        self.read_inner(name).map(Into::into)
    }

    fn add_line(&mut self, line: &str) {
        self.add_line_inner(line);
    }
}
